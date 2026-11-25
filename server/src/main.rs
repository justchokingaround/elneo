//! elneo-server: Matrix client daemon for Emacs.
//!
//! We run one daemon process per profile, communicating with Emacs over framed
//! JSON on stdio (see [`protocol`]); rooms and E2EE state persist under a per-
//! profile data directory.
//!
//! # System layout
//!
//! Emacs and the daemon are separate processes with deliberately separate
//! responsibilities.  On the Emacs side only UI lives:
//! `elneo-server.el` is transport only (frame writes/reads and `@req`
//! correlation), `elneo-client.el` keeps the account/room registry and
//! fans every daemon event into hooks the UI layers subscribe to
//! (`elneo-room.el` room buffers, `elneo-room-bar.el` the fixed
//! side-window bar, alerting).  Everything Matrix lives here: the client
//! lifecycle, homeserver connection and continuous sync, E2EE, and the
//! encrypted persistent stores (one data directory per profile — the
//! `matrix-rust-sdk` sqlite state and crypto stores plus `session.json`;
//! the Emacs side keeps only non-secrets).
//!
//! The modules of this crate:
//!
//! - [`daemon`]: core — client lifecycle, the framed stdio run loop
//!   (a single writer thread owns stdout; stdin is read on a blocking
//!   thread and commands dispatch sequentially), and the command handlers
//!   not owning their own module.  The `daemonInfo` greeting is queued
//!   in `main` before anything can emit, so it is always frame one.
//! - [`protocol`]: the wire contract — command/event enums, framing, and
//!   the `PROTOCOL_VERSION` gate Emacs activates sessions on.
//! - [`sync`]: the continuous sync loop and the per-event derivations
//!   (room summaries, edits/reactions/receipts merging).
//! - [`media`]: an on-disk media cache in front of the SDK's media
//!   fetches, running downloads as spawned tasks so they cannot stall
//!   command dispatch.
//! - [`verification`]: interactive SAS device verification and key
//!   recovery/SSSS flows.
//!
//! # Testing the daemon by hand
//!
//! ```bash
//! cargo run -- -p test -v 4
//! # then feed a frame on stdin, e.g.:
//! # send 28
//! # {"@type":"getSession","@req":1}
//! ```
//!
//! The daemon's first outbound frame is its `daemonInfo` greeting;
//! `elneo-server --version` prints the release version.  Tracing output
//! goes to stderr unless a `-l` log file is given: the framed protocol
//! owns stdout, so it must stay clean (the Emacs side normally passes
//! `-l` explicitly).

mod daemon;
mod media;
mod protocol;
mod sync;
mod verification;

use anyhow::Result;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "elneo-server")]
#[command(about = "Matrix client daemon for Emacs", long_about = None)]
#[command(version)]
struct Args {
    /// Profile name for this session (allows multiple concurrent sessions)
    #[arg(short, long, default_value = "default")]
    profile: String,

    /// Verbosity level (0-5)
    #[arg(short, long, default_value = "2")]
    verbosity: u8,

    /// Log file path
    #[arg(short, long)]
    logfile: Option<std::path::PathBuf>,
}

/// A log file is renamed to this suffix once it grows past
/// [`LOG_ROTATE_BYTES`], replacing the previous rotation.
const LOG_ROTATE_SUFFIX: &str = "1";
const LOG_ROTATE_BYTES: u64 = 50 * 1024 * 1024;

/// The subscriber's directives for a verbosity level.
///
/// `h2`/`hyper`'s frame logs are most of what a DEBUG daemon writes (40% of
/// one 1.8 GB log) and tell nobody anything; `matrix_sdk` logs per event, so
/// it sits at INFO until `-v 4` asks for the SDK's own trace.
fn directives(verbosity: u8) -> String {
    let level = match verbosity {
        0 => "error",
        1 => "warn",
        2 => "info",
        3 => "debug",
        _ => "trace",
    };
    let sdk = if verbosity >= 4 { "" } else { ",matrix_sdk=info" };
    format!("{level},h2=warn,hyper=warn,hyper_util=warn{sdk}")
}

/// Open the log for appending, first rotating it aside when it is over
/// LIMIT bytes (`LOG_ROTATE_BYTES`), so a long-lived daemon cannot fill the
/// disk.
fn open_log(logfile: &std::path::Path, limit: u64) -> Result<std::fs::File> {
    if let Ok(meta) = std::fs::metadata(logfile) {
        if meta.len() > limit {
            let rotated = logfile.with_file_name(format!(
                "{}.{LOG_ROTATE_SUFFIX}",
                logfile
                    .file_name()
                    .map(|n| n.to_string_lossy().into_owned())
                    .unwrap_or_else(|| "elneo-server.log".into())
            ));
            let _ = std::fs::remove_file(&rotated);
            // A rotation that fails is not worth refusing to start over:
            // say so on stderr and append to the old file.
            if let Err(e) = std::fs::rename(logfile, &rotated) {
                eprintln!(
                    "elneo-server: cannot rotate the log to {}: {e}",
                    rotated.display()
                );
            }
        }
    }
    Ok(std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(logfile)?)
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    // Setup logging.  The framed protocol uses stdout, so logs go to stderr
    // unless a log file is given (Emacs merges stderr into the process
    // buffer, so it should normally pass -l explicitly).  The file rotates
    // at 50 MB: an appended-forever log reached 1.8 GB in a month.
    let filter = tracing_subscriber::EnvFilter::new(directives(args.verbosity));
    if let Some(logfile) = &args.logfile {
        let writer = std::sync::Mutex::new(open_log(logfile, LOG_ROTATE_BYTES)?);
        tracing_subscriber::fmt()
            .with_env_filter(filter)
            .with_writer(writer)
            .init();
    } else {
        tracing_subscriber::fmt()
            .with_env_filter(filter)
            .with_writer(std::io::stderr)
            .init();
    }

    let (ev_tx, ev_rx) = tokio::sync::mpsc::unbounded_channel();
    // Greeting first: queue it before spawning the writer (and before
    // anything else can emit, including restore_session inside
    // ElneoDaemon::new), so it is frame 1 of the connection.
    let greeting = protocol::OutEvent::event(protocol::Event::DaemonInfo {
        protocol: protocol::PROTOCOL_VERSION,
        release: protocol::RELEASE_VERSION.into(),
    });
    if let Err(err) = ev_tx.send(greeting) {
        anyhow::bail!("event channel closed before greeting: {err}");
    }
    tracing::info!(
        "elneo-server {} speaking protocol {}",
        protocol::RELEASE_VERSION,
        protocol::PROTOCOL_VERSION
    );
    daemon::spawn_writer(ev_rx);

    let daemon = daemon::ElneoDaemon::new(args.profile, ev_tx)?;
    daemon.run().await
}

#[cfg(test)]
mod tests {
    use super::{directives, open_log};

    #[test]
    fn h2_and_hyper_stay_quiet_whatever_the_verbosity() {
        for verbosity in 0..=5 {
            let filter = directives(verbosity);
            assert!(filter.contains("h2=warn"), "{filter}");
            assert!(filter.contains("hyper=warn"), "{filter}");
            assert!(filter.contains("hyper_util=warn"), "{filter}");
        }
        // The SDK's own trace is only silenced below -v 4.
        assert!(directives(3).contains("matrix_sdk=info"));
        assert!(!directives(4).contains("matrix_sdk=info"));
        // The asked-for level is still the default one.
        assert!(directives(2).starts_with("info,"));
        assert!(directives(3).starts_with("debug,"));
    }

    #[test]
    fn a_big_log_is_rotated_once_and_appended_after() {
        let dir = tempfile::tempdir().unwrap();
        let log = dir.path().join("elneo-server-test.log");
        std::fs::write(&log, vec![b'x'; 40]).unwrap();

        let mut file = open_log(&log, 10).unwrap();
        assert_eq!(std::fs::metadata(&log).unwrap().len(), 0);
        let rotated = dir.path().join("elneo-server-test.log.1");
        assert_eq!(std::fs::metadata(&rotated).unwrap().len(), 40);

        use std::io::Write;
        writeln!(file, "new").unwrap();
        // A second open under the limit appends to the fresh file.
        drop(file);
        open_log(&log, 10).unwrap();
        assert_eq!(std::fs::metadata(&log).unwrap().len(), 4);
        assert_eq!(std::fs::metadata(&rotated).unwrap().len(), 40);
    }
}
