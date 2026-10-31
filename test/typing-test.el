;;; typing-test.el --- typing line above the draft separator -*- lexical-binding: t; -*-

(require 'elneo-room)

(defvar mx-failures 0)
(defun mx-check (label got want)
  (if (equal got want)
      (message "ok   %s" label)
    (setq mx-failures (1+ mx-failures))
    (message "FAIL %s\n  got:  %S\n  want: %S" label got want)))

(with-temp-buffer
  (insert "history line\n")
  (setq-local elneo-room--backfilled t)
  (elneo-room--reset-input-area "")
  (let ((before (buffer-string)))
    (setq elneo-room--typing '("@alice:hs"))
    (elneo-room--typing-redisplay)
    (mx-check "buffer text untouched" (buffer-string) before)
    (mx-check "separator keeps its own text"
              (and (string-match-p "dired ──\n" (buffer-string)) t) t)
    (let ((ov elneo-room--typing-overlay))
      (mx-check "typing line sits at the separator's start"
                (save-excursion (goto-char (overlay-start ov))
                                (looking-at-p "── draft"))
                t)
      (mx-check "own line" (substring-no-properties (overlay-get ov 'before-string))
                "alice is typing…\n"))
    (elneo-room--update-separator)
    (mx-check "survives a separator rewrite"
              (and (overlay-buffer elneo-room--typing-overlay) t) t)
    (setq elneo-room--typing nil)
    (elneo-room--typing-redisplay)
    (mx-check "nobody typing: gone"
              (overlay-buffer elneo-room--typing-overlay) nil)))

(kill-emacs (if (> mx-failures 0) 1 0))
