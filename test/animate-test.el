;;; animate-test.el --- short animated images play inline -*- lexical-binding: t; -*-

;; Animated images (GIF, animated WebP) only play when short: the
;; event's declared mimetype and size gate the whole-file fetch (any
;; thumbnail is a still frame), the decoded frame count and loop length
;; gate the playing.  The driver ticks through frames while the room is
;; visible, polls while it is not, and stops for good when the image's
;; text is gone.  Batch has no image display, so the display-side
;; pieces run against hand-built specs.
;;
;; Run: emacs -Q --batch -L lisp -l test/animate-test.el

(require 'elneo-room)

(let ((pass 0) (fail 0))
  (defun mx-check (label got expected)
    (if (equal got expected)
        (progn (cl-incf pass) (message "ok: %s" label))
      (cl-incf fail)
      (message "FAIL: %s — got %S, expected %S" label got expected)))

  (defun mx-tick-timers ()
    (seq-filter (lambda (tm) (eq (timer--function tm) #'elneo-room--animate-tick))
                timer-list))

  (let ((gif '((mimetype . "image/gif") (size . 500000)
               (source . ((url . "mxc://hs/full")))
               (thumb_source . ((url . "mxc://hs/thumb"))))))
    ;; --- the declared-metadata gate
    (mx-check "candidate: a small gif" (and (elneo-room--animate-candidate-p gif) t) t)
    (mx-check "candidate: a gif without a declared size"
              (elneo-room--animate-candidate-p (assq-delete-all 'size (copy-alist gif)))
              nil)
    (mx-check "candidate: a gif over the size cap"
              (let ((elneo-room-animate-max-size 1000))
                (elneo-room--animate-candidate-p gif))
              nil)
    (mx-check "candidate: a png is never one"
              (elneo-room--animate-candidate-p
               (cons '(mimetype . "image/png") gif))
              nil)
    (mx-check "candidate: off when animation is disabled"
              (let ((elneo-room-animate-images nil))
                (elneo-room--animate-candidate-p gif))
              nil)

    ;; --- the fetch: a short animation comes whole, never as a thumb
    (mx-check "fetch: a small gif fetches the full file"
              (elneo-room--media-fetch-args gif)
              (cons (alist-get 'source gif) "file"))
    (mx-check "fetch: a big gif keeps the sender's thumbnail"
              (let ((elneo-room-animate-max-size 1000))
                (elneo-room--media-fetch-args gif))
              (cons (alist-get 'thumb_source gif) "thumb")))

  ;; --- the decoded gate: frames and loop length
  (mx-check "short: 4 frames at 0.1 s" (and (elneo-room--animate-short-p '(4 . 0.1)) t) t)
  (mx-check "short: too many frames"
            (elneo-room--animate-short-p
             (cons (1+ elneo-room-animate-max-frames) 0.01))
            nil)
  (mx-check "short: a loop over the length cap"
            (elneo-room--animate-short-p '(100 . 0.5)) nil)
  (mx-check "short: a nil delay falls back to the default delay"
            (let ((image-default-frame-delay 1)
                  (elneo-room-animate-max-seconds 5))
              (elneo-room--animate-short-p '(10 . nil)))
            nil)
  (mx-check "short: a still image is not an animation"
            (elneo-room--animate-short-p nil) nil)

  ;; --- the driver.  insert-image displays a fresh cons sharing the
  ;; image's plist; the specs here are built the same way.
  (let* ((image (list 'image :type 'gif :file "/nonexistent.gif"))
         (buf (generate-new-buffer " *mx-animate*")))
    (with-current-buffer buf
      (insert "above\n")
      (insert (propertize "[image]" 'display (cons 'image (cdr image)))))
    ;; Shown nowhere: the tick only polls, no frame change.
    (let ((marker (with-current-buffer buf (copy-marker 7))))
      (elneo-room--animate-tick image marker 4 2)
      (mx-check "tick: a hidden room polls instead of advancing"
                (length (mx-tick-timers)) 1)
      (mx-check "tick: a hidden room keeps the frame"
                (plist-get (cdr image) :index) nil)
      (mapc #'cancel-timer (mx-tick-timers))
      ;; Text above shifts the image; the marker follows, so the
      ;; driver still finds it.
      (with-current-buffer buf
        (goto-char (point-min))
        (insert "backfill\n"))
      (mx-check "tick: the marker follows text inserted above"
                (eq (cdr (get-text-property marker 'display buf)) (cdr image))
                t)
      ;; Re-rendered (the image's text replaced): the driver stops
      ;; and releases its marker.
      (with-current-buffer buf
        (let ((pos (marker-position marker)))
          (delete-region pos (+ pos 7))
          (goto-char pos)
          (insert "[image]")))
      (elneo-room--animate-tick image marker 4 2)
      (mx-check "tick: a re-rendered image stops the driver"
                (length (mx-tick-timers)) 0)
      (mx-check "tick: a stopped driver releases its marker"
                (marker-buffer marker) nil))
    ;; A killed room stops it too.
    (let ((marker (with-current-buffer buf (copy-marker 1))))
      (kill-buffer buf)
      (elneo-room--animate-tick image marker 4 0)
      (mx-check "tick: a killed buffer stops the driver"
                (length (mx-tick-timers)) 0)))

  (message "results: %d passed, %d failed" pass fail)
  (kill-emacs (if (zerop fail) 0 1)))
