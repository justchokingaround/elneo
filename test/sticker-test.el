;;; sticker-test.el --- sticker packs and m.sticker sends -*- lexical-binding: t; -*-

;; The client side of stickers (protocol 27): `elneo-send-sticker'
;; builds the sendSticker command (info only when the pack has one),
;; `elneo-sticker-send' carries the room's reply and thread state and
;; leaves reply mode, the picker renders one heading per pack with a
;; cell per sticker, and `/' narrows by shortcode, body or pack name.
;;
;; Run: emacs -Q --batch -L lisp -l test/sticker-test.el

(require 'elneo-sticker)

(let ((pass 0) (fail 0)
      (sent nil))
  (defun mx-check (label got expected)
    (if (equal got expected)
        (progn (cl-incf pass) (message "ok: %s" label))
      (cl-incf fail)
      (message "FAIL: %s — got %S, expected %S" label got expected)))

  (cl-letf (((symbol-function 'elneo-server--call)
             (lambda (_profile command _callback) (push command sent) 1))
            ((symbol-function 'elneo-server--daemon-protocol)
             (lambda (_profile) 27)))

    ;; --- the command ---

    (elneo-send-sticker "p" "!r" '((shortcode . "cat") (url . "mxc://hs/cat")
                                   (body . "a cat") (info (w . 256)))
                        #'ignore)
    (let ((cmd (pop sent)))
      (mx-check "command: type" (alist-get '@type cmd) "sendSticker")
      (mx-check "command: url" (alist-get 'url cmd) "mxc://hs/cat")
      (mx-check "command: body" (alist-get 'body cmd) "a cat")
      (mx-check "command: info rides along" (alist-get 'info cmd) '((w . 256)))
      (mx-check "command: no relation" (assq 'reply_to cmd) nil))
    (elneo-send-sticker "p" "!r" '((shortcode . "dog") (url . "mxc://hs/dog"))
                        #'ignore nil "$root")
    (let ((cmd (pop sent)))
      (mx-check "command: body falls back to the shortcode" (alist-get 'body cmd) "dog")
      (mx-check "command: no info key without one" (assq 'info cmd) nil)
      (mx-check "command: thread root" (alist-get 'thread_root cmd) "$root"))

    ;; --- sending from a room in reply mode ---

    (let ((room (generate-new-buffer " *mx-sticker-room*")))
      (with-current-buffer room
        (elneo-room-mode)
        (setq-local elneo-room--profile "p")
        (setq-local elneo-room--room-id "!r")
        (setq elneo-room--seen (make-hash-table :test 'equal))
        (puthash "$in-thread" '(:thread-root "$root") elneo-room--seen)
        (setq elneo-room--reply-to "$in-thread"))
      (cl-letf (((symbol-function 'elneo-room--update-separator) #'ignore))
        (elneo-sticker-send room '((shortcode . "cat") (url . "mxc://hs/cat"))))
      (let ((cmd (pop sent)))
        (mx-check "reply: reply_to" (alist-get 'reply_to cmd) "$in-thread")
        (mx-check "reply: the target's thread" (alist-get 'thread_root cmd) "$root"))
      (mx-check "reply: reply mode ends"
                (buffer-local-value 'elneo-room--reply-to room) nil)

      ;; --- the picker ---

      (let ((picker (generate-new-buffer " *mx-sticker-picker*")))
        (with-current-buffer picker
          (elneo-sticker-mode)
          (setq elneo-sticker--room room
                elneo-sticker--packs
                (list '((id . "!s/cats") (source . "space") (name . "Cats")
                        (room_name . "Fun")
                        (stickers . [((shortcode . "cat") (url . "mxc://hs/cat") (body . "cat"))
                                     ((shortcode . "kitten") (url . "mxc://hs/kit") (body . "small cat"))]))
                      '((id . "user") (source . "user") (name . "Personal")
                        (stickers . [((shortcode . "dog") (url . "mxc://hs/dog") (body . "dog"))]))))
          (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
            (elneo-sticker--render)
            (mx-check "picker: pack headings and sources"
                      (and (string-search "Cats  space Fun" (buffer-string))
                           (string-search "Personal  personal" (buffer-string))
                           t)
                      t)
            (mx-check "picker: point starts on the first sticker"
                      (alist-get 'shortcode (elneo-sticker--at-point)) "cat")
            (elneo-sticker-next)
            (mx-check "picker: n moves on" (alist-get 'shortcode (elneo-sticker--at-point)) "kitten")
            (elneo-sticker-next-pack)
            (mx-check "picker: TAB jumps to the next pack"
                      (alist-get 'shortcode (elneo-sticker--at-point)) "dog")
            (elneo-sticker-filter "small")
            (mx-check "picker: / narrows by body"
                      (list (string-search "[kitten]" (buffer-string))
                            (string-search "[cat]" (buffer-string))
                            (string-search "Personal" (buffer-string)))
                      (list (string-search "[kitten]" (buffer-string)) nil nil))
            (elneo-sticker-filter "personal")
            (mx-check "picker: / matches the pack name"
                      (alist-get 'shortcode (elneo-sticker--at-point)) "dog")
            (elneo-sticker-filter "nothing-like-this")
            (mx-check "picker: no match says so"
                      (and (string-search "No sticker matches" (buffer-string)) t) t)
            (elneo-sticker-filter "")
            (mx-check "picker: empty filter shows all"
                      (and (string-search "[dog]" (buffer-string))
                           (string-search "[cat]" (buffer-string)) t)
                      t)
            (goto-char (point-min))
            (elneo-sticker-next)
            (cl-letf (((symbol-function 'quit-window) #'ignore)
                      ((symbol-function 'elneo-room--update-separator) #'ignore))
              (elneo-sticker-send-at-point))
            (mx-check "picker: RET sends the sticker at point"
                      (alist-get 'url (pop sent)) "mxc://hs/cat")))
        (kill-buffer picker))
      (kill-buffer room)))

  (message "%d checks: %d ok, %d FAILED" (+ pass fail) pass fail)
  (when (> fail 0) (kill-emacs 1)))
