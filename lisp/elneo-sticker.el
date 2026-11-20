;;; elneo-sticker.el --- send stickers from MSC2545 packs -*- lexical-binding: t; -*-

;;; Commentary:

;; Matrix has no sticker store; the packs other clients (Cinny,
;; FluffyChat, Nheko, Element with its image-pack labs flag) share are
;; MSC2545 image packs:
;;
;; - a room's packs are `im.ponies.room_emotes' state events, one per
;;   state key.  A space is a room, so a space's packs are its state
;;   too, and clients offer them in every room of the space;
;; - the personal pack is the `im.ponies.user_emotes' account data;
;; - `im.ponies.emote_rooms' account data enables room packs
;;   everywhere (the "use this pack globally" toggle of those clients).
;;
;; The daemon collects all four for a room (`getStickerPacks', protocol
;; 27) and keeps the images whose usage includes "sticker"; a pick goes
;; out as an `m.sticker' event (`sendSticker') — the same event Element
;; sends, which every client renders and elneo already shows inline.
;;
;; `elneo-sticker-pick' (C-c C-s in a room) opens the picker below
;; the room: one heading per pack, its stickers as a grid of server
;; thumbnails.  RET sends the sticker at point (a reply while the
;; room is replying, a thread reply in a thread view) and closes the
;; picker; `/' narrows by shortcode, body or pack name.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'text-property-search)
(require 'elneo-client)
(require 'elneo-room)

(defgroup elneo-sticker nil
  "Sending stickers from MSC2545 image packs."
  :group 'elneo)

(defcustom elneo-sticker-size 96
  "Edge, in pixels, of a sticker cell in the picker."
  :group 'elneo-sticker
  :type 'natnum)

(defcustom elneo-sticker-quit-after-send t
  "When non-nil, sending a sticker closes the picker."
  :group 'elneo-sticker
  :type 'boolean)

(defface elneo-sticker-pack-heading
  '((t :inherit bold :height 1.1))
  "Face of a pack's name in the sticker picker.")

(defface elneo-sticker-pack-source
  '((t :inherit shadow))
  "Face of a pack's origin (room, space, personal) in the picker.")

(defconst elneo-sticker--protocol 27
  "Daemon protocol version that knows the sticker commands.")

(defvar-local elneo-sticker--room nil
  "The room buffer this picker sends to.")

(defvar-local elneo-sticker--packs nil
  "The packs on display: the `stickerPacks' response's `packs' list.")

(defvar-local elneo-sticker--filter nil
  "The `/' narrowing string, or nil.")

(defvar elneo-sticker-cell-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'elneo-sticker-mouse-send)
    map)
  "Keymap on a sticker cell.")

;;;; Data

(defun elneo-sticker--supported-p (profile)
  "Non-nil when PROFILE's daemon speaks the sticker commands."
  (>= (or (elneo-server--daemon-protocol profile) 0) elneo-sticker--protocol))

(defun elneo-sticker--source-label (pack)
  "Where PACK comes from, as shown after its name."
  (let ((room (or (alist-get 'room_name pack) (alist-get 'room_id pack))))
    (pcase (alist-get 'source pack)
      ("room" "this room")
      ("space" (format "space %s" room))
      ("user" "personal")
      ("global" (format "global, from %s" room))
      (other other))))

(defun elneo-sticker--matches-p (filter pack sticker)
  "Non-nil when STICKER of PACK matches FILTER (words, all must hit)."
  (or (null filter)
      (let ((hay (downcase (mapconcat (lambda (s) (or s ""))
                                      (list (alist-get 'shortcode sticker)
                                            (alist-get 'body sticker)
                                            (alist-get 'name pack))
                                      " "))))
        (cl-every (lambda (word) (string-search word hay))
                  (split-string (downcase filter) nil t)))))

(defun elneo-sticker-send (room sticker)
  "Send STICKER (a `stickerPacks' sticker alist) to the ROOM buffer.
A room in reply mode sends it as that reply (and leaves reply
mode); a thread view, or a reply to a thread member, threads it."
  (with-current-buffer room
    (unless (elneo-sticker--supported-p elneo-room--profile)
      (user-error "This elneo-server is too old for stickers (protocol %d); rebuild and restart it"
                  elneo-sticker--protocol))
    (let* ((reply-to elneo-room--reply-to)
           (thread-root (or elneo-room--thread-root
                            (and reply-to
                                 (plist-get (gethash reply-to elneo-room--seen)
                                            :thread-root)))))
      (elneo-send-sticker elneo-room--profile elneo-room--room-id sticker
                          #'elneo-room--on-send-result reply-to thread-root)
      (when reply-to
        (setq elneo-room--reply-to nil
              elneo-room--reply-sender nil)
        (elneo-room--update-separator)))))

;;;; Rendering

(defun elneo-sticker--image (mxc)
  "The picker image of sticker MXC, or nil while it is not fetched."
  (when-let* ((file (elneo-client--reaction-file mxc))
              (display (elneo--image-for-display file)))
    (condition-case nil
        (create-image display nil nil
                      :max-width elneo-sticker-size
                      :max-height elneo-sticker-size
                      :ascent 'center :margin 4)
      (error nil))))

(defun elneo-sticker--placeholder ()
  "A blank the size of a sticker cell, kept until its image lands."
  `(space :width (,(+ 8 elneo-sticker-size))
          :height (,(+ 8 elneo-sticker-size))))

(defun elneo-sticker--cell (pack sticker)
  "The text of STICKER's cell in PACK."
  (let* ((mxc (alist-get 'url sticker))
         (graphic (display-graphic-p))
         (img (and graphic (elneo-sticker--image mxc))))
    (propertize (if graphic " " (format "[%s]" (alist-get 'shortcode sticker)))
                'display (and graphic (or img (elneo-sticker--placeholder)))
                'elneo-sticker sticker
                'elneo-sticker-pack pack
                'elneo-sticker-url mxc
                'mouse-face 'highlight
                'help-echo (format ":%s: %s" (alist-get 'shortcode sticker)
                                   (alist-get 'body sticker))
                'keymap elneo-sticker-cell-map)))

(defun elneo-sticker--per-row (window)
  "How many sticker cells fit on one line of WINDOW."
  (if (display-graphic-p)
      (max 1 (/ (window-body-width window t) (+ 8 elneo-sticker-size)))
    (max 1 (/ (window-body-width window) 14))))

(defun elneo-sticker--render ()
  "Redraw the picker from `elneo-sticker--packs', point kept by sticker."
  (let* ((inhibit-read-only t)
         (at (get-text-property (point) 'elneo-sticker-url))
         (window (or (get-buffer-window) (selected-window)))
         (per-row (elneo-sticker--per-row window))
         (shown 0))
    (erase-buffer)
    (dolist (pack elneo-sticker--packs)
      (let ((stickers (cl-remove-if-not
                       (lambda (s) (elneo-sticker--matches-p elneo-sticker--filter pack s))
                       (append (alist-get 'stickers pack) nil))))
        (when stickers
          (insert (propertize (alist-get 'name pack)
                              'face 'elneo-sticker-pack-heading
                              'elneo-sticker-heading t)
                  "  "
                  (propertize (elneo-sticker--source-label pack)
                              'face 'elneo-sticker-pack-source)
                  "\n")
          (let ((col 0))
            (dolist (s stickers)
              (when (= col per-row)
                (insert "\n")
                (setq col 0))
              (insert (elneo-sticker--cell pack s))
              (cl-incf col)
              (cl-incf shown)
              (elneo-sticker--ensure-image (alist-get 'url s))))
          (insert "\n\n"))))
    (when (zerop shown)
      (insert (if elneo-sticker--filter
                  (format "No sticker matches \"%s\" (/ with empty input clears).\n"
                          elneo-sticker--filter)
                (concat "No sticker packs here.\n\n"
                        "Packs are MSC2545 image packs: a room's or its space's, your\n"
                        "personal one, or room packs you enabled globally.  Create them\n"
                        "in Cinny, FluffyChat or Nheko.\n"))))
    (goto-char (point-min))
    (let ((found (and at (text-property-search-forward 'elneo-sticker-url at t))))
      (if found
          (goto-char (prop-match-beginning found))
        (goto-char (point-min))
        (elneo-sticker-next)))))

(defun elneo-sticker--ensure-image (mxc)
  "Fetch sticker MXC's thumbnail unless it is at hand or on its way."
  (when (and (display-graphic-p)
             (not (gethash mxc elneo-client--reaction-cache))
             (not (gethash mxc elneo-client--reaction-pending)))
    (let ((buffer (current-buffer)))
      (elneo-client-fetch-reaction-image
       (buffer-local-value 'elneo-room--profile elneo-sticker--room) mxc
       (lambda (_profile _event)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (elneo-sticker--arrived mxc))))))))

(defun elneo-sticker--arrived (mxc)
  "Show MXC's thumbnail in every cell waiting for it."
  (when-let* ((img (elneo-sticker--image mxc)))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (let (match)
          (while (setq match (text-property-search-forward 'elneo-sticker-url mxc t))
            (with-silent-modifications
              (put-text-property (prop-match-beginning match) (prop-match-end match)
                                 'display img))))))))

;;;; Commands

(defun elneo-sticker--at-point ()
  "The sticker alist at point, or nil."
  (get-text-property (point) 'elneo-sticker))

(defun elneo-sticker-send-at-point ()
  "Send the sticker at point to the picker's room."
  (interactive)
  (let ((sticker (elneo-sticker--at-point))
        (room elneo-sticker--room))
    (unless sticker (user-error "No sticker at point"))
    (unless (buffer-live-p room)
      (user-error "The room this picker was opened from is gone"))
    (elneo-sticker-send room sticker)
    (message "Matrix: sent :%s:" (alist-get 'shortcode sticker))
    (when elneo-sticker-quit-after-send
      (quit-window)
      (when-let* ((win (get-buffer-window room)))
        (select-window win)))))

(defun elneo-sticker-mouse-send (event)
  "Send the sticker clicked on in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (elneo-sticker-send-at-point))

(defun elneo-sticker-next (&optional n)
  "Move to the next sticker (N times)."
  (interactive "p")
  (dotimes (_ (or n 1))
    (let ((here (point)))
      (when (get-text-property (point) 'elneo-sticker)
        (forward-char 1))
      (let ((m (text-property-search-forward 'elneo-sticker nil
                                             (lambda (_ v) v) t)))
        (if m (goto-char (prop-match-beginning m)) (goto-char here))))))

(defun elneo-sticker-previous (&optional n)
  "Move to the previous sticker (N times)."
  (interactive "p")
  (dotimes (_ (or n 1))
    (let ((m (text-property-search-backward 'elneo-sticker nil
                                            (lambda (_ v) v) t)))
      (when m (goto-char (prop-match-beginning m))))))

(defun elneo-sticker-next-pack ()
  "Move to the first sticker of the next pack."
  (interactive)
  (let ((m (save-excursion
             (end-of-line)
             (text-property-search-forward 'elneo-sticker-heading t t))))
    (when m
      (goto-char (prop-match-end m))
      (elneo-sticker-next))))

(defun elneo-sticker-previous-pack ()
  "Move to the first sticker of the previous pack."
  (interactive)
  (let ((m (save-excursion
             (when (text-property-search-backward 'elneo-sticker-heading t t)
               (text-property-search-backward 'elneo-sticker-heading t t)))))
    (when m
      (goto-char (prop-match-end m))
      (elneo-sticker-next))))

(defun elneo-sticker-filter (filter)
  "Show only the stickers matching FILTER; empty input shows all.
Words match the shortcode, the body and the pack's name."
  (interactive (list (read-string "Stickers matching: " elneo-sticker--filter)))
  (setq elneo-sticker--filter (and (not (string-blank-p filter)) (string-trim filter)))
  (elneo-sticker--render))

(defun elneo-sticker-refresh ()
  "Fetch the room's packs again."
  (interactive)
  (elneo-sticker--fetch (current-buffer)))

(defun elneo-sticker--fetch (picker)
  "Fetch the packs of PICKER's room and render them into PICKER."
  (let ((room (buffer-local-value 'elneo-sticker--room picker)))
    (with-current-buffer room
      (elneo-sticker-packs
       elneo-room--profile elneo-room--room-id
       (lambda (profile event)
         (cond
          ((not (buffer-live-p picker)))
          ((not (alist-get '@ok event t nil #'equal))
           (message "Matrix [%s]: sticker packs failed: %s" profile
                    (elneo-server--error-string event)))
          (t
           (with-current-buffer picker
             (setq elneo-sticker--packs (append (alist-get 'packs event) nil))
             (elneo-sticker--render)))))))))

(defvar elneo-sticker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elneo-sticker-send-at-point)
    (define-key map (kbd "n") #'elneo-sticker-next)
    (define-key map (kbd "p") #'elneo-sticker-previous)
    (define-key map (kbd "f") #'elneo-sticker-next)
    (define-key map (kbd "b") #'elneo-sticker-previous)
    (define-key map (kbd "<right>") #'elneo-sticker-next)
    (define-key map (kbd "<left>") #'elneo-sticker-previous)
    (define-key map (kbd "TAB") #'elneo-sticker-next-pack)
    (define-key map (kbd "<backtab>") #'elneo-sticker-previous-pack)
    (define-key map (kbd "/") #'elneo-sticker-filter)
    (define-key map (kbd "s") #'elneo-sticker-filter)
    (define-key map (kbd "g") #'elneo-sticker-refresh)
    map)
  "Keymap of `elneo-sticker-mode'.")

(define-derived-mode elneo-sticker-mode special-mode "Stickers"
  "Pick a sticker from the room's MSC2545 packs.
\\{elneo-sticker-mode-map}"
  (setq-local truncate-lines t
              cursor-type 'box
              header-line-format
              '(:eval (format " Stickers → %s   RET send  n/p move  TAB pack  / filter  g reload  q close"
                              (if (buffer-live-p elneo-sticker--room)
                                  (buffer-name elneo-sticker--room)
                                "(room gone)"))))
  (add-hook 'window-size-change-functions
            (lambda (_frame)
              (when (derived-mode-p 'elneo-sticker-mode)
                (elneo-sticker--render)))
            nil t))

;;;###autoload
(defun elneo-sticker-pick ()
  "Pick a sticker from this room's packs and send it.
The packs are the room's own MSC2545 image packs, its parent
spaces', your personal pack and the packs you enabled everywhere."
  (interactive)
  (unless (derived-mode-p 'elneo-room-mode)
    (user-error "Run this in an elneo room"))
  (unless (elneo-sticker--supported-p elneo-room--profile)
    (user-error "This elneo-server is too old for stickers (protocol %d); rebuild and restart it"
                elneo-sticker--protocol))
  (let ((room (current-buffer))
        (picker (get-buffer-create "*elneo stickers*")))
    (with-current-buffer picker
      (unless (derived-mode-p 'elneo-sticker-mode)
        (elneo-sticker-mode))
      (setq elneo-sticker--room room
            elneo-sticker--filter nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Fetching sticker packs...\n")))
    (pop-to-buffer picker '((display-buffer-reuse-window display-buffer-below-selected)
                            (window-height . 0.4)))
    (elneo-sticker--fetch picker)))

(provide 'elneo-sticker)
;;; elneo-sticker.el ends here
