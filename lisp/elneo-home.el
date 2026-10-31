;;; elneo-home.el --- home page: every account's rooms, by space -*- lexical-binding: t; -*-

;;; Commentary:

;; One buffer over all accounts: each account's spaces as a foldable
;; tree (sub-spaces nest), then the rooms and people in no space.
;; Space rows sum their subtree's unread, so a folded tree still says
;; where to go.  Spaces can be hidden (`h'): they and everything under
;; them leave this tree, the room bar and the mode-line count, and
;; wait in the "Hidden" section at the foot, where `h' or RET brings
;; them back.  The registry and hidden state live in elneo-client;
;; this file only draws and navigates.
;;
;; Keys:
;;   RET  open the room / fold the space or account / show a hidden space
;;   TAB  fold or unfold the space or account at point
;;   h    hide the space at point (on a room: the space it sits in)
;;   n p  next / previous row     N P  next / previous space or account
;;   u    next row with unread    s    focus one space (completing)
;;   >    focus the space at point     ^  leave the focus, one level up
;;   f    cycle the account filter     0-9  all accounts / the Nth one
;;   b    the space's directory (rooms to join)
;;   g    redraw                  q    quit

;;; Code:

(require 'cl-lib)
(require 'elneo-client)
(require 'elneo-room-bar)

(declare-function elneo-space-browse "elneo-space" (profile room-id))

(defface elneo-home-account
  '((t :inherit elneo-room-bar-header :height 1.1))
  "Face for account headings on the home page."
  :group 'elneo)

(defface elneo-home-hidden
  '((t :inherit shadow :slant italic))
  "Face for the hidden spaces at the home page's foot."
  :group 'elneo)

(defvar elneo-home--buffer-name "*Matrix Home*")

(defvar elneo-home--folded (make-hash-table :test 'equal)
  "Fold state by (PROFILE . ID); ID is a space's room id or the
symbol `account'.  Spaces start folded, accounts unfolded: the value
is `open' or `closed' once toggled.")

(defvar elneo-home--account nil
  "Account the home page shows, nil for all.")

(defvar elneo-home--focus nil
  "(PROFILE . SPACE-ID) the home page is narrowed to, or nil.")

(defvar elneo-home--timer nil)

(defun elneo-home--folded-p (profile id)
  "Non-nil when PROFILE's ID (space id or `account') is folded."
  (pcase (gethash (cons profile id) elneo-home--folded)
    ('open nil)
    ('closed t)
    (_ (not (eq id 'account)))))

;;; Tree

(defun elneo-home--joined (profile)
  "PROFILE's joined, not removed rooms."
  (cl-remove-if-not
   (lambda (r) (and (equal (plist-get r :profile) profile)
                    (not (plist-get r :removed))
                    (equal (plist-get r :room-state) "joined")))
   (elneo-rooms-all)))

(defun elneo-home--sort (rooms)
  "ROOMS with spaces first by name, then the rest by activity."
  (sort rooms
        (lambda (a b)
          (cond ((and (plist-get a :space) (not (plist-get b :space))) t)
                ((and (plist-get b :space) (not (plist-get a :space))) nil)
                ((plist-get a :space)
                 (string-lessp (downcase (plist-get a :display-name))
                               (downcase (plist-get b :display-name))))
                (t (> (or (plist-get a :last-activity) 0)
                      (or (plist-get b :last-activity) 0)))))))

(defun elneo-home--children (space by-id)
  "Joined rooms BY-ID lists as children of the SPACE record."
  (elneo-home--sort
   (delq nil (mapcar (lambda (id) (gethash id by-id))
                     (plist-get space :space-children)))))

(defun elneo-home--counts (profile space by-id)
  "(UNREAD . HIGHLIGHTS) over PROFILE's SPACE subtree, BY-ID's rooms."
  (let ((unread 0) (hl 0))
    (dolist (id (elneo-space-subtree profile (plist-get space :room-id)))
      (when-let* ((r (gethash id by-id)))
        (unless (plist-get r :space)
          (cl-incf unread (or (plist-get r :unread) 0))
          (cl-incf hl (or (plist-get r :highlights) 0)))))
    (cons unread hl)))

;;; Rendering

(defun elneo-home--count-text (unread hl)
  "\" 3\" / \" 3 !1\" badge for UNREAD and HL, \"\" when both are 0."
  (concat (if (> unread 0)
              (propertize (format " %d" unread) 'face 'elneo-room-bar-unread)
            "")
          (if (> hl 0)
              (propertize (format " !%d" hl) 'face 'elneo-room-bar-highlight)
            "")))

(defun elneo-home--line (text &rest props)
  "Insert TEXT and a newline, the whole line carrying PROPS."
  (let ((beg (point)))
    (insert text "\n")
    (add-text-properties beg (point) props)))

(defun elneo-home--insert-room (room depth space)
  "Insert ROOM's row at DEPTH, inside SPACE (a room id or nil)."
  (let* ((unread (or (plist-get room :unread) 0))
         (name (plist-get room :display-name)))
    (elneo-home--line
     (concat (make-string (* 2 depth) ?\s)
             (if (plist-get room :direct) elneo-room-bar-dm-glyph "#")
             " "
             (propertize name 'face (cond ((elneo-room-muted-p room)
                                           'elneo-room-bar-muted)
                                          ((> unread 0) 'elneo-room-bar-unread)
                                          (t 'elneo-room-bar-room)))
             (elneo-home--count-text unread (or (plist-get room :highlights) 0)))
     'elneo-home-kind 'room
     'elneo-home-room room
     'elneo-home-space space
     'elneo-home-unread (> unread 0))))

(defun elneo-home--insert-space (profile space depth by-id hidden seen)
  "Insert PROFILE's SPACE row at DEPTH and, unfolded, its subtree.
HIDDEN is the hidden-room table; SEEN the space ids on this path."
  (let* ((id (plist-get space :room-id))
         (folded (elneo-home--folded-p profile id))
         (counts (elneo-home--counts profile space by-id)))
    (elneo-home--line
     (concat (make-string (* 2 depth) ?\s)
             (if folded "▸ " "▾ ")
             (propertize (concat elneo-room-bar-space-glyph
                                 (plist-get space :display-name))
                         'face 'elneo-room-bar-space)
             (elneo-home--count-text (car counts) (cdr counts)))
     'elneo-home-kind 'space
     'elneo-home-room space
     'elneo-home-space id
     'elneo-home-unread (> (car counts) 0))
    (unless (or folded (member id seen))
      (dolist (child (elneo-home--children space by-id))
        (unless (elneo-room-hidden-p child hidden)
          (if (plist-get child :space)
              (elneo-home--insert-space profile child (1+ depth) by-id
                                        hidden (cons id seen))
            (elneo-home--insert-room child (1+ depth) id)))))))

(defun elneo-home--insert-account (profile hidden)
  "Insert PROFILE's heading and tree; HIDDEN is the hidden-room table."
  (let* ((rooms (cl-remove-if (lambda (r) (elneo-room-hidden-p r hidden))
                              (elneo-home--joined profile)))
         (by-id (make-hash-table :test 'equal))
         (in-space (make-hash-table :test 'equal))
         (folded (elneo-home--folded-p profile 'account))
         (unread (apply #'+ (mapcar (lambda (r) (if (plist-get r :space) 0
                                                  (or (plist-get r :unread) 0)))
                                    rooms))))
    (dolist (r rooms) (puthash (plist-get r :room-id) r by-id))
    (dolist (r rooms)
      (dolist (c (plist-get r :space-children))
        (unless (equal c (plist-get r :room-id)) (puthash c t in-space))))
    (elneo-home--line
     (concat (if folded "▸ " "▾ ")
             (propertize profile 'face 'elneo-home-account)
             (propertize (format "  %s"
                                 (or (plist-get (alist-get profile elneo-accounts
                                                           nil nil #'equal)
                                                :user-id)
                                     ""))
                         'face 'shadow)
             (if (elneo-server-live-p profile) ""
               (propertize "  offline" 'face 'elneo-room-bar-offline))
             (elneo-home--count-text unread 0))
     'elneo-home-kind 'account
     'elneo-home-profile profile)
    (unless folded
      (let ((top (elneo-home--sort
                  (cl-remove-if (lambda (r) (gethash (plist-get r :room-id) in-space))
                                rooms))))
        (dolist (space (cl-remove-if-not (lambda (r) (plist-get r :space)) top))
          (elneo-home--insert-space profile space 1 by-id hidden nil))
        (pcase-dolist (`(,label . ,pred) `(("Rooms" . ,(lambda (r) (not (plist-get r :direct))))
                                            ("People" . ,(lambda (r) (plist-get r :direct)))))
          (when-let* ((loose (cl-remove-if-not
                              (lambda (r) (and (not (plist-get r :space)) (funcall pred r)))
                              top)))
            (elneo-home--line (propertize (concat "  " label) 'face 'elneo-room-bar-section)
                              'elneo-home-kind 'section)
            (dolist (r loose) (elneo-home--insert-room r 2 nil))))))
    (insert "\n")))

(defun elneo-home--insert-hidden ()
  "Insert the Hidden section: one row per hidden space, to bring back."
  (when-let* ((spaces (cl-remove-if-not
                       (lambda (key) (or (null elneo-home--account)
                                         (equal (car key) elneo-home--account)))
                       (elneo-hidden-spaces))))
    (elneo-home--line (propertize "Hidden — h or RET shows again" 'face 'elneo-room-bar-section)
                      'elneo-home-kind 'section)
    (pcase-dolist (`(,profile . ,id) spaces)
      (let ((room (gethash (cons profile id) elneo--rooms)))
        (elneo-home--line
         (propertize (format "  %s%s  (%s)" elneo-room-bar-space-glyph
                             (if room (plist-get room :display-name) id)
                             profile)
                     'face 'elneo-home-hidden)
         'elneo-home-kind 'hidden
         'elneo-home-profile profile
         'elneo-home-space id)))))

(defun elneo-home--header ()
  "The header line: account filter, focus and the main keys."
  (concat
   " "
   (mapconcat (lambda (p)
                (let ((on (equal p elneo-home--account)))
                  (propertize (format "%s" (or p "all"))
                              'face (if on 'elneo-room-bar-header 'shadow))))
              (cons nil (mapcar #'car elneo-accounts))
              " · ")
   (when elneo-home--focus
     (let ((room (gethash elneo-home--focus elneo--rooms)))
       (format "   in %s%s (^ out)" elneo-room-bar-space-glyph
               (if room (plist-get room :display-name) (cdr elneo-home--focus)))))
   (propertize "   RET open · TAB fold · h hide · s space · f account · ? keys"
               'face 'shadow)))

(defun elneo-home--row-key ()
  "Identity of the row at point, to find it again after a redraw."
  (let ((room (get-text-property (point) 'elneo-home-room)))
    (list (get-text-property (point) 'elneo-home-kind)
          (or (plist-get room :profile) (get-text-property (point) 'elneo-home-profile))
          (or (plist-get room :room-id) (get-text-property (point) 'elneo-home-space)))))

(defun elneo-home-refresh ()
  "Redraw the home page, keeping point on the same row."
  (interactive)
  (when-let* ((buffer (get-buffer elneo-home--buffer-name)))
    (with-current-buffer buffer
      (let* ((key (elneo-home--row-key))
             (line (line-number-at-pos))
             (windows (get-buffer-window-list buffer nil t))
             (starts (mapcar #'window-start windows))
             (hidden (elneo-hidden-room-table))
             (inhibit-read-only t))
        (erase-buffer)
        (setq header-line-format (elneo-home--header))
        (if elneo-home--focus
            (let* ((profile (car elneo-home--focus))
                   (by-id (make-hash-table :test 'equal))
                   (space (gethash elneo-home--focus elneo--rooms)))
              (dolist (r (elneo-home--joined profile))
                (puthash (plist-get r :room-id) r by-id))
              (if (null space)
                  (insert "That space is gone.  ^ to go back.\n")
                ;; The focused space is open whatever its fold says.
                (puthash elneo-home--focus 'open elneo-home--folded)
                (elneo-home--insert-space profile space 0 by-id hidden nil)))
          (elneo--accounts-load)
          (dolist (profile (mapcar #'car elneo-accounts))
            (when (or (null elneo-home--account) (equal profile elneo-home--account))
              (elneo-home--insert-account profile hidden)))
          (elneo-home--insert-hidden))
        (goto-char (point-min))
        (while (not (or (eobp) (equal (elneo-home--row-key) key)))
          (forward-line 1))
        (when (eobp)
          (goto-char (point-min))
          (forward-line (1- line)))
        (cl-mapc #'set-window-start windows starts)
        (dolist (w windows) (set-window-point w (point)))))))

(defun elneo-home--schedule (&rest _)
  "Coalesce registry updates into one redraw."
  (when (and (get-buffer elneo-home--buffer-name) (not elneo-home--timer))
    (setq elneo-home--timer
          (run-with-idle-timer 0.3 nil
                               (lambda ()
                                 (setq elneo-home--timer nil)
                                 (elneo-home-refresh))))))

;;; Commands

(defun elneo-home--at (prop)
  "PROP of the row at point."
  (get-text-property (point) prop))

(defun elneo-home--space-key ()
  "(PROFILE . SPACE-ID) of the space at point or around the room at point."
  (when-let* ((id (elneo-home--at 'elneo-home-space)))
    (cons (or (plist-get (elneo-home--at 'elneo-home-room) :profile)
              (elneo-home--at 'elneo-home-profile))
          id)))

(defun elneo-home-toggle-fold ()
  "Fold or unfold the space or account at point."
  (interactive)
  (pcase (elneo-home--at 'elneo-home-kind)
    ('space (let ((key (elneo-home--space-key)))
              (puthash key (if (elneo-home--folded-p (car key) (cdr key)) 'open 'closed)
                       elneo-home--folded)))
    ('account (let ((p (elneo-home--at 'elneo-home-profile)))
                (puthash (cons p 'account)
                         (if (elneo-home--folded-p p 'account) 'open 'closed)
                         elneo-home--folded)))
    (_ (user-error "No space or account here")))
  (elneo-home-refresh))

(defun elneo-home-ret ()
  "Open the room at point, fold a space or account, show a hidden space."
  (interactive)
  (pcase (elneo-home--at 'elneo-home-kind)
    ('room (elneo-room-open (elneo-home--at 'elneo-home-room)))
    ('hidden (elneo-home-hide))
    ((or 'space 'account) (elneo-home-toggle-fold))))

(defun elneo-home-hide ()
  "Hide the space at point (on a room, its space); on a hidden row, show it."
  (interactive)
  (let ((key (elneo-home--space-key))
        (hidden (eq (elneo-home--at 'elneo-home-kind) 'hidden)))
    (unless key (user-error "Not in a space"))
    (let ((name (if-let* ((r (gethash key elneo--rooms))) (plist-get r :display-name) (cdr key))))
      (when (and (not hidden) (equal key elneo-home--focus))
        (setq elneo-home--focus nil))
      (elneo-space-set-hidden (car key) (cdr key) (not hidden))
      (message (if hidden "Matrix: %s shown again" "Matrix: %s hidden — it waits under Hidden at the bottom")
               name))))

(defun elneo-home--move (n pred)
  "Move to the Nth next row (backwards for negative N) satisfying PRED."
  (let ((pos (point)) (found nil))
    (dotimes (_ (abs n))
      (setq found nil)
      (while (and (not found) (zerop (forward-line (if (< n 0) -1 1))) (not (eobp)))
        (when (funcall pred) (setq found t pos (point)))))
    (goto-char pos)
    (unless found (message "Matrix: no more"))))

(defun elneo-home-next (n)
  "Move to the Nth next row."
  (interactive "p")
  (elneo-home--move n (lambda () (memq (elneo-home--at 'elneo-home-kind)
                                       '(room space account hidden)))))

(defun elneo-home-previous (n)
  "Move to the Nth previous row."
  (interactive "p")
  (elneo-home-next (- n)))

(defun elneo-home-next-space (n)
  "Move to the Nth next space or account row."
  (interactive "p")
  (elneo-home--move n (lambda () (memq (elneo-home--at 'elneo-home-kind) '(space account)))))

(defun elneo-home-previous-space (n)
  "Move to the Nth previous space or account row."
  (interactive "p")
  (elneo-home-next-space (- n)))

(defun elneo-home-next-unread ()
  "Move to the next row with unread messages, wrapping."
  (interactive)
  (let ((start (point)))
    (elneo-home--move 1 (lambda () (elneo-home--at 'elneo-home-unread)))
    (when (= (point) start)
      (goto-char (point-min))
      (unless (elneo-home--at 'elneo-home-unread)
        (elneo-home--move 1 (lambda () (elneo-home--at 'elneo-home-unread)))))))

(defun elneo-home--focus-on (key)
  "Narrow the home page to space KEY, (PROFILE . SPACE-ID)."
  (setq elneo-home--focus key)
  (elneo-home-refresh)
  (goto-char (point-min)))

(defun elneo-home-focus-space ()
  "Pick a shown space of any account and narrow the home page to it."
  (interactive)
  (let* ((spaces (cl-remove-if-not
                  (lambda (r) (and (plist-get r :space)
                                   (or (null elneo-home--account)
                                       (equal (plist-get r :profile) elneo-home--account))))
                  (elneo-rooms-shown)))
         (cands (mapcar (lambda (r) (cons (elneo--room-pick-label r) r)) spaces))
         (pick (cdr (assoc (completing-read "Space: " cands nil t) cands))))
    (elneo-home--focus-on (cons (plist-get pick :profile) (plist-get pick :room-id)))))

(defun elneo-home-focus-at-point ()
  "Narrow the home page to the space at point."
  (interactive)
  (unless (eq (elneo-home--at 'elneo-home-kind) 'space)
    (user-error "No space here"))
  (elneo-home--focus-on (elneo-home--space-key)))

(defun elneo-home-unfocus ()
  "Leave the space focus: up to its parent space, else the full page."
  (interactive)
  (let* ((profile (car elneo-home--focus))
         (parent (and elneo-home--focus
                      (seq-find (lambda (r)
                                  (and (equal (plist-get r :profile) profile)
                                       (not (equal (plist-get r :room-id)
                                                   (cdr elneo-home--focus)))
                                       (member (cdr elneo-home--focus)
                                               (plist-get r :space-children))))
                                (elneo-rooms-shown)))))
    (elneo-home--focus-on (and parent (cons profile (plist-get parent :room-id))))))

(defun elneo-home-set-account (profile)
  "Show only PROFILE (nil: every account)."
  (setq elneo-home--account profile
        elneo-home--focus nil)
  (elneo-home-refresh)
  (message "Matrix: %s" (or profile "all accounts")))

(defun elneo-home-cycle-account ()
  "Cycle the account filter: all, then each account in turn."
  (interactive)
  (let ((all (cons nil (mapcar #'car elneo-accounts))))
    (elneo-home-set-account (or (cadr (member elneo-home--account all)) nil))))

(defun elneo-home-digit ()
  "0 shows every account, N the Nth account only."
  (interactive)
  (let ((n (- last-command-event ?0)))
    (elneo-home-set-account (and (> n 0)
                                 (or (car (nth (1- n) elneo-accounts))
                                     (user-error "No account %d" n))))))

(defun elneo-home-browse-space ()
  "Open the directory page of the space at point (rooms to join)."
  (interactive)
  (let ((key (elneo-home--space-key)))
    (unless key (user-error "Not in a space"))
    (require 'elneo-space)
    (elneo-space-browse (car key) (cdr key))))

(defvar-keymap elneo-home-mode-map
  :parent special-mode-map
  "RET" #'elneo-home-ret
  "TAB" #'elneo-home-toggle-fold
  "<mouse-1>" #'elneo-home-ret
  "h" #'elneo-home-hide
  "n" #'elneo-home-next
  "p" #'elneo-home-previous
  "N" #'elneo-home-next-space
  "P" #'elneo-home-previous-space
  "u" #'elneo-home-next-unread
  "s" #'elneo-home-focus-space
  ">" #'elneo-home-focus-at-point
  "^" #'elneo-home-unfocus
  "f" #'elneo-home-cycle-account
  "b" #'elneo-home-browse-space
  "g" #'elneo-home-refresh
  "0" #'elneo-home-digit "1" #'elneo-home-digit "2" #'elneo-home-digit
  "3" #'elneo-home-digit "4" #'elneo-home-digit "5" #'elneo-home-digit
  "6" #'elneo-home-digit "7" #'elneo-home-digit "8" #'elneo-home-digit
  "9" #'elneo-home-digit)

(define-derived-mode elneo-home-mode special-mode "Matrix Home"
  "Every account's rooms, grouped by space.
\\{elneo-home-mode-map}"
  (setq truncate-lines t)
  (hl-line-mode 1))

;;;###autoload
(defun elneo-home ()
  "Show the Matrix home page: rooms of all accounts, by space."
  (interactive)
  (let ((buffer (get-buffer-create elneo-home--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'elneo-home-mode)
        (elneo-home-mode)))
    (add-hook 'elneo-room-update-hook #'elneo-home--schedule)
    (add-hook 'elneo-session-sync-hook #'elneo-home--schedule)
    (add-hook 'elneo-hidden-spaces-changed-hook #'elneo-home--schedule)
    (pop-to-buffer-same-window buffer)
    (elneo-home-refresh)))

(provide 'elneo-home)
;;; elneo-home.el ends here
