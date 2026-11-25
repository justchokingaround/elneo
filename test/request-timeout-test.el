;;; request-timeout-test.el --- a request whose answer never comes -*- lexical-binding: t; -*-

;; A daemon that panics in a handler acks the request now, but a socket
;; that goes quiet still answers nothing: the request deadline is what
;; keeps the view behind it from waiting for ever.

(require 'cl-lib)
(require 'elneo-server)

(defvar mx-failures 0)
(defun mx-check (label got want)
  (if (equal got want)
      (message "ok   %s" label)
    (setq mx-failures (1+ mx-failures))
    (message "FAIL %s\n  got:  %S\n  want: %S" label got want)))

(let ((elneo-server-request-timeout 0.05)
      (elneo-server--pending nil)
      (elneo-server--timers nil)
      (calls nil))
  (cl-letf (((symbol-function 'elneo-server--send) #'ignore))
    ;; Nothing answers: the deadline fails the request once.
    (let ((req (elneo-server--call "t" '((@type . "getSession"))
                                   (lambda (profile event)
                                     (push (cons profile event) calls)))))
      (sleep-for 0.2)
      (mx-check "the deadline fails the callback once" (length calls) 1)
      (mx-check "with the request's own profile" (car (car calls)) "t")
      (mx-check "and a reason in plain words"
                (and (string-match-p "no answer"
                                     (or (alist-get '@error (cdr (car calls))) ""))
                     t)
                t)
      (mx-check "the pending entry is gone"
                (gethash req (elneo-server--pending-table "t")) nil)
      (mx-check "so is its timer"
                (gethash req (elneo-server--timer-table "t")) nil))
    ;; An answer wins, and takes the deadline with it.
    (setq calls nil)
    (let ((req (elneo-server--call "t" '((@type . "getSession"))
                                   (lambda (_profile _event) (push t calls)))))
      (elneo-server--dispatch-event "t" (list (cons '@req req) (cons '@ok t)))
      (mx-check "the answer runs the callback" (length calls) 1)
      (mx-check "the deadline is cancelled"
                (gethash req (elneo-server--timer-table "t")) nil)
      (sleep-for 0.2)
      (mx-check "and never fires afterwards" (length calls) 1))
    ;; A daemon that goes away fails what is in flight and leaves no timer.
    (setq calls nil)
    (elneo-server--call "t" '((@type . "getSession"))
                        (lambda (_profile _event) (push t calls)))
    (elneo-server--flush-pending "t" "killed")
    (sleep-for 0.2)
    (mx-check "a flush fails the request once" (length calls) 1)
    (mx-check "and drops its timers" elneo-server--timers nil)))

(kill-emacs (if (> mx-failures 0) 1 0))
;;; request-timeout-test.el ends here
