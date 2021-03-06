;;; Copyright (C) 2009 Chris Newton <redshodan@gmail.com>
;;;
;;; This file is part of remacs.
;;;
;;; remacs is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; remacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with remacs.  If not, see <http://www.gnu.org/licenses/>.

;;;
;;; Author: Chris Newton <redshodan@gmail.com>
;;; $Revision$
;;;


(defvar remacs-idle-last (current-time))
(defvar remacs-idle-idle '(0 0 0))
(defvar remacs-idle-pending t)
(defvar remacs-idle-ignore '(0 0 0))
(defvar remacs-idle-timer nil)


;;;
;;; Commands
;;;
(defun remacs-handle-unidle (xml proc)
  (remacs-log (format "unidle from %s" (process-get proc 'id)) proc)
  (remacs-do-unidle)
  (remacs-broadcast xml proc))

(defun remacs-send-unidle ()
  (unless (null remacs-clients)
    (remacs-send-xml (remacs-query 'unidle "set"))))

(defun remacs-do-unidle ()
  (let ((cur (current-time)))
    (setcar (cdr cur) (+ (car (cdr cur)) 2))
    (setq remacs-idle-ignore cur))
  (signal-process (emacs-pid) 10))


;;;
;;; Idle timer
;;;

(defun remacs-start-idle-timer ()
  (remacs-stop-idle-timer)
  (setq remacs-idle-timer (run-at-time t 1 'remacs-check-idle)))

(defun remacs-stop-idle-timer ()
  (when remacs-idle-timer
    (cancel-timer remacs-idle-timer))
  (setq remacs-idle-timer nil))

(defun remacs-check-idle ()
  (condition-case err
      (let ((cur (if (current-idle-time) (current-idle-time) '(0 0 0)))
            (now (current-time)))
        ;; (remacs-log
        ;;  (format
        ;;"remacs-check-idle: idle=%s cur=%s now=%s last=%s ignore=%s pending=%s"
        ;;   remacs-idle-idle cur now remacs-idle-last remacs-idle-ignore
        ;;   remacs-idle-pending))
        (if (and (not (equal remacs-idle-ignore '(0 0 0)))
                 (remacs-time-< remacs-idle-ignore now))
            (setq remacs-idle-ignore '(0 0 0)
                  remacs-idle-pending nil
                  remacs-idle-last now)
          (if (and (not (equal remacs-idle-ignore '(0 0 0)))
                   (not (remacs-time-< remacs-idle-ignore now)))
              (setq remacs-idle-pending nil
                    remacs-idle-last now)
            (let ((tmp (copy-tree remacs-idle-last))
                  (fire))
              (setcar (cdr tmp) (+ (car (cdr tmp)) remacs-idle-delay))
              (if (or (equal cur '(0 0 0))
                      (remacs-time-< cur remacs-idle-idle))
                  (if (remacs-time-< tmp now)
                      (setq fire t)
                    (setq remacs-idle-pending t))
                (when (and remacs-idle-pending
                           (remacs-time-< tmp now))
                  (setq fire t)))
              (when fire
                (remacs-send-unidle)
                (setq remacs-idle-pending nil
                      remacs-idle-last now)))))
        (setq remacs-idle-idle cur))
    (error
     (remacs-stop-idle-timer)
     (remacs-return-error err))))

(defun remacs-time-< (lhs rhs)
  (if (< (car lhs) (car rhs))
      t
    (if (and (= (car lhs) (car rhs))
             (< (car (cdr lhs)) (car (cdr rhs))))
        t
      (if (and (= (car lhs) (car rhs))
               (= (car (cdr lhs)) (car (cdr rhs)))
               (< (car (cdr (cdr lhs))) (car (cdr (cdr rhs)))))
          t
        nil))))


(provide 'remacs-idle)
