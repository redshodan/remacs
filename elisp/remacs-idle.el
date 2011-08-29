;;; Copyright (C) 2009 Chris Newton <redshodan@gmail.com>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;;
;;; Author: Chris Newton <redshodan@gmail.com>
;;; $Revision$
;;;


(defvar remacs-idle-last (current-time))
(defvar remacs-idle-idle '(0 0 0))
(defvar remacs-idle-pending t)
(defvar remacs-idle-ignore '(0 0 0))


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

(defun remacs-do-unidle ()
  (let ((cur (current-time)))
    (setcar (cdr cur) (+ (car (cdr cur)) 2))
    (setq remacs-idle-ignore cur))
  (signal-process (emacs-pid) 10))

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
     (cancel-timer remacs-idle-timer)
     (remacs-return-error err))))

(provide 'remacs-idle)
