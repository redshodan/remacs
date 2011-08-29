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


;;;
;;; Notification
;;;


(defvar remacs-notify-counter 0)
(defvar remacs-notify-alist '())


(defun remacs-notify (title body &optional cb)
  (setq remacs-notify-counter (+ 1 remacs-notify-counter))
  (set-alist 'remacs-notify-alist (format "%s" remacs-notify-counter)
             (list title body cb))
  (let ((msg (format (concat "<notify id='%d' type='set'><title>%s</title>"
                             "<body>%s</body></notify>")
                     remacs-notify-counter title body)))
    (remacs-send-string msg)
    remacs-notify-counter))

(defun remacs-notify-invoke (id proc ack-only)
  (let ((notif (get-alist id remacs-notify-alist)))
    (if (not notif)
        (remacs-send-error proc (format "Failed to find notification: %s" id))
      (progn
        (if (or ack-only (not (nth 2 notif)))
            (remacs-log (format "Clearing notification %s: %s" id notif))
          (remacs-log (format "Invoking notification %s: %s" id notif))
          (funcall (nth 2 notif) id))
        (remove-alist 'remacs-notify-alist id)))))

(provide 'remacs-notify)
