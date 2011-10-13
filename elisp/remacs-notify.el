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


;;;
;;; Notification
;;;


(defvar remacs-notify-counter 0)
(defvar remacs-notify-alist '())


(defun remacs-notify (title body &optional cb)
  ;; <query type='set'>
  ;;    <notify id='' type='set'>
  ;;       <title>...</title>"
  ;;       <body>...</body>
  ;;    </notify>
  ;; </query>
  (setq remacs-notify-counter (+ 1 remacs-notify-counter))
  (set-alist 'remacs-notify-alist (format "%s" remacs-notify-counter)
             (list title body cb))
  (let* ((query (remacs-query 'notify "set"))
         (notify (xml-get-child query 'notify)))
    (xml-put-child notify 'title title)
    (xml-put-child notify 'body body)
    (xml-put-attribute notify 'id (format "%s" remacs-notify-counter))
    (remacs-send-xml query)))

(defun remacs-notify-invoke (id proc ack-only)
  (let ((notif (get-alist id remacs-notify-alist)))
    (if (not notif)
        (remacs-send-error (format "Failed to find notification: %s" id) proc)
      (progn
        (if (or ack-only (not (nth 2 notif)))
            (remacs-log (format "Clearing notification %s: %s" id notif))
          (remacs-log (format "Invoking notification %s: %s" id notif))
          (funcall (nth 2 notif) id))
        (remove-alist 'remacs-notify-alist id)))))

(provide 'remacs-notify)
