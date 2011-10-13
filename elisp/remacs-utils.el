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
;;; Remacs buffer functions
;;;


(define-minor-mode remacs-mode
  :global t
  :group 'remacs
  :version "22.1"
  (remacs-start (not remacs-mode)))

(defun remacs-log (string &optional client)
  (when remacs-log
    (with-current-buffer (get-buffer-create remacs-buffer)
      (goto-char (point-max))
      (insert (current-time-string)
              (cond
               ((null client) " ")
               ((listp client)
                (format " %s-%s: " (car client) (process-get proc 'id)))
               (t (format " %s: " client)))
              string)
      (or (bolp) (newline))
      (remacs-truncate-buffer))))

(defun remacs-process-log (server client msg)
  (remacs-log msg client)
  (remacs-truncate-buffer))

(defun remacs-truncate-buffer ()
  (with-current-buffer (get-buffer-create remacs-buffer)
    (when (> (buffer-size) remacs-buffer-size)
      (save-restriction
        (widen)
        (let ((end (- (buffer-end 1) remacs-buffer-size)))
          (goto-char end)
          (beginning-of-line)
          (setq end (point))
          (let ((inhibit-read-only t))
            (delete-region (point-min) end)))))))

;;;
;;; misc remacs utilities
;;;
(defun remacs-hostname ()
  (with-temp-buffer
    (call-process shell-file-name nil t nil shell-command-switch "hostname")
    (beginning-of-buffer)
    (search-forward "\n")
    (narrow-to-region 1 (- (point) 1))
    (buffer-string)))

;;;
;;; alist utilities
;;;
(unless (fboundp 'get-alist)
  (defun get-alist (key alist)
    (cdr (assoc key alist))))

(unless (fboundp 'set-alist)
  (defun set-alist (symbol key value)
    "Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE."
    (or (boundp symbol)
        (set symbol nil))
    (set symbol (put-alist key value (symbol-value symbol))))
  
  (defun put-alist (key value alist)
    "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
    (let ((elm (assoc key alist)))
      (if elm
          (progn
            (setcdr elm value)
            alist)
        (cons (cons key value) alist))))
  
  (defun del-alist (key alist)
    "Delete an element whose car equals KEY from ALIST.
Return the modified ALIST."
    (let ((pair (assoc key alist)))
      (if pair
          (delq pair alist)
        alist)))
  
  (defun remove-alist (symbol key)
    "Delete an element whose car equals KEY from the alist bound to SYMBOL."
    (and (boundp symbol)
         (set symbol (del-alist key (symbol-value symbol))))))

(provide 'remacs-utils)
