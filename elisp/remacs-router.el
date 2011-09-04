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


(defvar remacs-stanza-counter 0)


(defun remacs-query (body type &optional from to text &rest attrs)
  (let ((qattrs) (key))
    (unless from (setq from remacs-id))
    (push (cons 'type type) qattrs)
    (push (cons 'from from) qattrs)
    (when to (push (cons 'to to) qattrs))
    (list (list 'query qattrs (list (apply 'xml-node body text attrs))))))

(defun remacs-send-error (err &optional proc)
  (when (not (stringp err))
    (setq err (error-message-string err)))
  (remacs-log (concat "ERROR: " err) proc)
  (remacs-send-xml (remacs-query 'error "set" nil nil err) proc))

(defun remacs-send-string (string &optional proc originator)
  (with-temp-buffer
    (insert string)
    (remacs-send-xml (xml-parse-region (point-min) (point-max))
                     proc originator)))

(defun remacs-send-xml (xml &optional proc originator)
  (if proc
      (remacs-send-xml2 xml proc)
    (dolist (proc remacs-clients)
      ;; Apply stanza filters
      (unless (or (eq proc originator)
                  (memq (car xml) (process-get proc 'stanza-filters)))
        (remacs-send-xml2 xml proc)))))

(defun remacs-send-xml2 (xml proc)
  (setq remacs-stanza-counter (+ 1 remacs-stanza-counter))
  (xml-put-attribute xml 'to (process-get proc 'id))
  (xml-put-attribute xml 'id (format "%d" remacs-stanza-counter))
  (let ((string (xml-node-to-string xml)))
    (remacs-log (concat "Sent: " string) proc)
    (process-send-string proc (concat string "\000"))
    remacs-stanza-counter))

(defun remacs-broadcast (xml proc)
  (xml-put-attribute xml 'from (process-get proc 'id))
  (remacs-log (format "broadcasting: %s" (xml-node-to-string xml)) proc)
  (remacs-send-xml xml nil proc))

(provide 'remacs-router)
