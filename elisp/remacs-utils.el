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
;;; Misc utilities to complete emacs behavior
;;;

(unless (fboundp 'jabber-escape-xml)
  (defun jabber-escape-xml (str)
    "escape strings for xml"
    (if (stringp str)
        (let ((newstr (concat str)))
          ;; Form feeds might appear in code you copy, etc.  Nevertheless,
          ;; it's invalid XML.
          (setq newstr (replace-regexp-in-string newstr "\f" "\n"))
          ;; Other control characters are also illegal, except for
          ;; tab, CR, and LF.
          (setq newstr (replace-regexp-in-string
                          newstr "[\000-\010\013\014\016-\037]" " "))
          (setq newstr (replace-regexp-in-string newstr "&" "&amp;"))
          (setq newstr (replace-regexp-in-string newstr "<" "&lt;"))
          (setq newstr (replace-regexp-in-string newstr ">" "&gt;"))
          (setq newstr (replace-regexp-in-string newstr "'" "&apos;"))
          (setq newstr (replace-regexp-in-string newstr "\"" "&quot;"))
          newstr)
      str)))

;; Was jabber-sexp2xml from jabber.el
(defun xml-node-to-string (sexp)
 "converts an SEXP in the format (tagname ((attribute-name . attribute-value)...)
  children...) and converts it to well-formatted xml."
  (cond
   ((stringp sexp)
    (jabber-escape-xml sexp))
   ((listp (car sexp))
    (let ((xml ""))
      (dolist (tag sexp)
        (setq xml (concat xml (xml-node-to-string tag))))
      xml))
   ;; work around bug in old versions of xml.el, where ("") can appear
   ;; as children of a node
   ((and (consp sexp)
         (stringp (car sexp))
         (zerop (length (car sexp))))
    "")
   (t
    (let ((xml ""))
      (setq xml (concat "<" 
                        (symbol-name (car sexp))))
      (dolist (attr (cadr sexp))
        (if (consp attr)
            (setq xml (concat xml
                              (format " %s='%s'"
                                      (symbol-name (car attr))
                                      (cdr attr)
                                      (jabber-escape-xml (cdr attr))
                                      )))))
      (if (cddr sexp)
          (progn
            (setq xml (concat xml ">"))
            (dolist (child (cddr sexp))
              (setq xml (concat xml
                                (xml-node-to-string child))))
            (setq xml (concat xml
                              "</"
                              (symbol-name (car sexp))
                              ">")))
        (setq xml (concat xml
                          "/>")))
      xml))))

(unless (fboundp 'xml-put-attribute)
  (defun xml-put-attribute (node attribute value)
    (let ((attrs (xml-node-attributes node)))
      (if (not attrs)
          (setcar (cdr node) (list (cons attribute value)))
        (setcar (cdr node) (put-alist attribute value attrs)))
      node)))

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
