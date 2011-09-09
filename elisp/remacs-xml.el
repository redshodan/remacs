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
;;; xml utilies to complete emacs xml.el abilities. Includes some modified
;;; front end functions from emacs's xml.el.
;;;

;; xml structure:
;;    xml-list   ::= (node node ...)
;;    node       ::= (qname attribute-list . child_node_list)
;;    child_node_list ::= child_node child_node ...
;;    child_node ::= node | string
;;    qname      ::= (:namespace-uri . "name") | "name"
;;    attribute_list ::= ((qname . "value") (qname . "value") ...)
;;                       | nil
;;    string     ::= "..."
;;
;; xml functions:
;;   xml-nodep (node)
;;   xml-node-node (node-or-xml-list)
;;   xml-node (tag &optional text &rest attrs)
;;   xml-node-name (node)
;;   xml-node-attributes (node)
;;   xml-node-children (node)
;;   xml-get-child (node child-name)
;;   xml-get-children (node child-name)
;;   xml-put-child (node child)
;;   xml-get-attribute-or-nil (node attribute)
;;   xml-get-attribute (node attribute)
;;   xml-put-attribute (node attribute value)


(require 'xml)


(defun xml-nodep (node)
  (condition-case err
      (and
       ;; (symbol ...)
       (when (listp node)
         (symbolp (car node)))
       (or
        ;; (symbol ((a . b) ...) ...)
        (when (car (cdr node))
          (and (listp (car (cdr node)))
               (consp (car (car (cdr node))))))
        ;; (symbol nil ...)
        (and (eq (car (cdr node)) nil)
             (or
              ;; (symbol nil string ...)
              (stringp (cddr node))
              ;; (symbol nil (node ...))
              (symbolp (caar (cddr node)))
              ))
        ;; (symbol nil)
        (and (eq (length node) 2)
             (eq (car (cdr node)) nil))
        ;; (symbol nil nil)
        (and (eq (length node) 3)
             (eq (car (cdr node)) nil)
             (eq (cadar (cdr node)) nil))
        ))
    (error nil)))
  
(defun xml-node-node (node-or-xml-list)
  (if (stringp node-or-xml-list)
      node-or-xml-list
    (if (xml-nodep (car node-or-xml-list))
        (car node-or-xml-list)
      (if (listp (car node-or-xml-list))
          (xml-node-node (car node-or-xml-list))
        node-or-xml-list))))

(defun xml-node (tag &optional text &rest attrs)
  (let ((tattrs) (children) (key))
    (dolist (attr attrs)
      (if key
          (progn
            (push (cons key attr) tattrs)
            (setq key nil))
        (setq key attr)))
    (when text
      (setq children text))
    (list tag tattrs children)))

(defun xml-get-child (node &optional child-name)
  (xml-node-node (xml-get-children node child-name)))

(defun xml-put-child (node tag &optional text &rest attrs)
  (message "node %s - %s - %s" (xml-node-node node) (last (xml-node-node node))
           (last (last (xml-node-node node))))
  (let ((child (apply 'xml-node tag text attrs))
         (l (last (xml-node-node node))))
    (if (car l)
        (setcdr l (cons child nil))
      (setcar l child)))
  (message "node after %s" node)
  node)

(defun xml-put-child-string (node text)
  (setcdr (last (cddr (xml-node-node node))) (cons text '()))
  node)

(defun xml-put-attribute (node attribute value)
  (setq node (xml-node-node node))
  (let ((attrs (xml-node-attributes node)))
    (if (not attrs)
        (setcar (cdr node) (list (cons attribute value)))
      (setcar (cdr node) (put-alist attribute value attrs)))
    node))

(defsubst xml-node-name (node)
  "Return the tag associated with NODE.
Without namespace-aware parsing, the tag is a symbol.

With namespace-aware parsing, the tag is a cons of a string
representing the uri of the namespace with the local name of the
tag.  For example,

    <foo>

would be represented by

    '(\"\" . \"foo\")."

  (car (xml-node-node node)))

(defsubst xml-node-attributes (node)
  "Return the list of attributes of NODE.
The list can be nil."
  (nth 1 (xml-node-node node)))

(defsubst xml-node-children (node)
  "Return the list of children of NODE.
This is a list of nodes, and it can be nil."
  (cddr (xml-node-node node)))

(defun xml-get-children (node &optional child-name)
  "Return the children of NODE whose tag is CHILD-NAME.
CHILD-NAME should match the value returned by `xml-node-name'."
  (if child-name
      (let ((match ()))
        (dolist (child (xml-node-children (xml-node-node node)))
          (if (and (listp child)
                   (equal (xml-node-name child) child-name))
              (push child match)))
        (nreverse match))
    (car (xml-node-children (xml-node-node node)))))

(defun xml-get-attribute-or-nil (node attribute)
  "Get from NODE the value of ATTRIBUTE.
Return nil if the attribute was not found.

See also `xml-get-attribute'."
  (cdr (assoc attribute (xml-node-attributes (xml-node-node node)))))

(defsubst xml-get-attribute (node attribute)
  "Get from NODE the value of ATTRIBUTE.
An empty string is returned if the attribute was not found.

See also `xml-get-attribute-or-nil'."
  (or (xml-get-attribute-or-nil (xml-node-node node) attribute) ""))

;; Was jabber-sexp2xml from jabber.el
(defun xml-node-to-string (sexp)
  "converts an SEXP in the format (tagname ((attribute-name . attribute-value)..)
  children...) and converts it to well-formatted xml."
  (setq sexp (xml-node-node sexp))
  (cond
   ((stringp sexp)
    (xml-escape-xml sexp))
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
                                      (xml-escape-xml (cdr attr))
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

;; Was jabber-escape-xml from jabber.el
(defun xml-escape-xml (str)
  "escape strings for xml"
  (if (stringp str)
      (let ((newstr (concat str)))
        ;; Form feeds might appear in code you copy, etc.  Nevertheless,
        ;; it's invalid XML.
        (setq newstr (replace-regexp-in-string "\f" "\n" newstr))
        ;; Other control characters are also illegal, except for
        ;; tab, CR, and LF.
        (setq newstr (replace-regexp-in-string
                      "[\000-\010\013\014\016-\037]" " " newstr))
        (setq newstr (replace-regexp-in-string "&" "&amp;" newstr))
        (setq newstr (replace-regexp-in-string "<" "&lt;" newstr))
        (setq newstr (replace-regexp-in-string ">" "&gt;" newstr))
        (setq newstr (replace-regexp-in-string "'" "&apos;" newstr))
        (setq newstr (replace-regexp-in-string "\"" "&quot;" newstr))
        newstr)
    str))

(provide 'remacs-xml)
