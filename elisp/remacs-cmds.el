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

(defvar remacs-query-cmd-handlers-alist
  '((setup . remacs-process-setup)
    (notify . remacs-process-notify)
    (msg . remacs-process-msg)
    (resume . remacs-process-resume)
    (eval . remacs-process-eval)
    (unidle . remacs-process-unidle)))

(defun remacs-process-command (proc frame tty-name xml)
  (let* ((child (xml-get-child xml))
         (handler
          (when (and child (eq (xml-node-name xml) 'query))
            (get-alist (xml-node-name child)
                       remacs-query-cmd-handlers-alist))))
    (if handler
        (funcall handler proc frame tty-name xml child)
      (error "Unknown command"))))

;; <setup>
(defun remacs-process-setup (proc frame tty-name xml child)
  (let ((tty (xml-get-children child 'tty))
        (env (xml-get-children child 'env))
        (session (xml-get-children child 'session))
        (filter (xml-get-children child 'filter))
        (envvar) (tty-name) (tty-term) (filters))
    ;; <env>
    (when env
      (dolist (var (xml-get-children env 'var))
        ;; XXX Variables should be encoded as in getenv/setenv.
        (process-put proc 'env (cons (car (xml-node-children var))
                                     (process-get proc 'env)))))
    ;; <tty>
    (setq tty-name (xml-get-attribute tty 'name)
          frame (remacs-create-tty-frame
                 tty-name (xml-get-attribute tty 'term) proc))
    ;; <session>
    (process-put proc 'id (xml-get-attribute session 'name))
    ;; <filter>
    (when filter
      (dolist (f (xml-node-children filter))
        (push (xml-node-name f) filters))
      (process-put proc 'stanza-filters filters))))

;; <notify>
(defun remacs-process-notify (proc frame tty-name xml child)
  (lexical-let ((child child))
    (list (lambda ()
            (let ((id (xml-get-attribute child 'id))
                  (invoke (car (xml-node-children child))))
              (remacs-notify-invoke id proc (not invoke))
              (remacs-broadcast xml proc))))))

;; <msg>
(defun remacs-process-msg (proc frame tty-name xml child)
  (let ((msg (format "remacs message: %s"
                     (car (xml-node-children (xml-node-name child))))))
    (remacs-log msg proc)
    (message msg)))

;; <resume>
(defun remacs-process-resume (proc frame tty-name xml child)
  (remacs-log "Resuming" proc)
  (lexical-let ((terminal (process-get proc 'terminal)))
    (list (lambda ()
            (when (eq (terminal-live-p terminal) t)
              (resume-tty terminal))))))

;; <eval>
(defun remacs-process-eval (proc frame tty-name xml child)
  (let ((coding-system (and default-enable-multibyte-characters
                            (or file-name-coding-system
                                default-file-name-coding-system))))
    (lexical-let ((expr (car (xml-node-children child))))
      (if coding-system
          (setq expr (decode-coding-string expr coding-system)))
      (list (lambda () (remacs-eval-and-print expr proc))))))

;; <unidle>
(defun remacs-process-unidle (proc frame tty-name xml child)
  (list
   (lexical-let ((xml xml))
     (lambda () (remacs-handle-unidle xml proc)))))

(provide 'remacs-cmds)
