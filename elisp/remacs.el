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
;;; Based on server.el.
;;;

;;
;; Protocol:
;;
;; Sent:
;;   <emacs pid='12345'/>
;;   <error>str</error>
;;   <notify id='12345'><title>str</title><body>str</body></notify>
;;   <suspend/>
;;
;; Received:
;;   <setup>
;;     <tty name='' term='' row='' col=''/>
;;     <id name=''/>
;;     <env>
;;       <var>NAME=VALUE</var>
;;     </env>
;;     <filter>      -- optional
;;       <unidle/>   -- and other stanzas
;;     </filter>
;;   </setup>
;;   <eval>str</eval>
;;   <resume/>
;;
;;   Broadcasted messages:
;;     <msg>str</msg>
;;     <notify id='12345' type='set|result'/>
;;     <unidle/>
;;

(require 'cl)
(require 'xml)
(require 'remacs-utils)
(require 'remacs-router)
(require 'remacs-idle)
(require 'remacs-server)


(defvar remacs-name "remacs"
  "Name of the remacs server. Used to identify it locally with the
`remacs-socket-dir'")

(defvar remacs-socket-dir
  (format "%s/remacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid))
  "Directory that contains the remacs socket interface.")

(defvar remacs-file (expand-file-name remacs-name remacs-socket-dir)
  "The filename of the remacs socket, built on top of `remacs-socket-dir'")

(defvar remacs-buffer "*remacs*"
  "The name of the main remacs logging buffer")

(defvar remacs-buffer-size (* 1024 1024)
  "The maximum size of the `remacs-buffer'")

(defvar remacs-log nil
  "Set to non-nil to enable logging.")

(defvar remacs-idle-delay 20
  "Controls how often to send idle messages.")



;;;
;;; Test code
;;;

(defun remacs-test ()
  (interactive)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq remacs-log t)
  (toggle-debug-on-error)
  (remacs-start)
  (switch-to-buffer remacs-buffer))

(defun remacs-notify-test-cb (id)
  (message "remacs-notify-test-cb: %s" id))

(defun remacs-notify-test ()
  (interactive)
  (remacs-notify "1 title" "1 body")
  ;; (remacs-send-error proc "some error")
  (remacs-notify "2 title" "2 body" 'remacs-notify-test-cb))


(provide 'remacs)
