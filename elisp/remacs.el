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


(require 'cl)
(require 'remacs-utils)
(require 'remacs-xml)
(require 'remacs-router)
(require 'remacs-idle)
(require 'remacs-server)
(require 'remacs-cmds)
(require 'remacs-notify)


(defvar remacs-id (concat (remacs-hostname) "-emacs")
  "Name of the remacs server. Used to identify it with other remacs entities.")

(defvar remacs-server-name "remacs"
  "Name of the remacs server. Used to identify it locally with the
`remacs-socket-dir'")

(defvar remacs-socket-dir
  (format "%s/remacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid))
  "Directory that contains the remacs socket interface.")

(defvar remacs-file (expand-file-name remacs-server-name remacs-socket-dir)
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
  ;; (remacs-send-error "some error" proc)
  (remacs-notify "2 title" "2 body" 'remacs-notify-test-cb))


(provide 'remacs)
