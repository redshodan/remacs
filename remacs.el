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
;;; todo:
;;; - uniqueify:
;;;     server-clients, server-running-p
;;;

(require 'cl)
(require 'server)

(defvar remacs-name "remacs")
(defvar remacs-process nil)
(defvar remacs-socket-dir
  (format "%s/remacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid)))

(defconst remacs-buffer "*remacs*"
  "Buffer used internally by the remacs server.
One use is to log the I/O for debugging purposes (see `remacs-log'),
the other is to provide a current buffer in which the process filter can
safely let-bind buffer-local variables like `default-directory'.")

(defun remacs-start (&optional leave-dead)
  "Allow this Emacs process to be a server for client processes.
This starts a server communications subprocess through which
client \"editors\" can send your editing commands to this Emacs
job.  To use the server, set up the program `emacsclient' in the
Emacs distribution as your standard \"editor\".

Optional argument LEAVE-DEAD (interactively, a prefix arg) means just
kill any existing server communications subprocess.

If a server is already running, the server is not started.
To force-start a server, do \\[server-force-delete] and then
\\[server-start]."
  (interactive "P")
  (when (or (not server-clients)
	    ;; Ask the user before deleting existing clients---except
	    ;; when we can't get user input, which may happen when
	    ;; doing emacsclient --eval "(kill-emacs)" in daemon mode.
	    (if (and (daemonp)
		     (null (cdr (frame-list)))
		     (eq (selected-frame) terminal-frame))
		leave-dead
	      (yes-or-no-p
	       "The current server still has clients; delete them? ")))
    (let ((server-file (expand-file-name remacs-name remacs-socket-dir)))
      (when remacs-process
	;; kill it dead!
	(ignore-errors (delete-process remacs-process)))
      ;; Delete the socket files made by previous server invocations.
      (if (not (eq t (server-running-p remacs-name)))
	  ;; Remove any leftover socket or authentication file
	  (ignore-errors (delete-file server-file))
	(setq server-mode nil) ;; already set by the minor mode code
	(display-warning
	 'server
	 (concat "Unable to start remacs.\n"
		 (format "There is an existing remacs server, named %S.\n"
			 remacs-name)
		 "To start remacs in this Emacs process, stop the existing
remacs or call `M-x server-force-delete' to forcibly disconnect it.")
	 :warning)
	(setq leave-dead t))
      ;; If this Emacs already had a server, clear out associated status.
      (while server-clients
	(server-delete-client (car server-clients)))
      ;; Now any previous server is properly stopped.
      (if leave-dead
	  (progn
	    (unless (eq t leave-dead) (remacs-log (message "Remacs stopped")))
	    (setq remacs-process nil))
	;; Make sure there is a safe directory in which to place the socket.
	(server-ensure-safe-dir remacs-socket-dir)
	(when remacs-process
	  (remacs-log (message "Restarting remacs")))
	(letf (((default-file-modes) ?\700))
	  (add-hook 'suspend-tty-functions 'server-handle-suspend-tty)
	  (add-hook 'delete-frame-functions 'server-handle-delete-frame)
	  (add-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
	  (add-hook 'kill-emacs-query-functions 'server-kill-emacs-query-function)
	  (add-hook 'kill-emacs-hook (lambda () (server-mode -1)))
	  (setq remacs-process
		(apply #'make-network-process
		       :name remacs-name
		       :server t
		       :noquery t
		       :sentinel 'remacs-sentinel
		       :filter 'remacs-process-filter
               :log 'remacs-process-log
		       :coding 'raw-text-unix
               (list :family 'local
                     :service server-file
                     )))
      (remacs-log server-file)
	  (unless remacs-process (error "Could not start remacs process"))
	  (process-put remacs-process :server-file server-file))))))

(defun remacs-sentinel (proc msg)
  "The process sentinel for Emacs server connections."
  ;; If this is a new client process, set the query-on-exit flag to nil
  ;; for this process (it isn't inherited from the server process).
  (when (and (eq (process-status proc) 'open)
             (process-query-on-exit-flag proc))
    (set-process-query-on-exit-flag proc nil))
  ;; Delete the associated connection file, if applicable.
  ;; Although there's no 100% guarantee that the file is owned by the
  ;; running Emacs instance, server-start uses server-running-p to check
  ;; for possible servers before doing anything, so it *should* be ours.
  (and (process-contact proc :server)
       (eq (process-status proc) 'closed)
       (ignore-errors (delete-file (process-get proc :server-file))))
  (server-log (format "Status changed to %s: %s" (process-status proc) msg) proc)
  (server-delete-client proc))

(defun* remacs-process-filter (proc string)
  "Process a request from the server to edit some files.
PROC is the server process.  STRING consists of a sequence of
commands prefixed by a dash.  Some commands have arguments;
these are &-quoted and need to be decoded by `server-unquote-arg'.
The filter parses and executes these commands.

To illustrate the protocol, here is an example command that
emacsclient sends to create a new X frame (note that the whole
sequence is sent on a single line):

	-env HOME=/home/lorentey
	-env DISPLAY=:0.0
	... lots of other -env commands
	-display :0.0
	-window-system

The following commands are accepted by the server:

`-env NAME=VALUE'
  An environment variable on the client side.

`-dir DIRNAME'
  The current working directory of the client process.

`-position LINE[:COLUMN]'
  Go to the given line and column number
  in the next file opened.

`-file FILENAME'
  Load the given file in the current frame.

`-eval EXPR'
  Evaluate EXPR as a Lisp expression and return the
  result in -print commands.

`-tty DEVICENAME TYPE'
  Open a new tty frame at the client.

`-suspend'
  Suspend this tty frame.  The client sends this string in
  response to SIGTSTP and SIGTTOU.  The server must cease all I/O
  on this tty until it gets a -resume command.

`-resume'
  Resume this tty frame.  The client sends this string when it
  gets the SIGCONT signal and it is the foreground process on its
  controlling tty.

`-ignore COMMENT'
  Do nothing, but put the comment in the server log.
  Useful for debugging.


The following commands are accepted by the client:

`-emacs-pid PID'
  Describes the process id of the Emacs process;
  used to forward window change signals to it.

`-print STRING'
  Print STRING on stdout.  Used to send values
  returned by -eval.

`-error DESCRIPTION'
  Signal an error (but continue processing).

`-suspend'
  Suspend this terminal, i.e., stop the client process.
  Sent when the user presses C-z."
  (remacs-log (concat "Received " string) proc)
  (let ((prev (process-get proc 'previous-string)))
    (when prev
      (setq string (concat prev string))
      (process-put proc 'previous-string nil)))
  (condition-case err
      (progn
        (server-add-client proc)
        (if (not (string-match "\n" string))
            ;; Save for later any partial line that remains.
            (when (> (length string) 0)
              (process-put proc 'previous-string string))

          (let ((request (substring string 0 (match-beginning 0)))
                (coding-system (and default-enable-multibyte-characters
                                    (or file-name-coding-system
                                        default-file-name-coding-system)))
                frame ; The frame that was opened for the client.
                dontkill       ; t if the client should not be killed.
                commands
                dir
                tty-name       ; the tty name.
                tty-type       ; string.
                files
                filepos
                command-line-args-left
                arg)
            ;; Remove this line from STRING.
            (setq string (substring string (match-end 0)))
            (setq command-line-args-left
                  (mapcar 'server-unquote-arg (split-string request " " t)))
            (while (setq arg (pop command-line-args-left))
              (cond
               ;; -version CLIENT-VERSION: obsolete at birth.
               ((and (equal "-version" arg) command-line-args-left)
                (pop command-line-args-left))

               ;; -resume:  Resume a suspended tty frame.
               ((equal "-resume" arg)
                (lexical-let ((terminal (process-get proc 'terminal)))
                  (setq dontkill t)
                  (push (lambda ()
                          (when (eq (terminal-live-p terminal) t)
                            (resume-tty terminal)))
                        commands)))

               ;; -suspend:  Suspend the client's frame.  (In case we
               ;; get out of sync, and a C-z sends a SIGTSTP to
               ;; emacsclient.)
               ((equal "-suspend" arg)
                (lexical-let ((terminal (process-get proc 'terminal)))
                  (setq dontkill t)
                  (push (lambda ()
                          (when (eq (terminal-live-p terminal) t)
                            (suspend-tty terminal)))
                        commands)))

               ;; -ignore COMMENT:  Noop; useful for debugging emacsclient.
               ;; (The given comment appears in the server log.)
               ((and (equal "-ignore" arg) command-line-args-left
                     (setq dontkill t)
                     (pop command-line-args-left)))

               ;; -tty DEVICE-NAME TYPE:  Open a new tty frame at the client.
               ((and (equal "-tty" arg)
                     (cdr command-line-args-left))
                (setq tty-name (pop command-line-args-left)
                      tty-type (pop command-line-args-left)
                      dontkill (or dontkill
                                   (not use-current-frame))))

               ;; -position LINE[:COLUMN]:  Set point to the given
               ;;  position in the next file.
               ((and (equal "-position" arg)
                     command-line-args-left
                     (string-match "\\+\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?"
                                   (car command-line-args-left)))
                (setq arg (pop command-line-args-left))
                (setq filepos
                      (cons (string-to-number (match-string 1 arg))
                            (string-to-number (or (match-string 2 arg) "")))))

               ;; -file FILENAME:  Load the given file.
               ((and (equal "-file" arg)
                     command-line-args-left)
                (let ((file (pop command-line-args-left)))
                  (if coding-system
                      (setq file (decode-coding-string file coding-system)))
                  (setq file (expand-file-name file dir))
                  (push (cons file filepos) files)
                  (remacs-log (format "New file: %s %s"
                                      file (or filepos "")) proc))
                (setq filepos nil))

               ;; -eval EXPR:  Evaluate a Lisp expression.
               ((and (equal "-eval" arg)
                     command-line-args-left)
                (if use-current-frame
                    (setq use-current-frame 'always))
                (lexical-let ((expr (pop command-line-args-left)))
                  (if coding-system
                      (setq expr (decode-coding-string expr coding-system)))
                  (push (lambda () (server-eval-and-print expr proc))
                        commands)
                  (setq filepos nil)))

               ;; -env NAME=VALUE:  An environment variable.
               ((and (equal "-env" arg) command-line-args-left)
                (let ((var (pop command-line-args-left)))
                  ;; XXX Variables should be encoded as in getenv/setenv.
                  (process-put proc 'env
                               (cons var (process-get proc 'env)))))

               ;; -dir DIRNAME:  The cwd of the emacsclient process.
               ((and (equal "-dir" arg) command-line-args-left)
                (setq dir (pop command-line-args-left))
                (if coding-system
                    (setq dir (decode-coding-string dir coding-system)))
                (setq dir (command-line-normalize-file-name dir)))

               ;; -msg MESSAGE:  Display a message
               ((and (equal "-msg" arg) command-line-args-left)
                (message "remacs msg: %s" (pop command-line-args-left)))

               ;; Unknown command.
               (t (error "Unknown command: %s" arg))))

            (setq frame
                  (cond
                   ((and use-current-frame
                         (or (eq use-current-frame 'always)
                             ;; We can't use the Emacs daemon's
                             ;; terminal frame.
                             (not (and (daemonp)
                                       (null (cdr (frame-list)))
                                       (eq (selected-frame)
                                           terminal-frame)))))
                    (setq tty-name nil tty-type nil)
                    (if display (server-select-display display)))
                   ((eq tty-name 'window-system)
                    (server-create-window-system-frame display nowait proc))
                   ;; When resuming on a tty, tty-name is nil.
                   (tty-name
                    (server-create-tty-frame tty-name tty-type proc))))

            (process-put
             proc 'continuation
             (lexical-let ((proc proc)
                           (files files)
                           (nowait nowait)
                           (commands commands)
                           (dontkill dontkill)
                           (frame frame)
                           (dir dir)
                           (tty-name tty-name))
               (lambda ()
                 (with-current-buffer (get-buffer-create remacs-buffer)
                   ;; Use the same cwd as the emacsclient, if possible, so
                   ;; relative file names work correctly, even in `eval'.
                   (let ((default-directory
                           (if (and dir (file-directory-p dir))
                               dir default-directory)))
                     (server-execute proc files nowait commands
                                     dontkill frame tty-name))))))

            (when (or frame files)
              (server-goto-toplevel proc))

            (server-execute-continuation proc))))
    ;; condition-case
    (error (server-return-error proc err))))


(defvar remacs-log nil
  "If non-nil, log the inputs and outputs of remacs in the `remacs-buffer'.")

(defun remacs-log (string &optional client)
  "If `remacs-log' is non-nil, log STRING to `remacs-buffer'.
If CLIENT is non-nil, add a description of it to the logged message."
  (when remacs-log
    (with-current-buffer (get-buffer-create remacs-buffer)
      (goto-char (point-max))
      (insert (funcall server-log-time-function)
              (cond
               ((null client) " ")
               ((listp client) (format " %s: " (car client)))
               (t (format " %s: " client)))
              string)
      (or (bolp) (newline)))))
(fset 'server-log 'remacs-log)

(defun remacs-process-log(server client msg)
  (remacs-log msg client))

(defun remacs-test ()
  (interactive)
  (setq remacs-log t)
  (toggle-debug-on-error)
  (remacs-start)
  (get-buffer-create remacs-buffer)
  (switch-to-buffer remacs-buffer))
