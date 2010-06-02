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

(require 'cl)

(defvar remacs-name "remacs")
(defvar remacs-socket-dir
  (format "%s/remacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid)))
(defvar remacs-file (expand-file-name remacs-name remacs-socket-dir))
(defvar remacs-buffer "*remacs*")
(defvar remacs-log nil)

(defvar remacs-process nil)
(defvar remacs-clients nil)

(define-minor-mode remacs-mode
  :global t
  :group 'remacs
  :version "22.1"
  (remacs-start (not remacs-mode)))

(defun remacs-start (&optional leave-dead)
  (interactive "P")
  (when (or (not remacs-clients)
            ;; Ask the user before deleting existing clients---except
            ;; when we can't get user input, which may happen when
            ;; doing emacsclient --eval "(kill-emacs)" in daemon mode.
            (if (and (daemonp)
                     (null (cdr (frame-list)))
                     (eq (selected-frame) terminal-frame))
                leave-dead
              (yes-or-no-p
               "The current remacs still has clients; delete them? ")))
    (let ()
      (when remacs-process
        ;; kill it dead!
        (ignore-errors (delete-process remacs-process)))
      ;; Delete the socket files made by previous server invocations.
      (if (not (eq t (remacs-running-p remacs-name)))
          ;; Remove any leftover socket or authentication file
          (ignore-errors (delete-file remacs-file))
        (setq remacs-mode nil) ;; already set by the minor mode code
        (display-warning
         'remacs
         (concat "Unable to start remacs.\n"
                 (format "There is an existing remacs server, named %S.\n"
                         remacs-name)
                 "To start remacs in this Emacs process, stop the existing
remacs or call `M-x remacs-force-delete' to forcibly disconnect it.")
         :warning)
        (setq leave-dead t))
      ;; If this Emacs already had a server, clear out associated status.
      (while remacs-clients
        (remacs-delete-client (car remacs-clients)))
      ;; Now any previous server is properly stopped.
      (if leave-dead
          (progn
            (unless (eq t leave-dead) (remacs-log (message "Remacs stopped")))
            (setq remacs-process nil))
        ;; Make sure there is a safe directory in which to place the socket.
        (remacs-ensure-safe-dir remacs-socket-dir)
        (when remacs-process
          (remacs-log (message "Restarting remacs")))
        (letf (((default-file-modes) ?\700))
          (add-hook 'suspend-tty-functions 'remacs-handle-suspend-tty)
          (add-hook 'delete-frame-functions 'remacs-handle-delete-frame)
          (add-hook 'kill-emacs-query-functions
                    'remacs-kill-emacs-query-function)
          (add-hook 'kill-emacs-hook (lambda () (remacs-mode -1)))
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
                             :service remacs-file)))
          (unless remacs-process (error "Could not start remacs process"))
          (process-put remacs-process :remacs-file remacs-file))))))

(defun remacs-sentinel (proc msg)
  (let ((status (process-status proc)))
    ;; If this is a new client process, set the query-on-exit flag to nil
    ;; for this process (it isn't inherited from the server process).
    (when (and (eq status 'open)
               (process-query-on-exit-flag proc))
      (set-process-query-on-exit-flag proc nil))
    ;; Delete the associated connection file, if applicable.
    ;; Although there's no 100% guarantee that the file is owned by the
    ;; running Emacs instance, remacs-start uses remacs-running-p to check
    ;; for possible servers before doing anything, so it *should* be ours.
    (and (process-contact proc :server)
         (eq (process-status proc) 'closed)
         (ignore-errors (delete-file (process-get proc :remacs-file))))
    (remacs-log (format "Status changed to %s: %s" status msg) proc)
    (when (or (eq status 'exit) (eq status 'signal) (eq status 'closed)
              (eq status 'failed) (eq status nil))
      (remacs-delete-client proc))))

(defun* remacs-process-filter (proc string)
  (remacs-log (concat "Received " string) proc)
  (let ((prev (process-get proc 'previous-string)))
    (when prev
      (setq string (concat prev string))
      (process-put proc 'previous-string nil)))
  ;;(condition-case err
      (progn
        (add-to-list 'remacs-clients proc)
        (if (not (string-match "\n" string))
            ;; Save for later any partial line that remains.
            (when (> (length string) 0)
              (process-put proc 'previous-string string))

          (let ((use-current-frame nil)
                (request (substring string 0 (match-beginning 0)))
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
                  (mapcar 'remacs-unquote-arg (split-string request " " t)))
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
                  (push (lambda () (remacs-eval-and-print expr proc))
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
                    (if display (remacs-select-display display)))
                   ((eq tty-name 'window-system)
                    (remacs-create-window-system-frame display proc))
                   ;; When resuming on a tty, tty-name is nil.
                   (tty-name
                    (remacs-create-tty-frame tty-name tty-type proc))))

            (process-put
             proc 'continuation
             (lexical-let ((proc proc)
                           (files files)
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
                     (remacs-execute proc commands frame tty-name))))))

            (when (or frame files)
              (remacs-goto-toplevel proc))

            (remacs-execute-continuation proc))))
    ;; condition-case
    ;;(error (remacs-return-error proc err)))
      )

(defun remacs-goto-toplevel (proc)
  (condition-case nil
      ;; If we're running isearch, we must abort it to allow Emacs to
      ;; display the buffer and switch to it.
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (bound-and-true-p isearch-mode)
            (isearch-cancel))))
    ;; Signaled by isearch-cancel.
    (quit (message nil)))
  (when (> (recursion-depth) 0)
    ;; We're inside a minibuffer already, so if the emacs-client is trying
    ;; to open a frame on a new display, we might end up with an unusable
    ;; frame because input from that display will be blocked (until exiting
    ;; the minibuffer).  Better exit this minibuffer right away.
    ;; Similarly with recursive-edits such as the splash screen.
    (run-with-timer 0 nil (lexical-let ((proc proc))
                            (lambda () (remacs-execute-continuation proc))))
    (top-level)))

(defun remacs-execute-continuation (proc)
  (let ((continuation (process-get proc 'continuation)))
    (process-put proc 'continuation nil)
    (if continuation (ignore-errors (funcall continuation)))))

(defun remacs-execute (proc commands frame tty-name)
  (with-local-quit
    (condition-case err
        (mapc 'funcall (nreverse commands))

      (cond
       ((or isearch-mode (minibufferp))
        nil)
       (frame
        (message "%s" (substitute-command-keys
                       "When done with this frame, type \\[delete-frame]"))))
      (when (and frame (null tty-name))
        (remacs-unselect-display frame)))
    (error (remacs-return-error proc err))))

(defun remacs-delete-client (proc &optional noframe)
  (remacs-log (concat "remacs-delete-client" (if noframe " noframe")) proc)
  (remacs-log (format "remacs-clients %s" remacs-clients))
  ;; Force a new lookup of client (prevents infinite recursion).
  (when (memq proc remacs-clients)
    ;; Delete the client's frames.
    (unless noframe
      (dolist (frame (frame-list))
        (when (and (frame-live-p frame)
                   (equal proc (frame-parameter frame 'remacs-client)))
          ;; Prevent `remacs-handle-delete-frame' from calling us
          ;; recursively.
          (set-frame-parameter frame 'remacs-client nil)
          (delete-frame frame))))

    (setq remacs-clients (delq proc remacs-clients))

    ;; Delete the client's tty.
    (let ((terminal (process-get proc 'terminal)))
      ;; Only delete the terminal if it is non-nil.
      (when (and terminal (eq (terminal-live-p terminal) t))
        (delete-terminal terminal)))

    ;; Delete the client's process.
    (if (eq (process-status proc) 'open)
        (delete-process proc))

    (remacs-log "Deleted" proc)))

(defun remacs-handle-delete-frame (frame)
  (let ((proc (frame-parameter frame 'remacs-client)))
    (when (and (frame-live-p frame)
               proc
               ;; See if this is the last frame for this client.
               (>= 1 (let ((frame-num 0))
                       (dolist (f (frame-list))
                         (when (eq proc (frame-parameter f 'remacs-client))
                           (setq frame-num (1+ frame-num))))
                       frame-num)))
      (remacs-log (format "remac-handle-delete-frame, frame %s" frame) proc)
      ;; Let delete-frame delete the frame later.
      (remacs-delete-client proc 'noframe))))

(defun remacs-handle-suspend-tty (terminal)
  (dolist (proc (remacs-clients-with 'terminal terminal))
    (remacs-log (format "remacs-handle-suspend-tty, terminal %s" terminal) proc)
    (condition-case err
        (remacs-send-string proc "-suspend \n")
      (file-error                       ;The pipe/socket was closed.
       (ignore-errors (remacs-delete-client proc))))))

(defun remacs-create-tty-frame (tty type proc)
  (unless tty
    (error "Invalid terminal device"))
  (unless type
    (error "Invalid terminal type"))
  (add-to-list 'frame-inherited-parameters 'remacs-client)
  (let ((frame
         (remacs-with-environment (process-get proc 'env)
             '("LANG" "LC_CTYPE" "LC_ALL"
               ;; For tgetent(3); list according to ncurses(3).
               "BAUDRATE" "COLUMNS" "ESCDELAY" "HOME" "LINES"
               "NCURSES_ASSUMED_COLORS" "NCURSES_NO_PADDING"
               "NCURSES_NO_SETBUF" "TERM" "TERMCAP" "TERMINFO"
               "TERMINFO_DIRS" "TERMPATH"
               ;; rxvt wants these
               "COLORFGBG" "COLORTERM")
           (make-frame `((window-system . nil)
                         (tty . ,tty)
                         (tty-type . ,type)
                         (client . ,proc)
                         ;; This is a leftover from an earlier
                         ;; attempt at making it possible for process
                         ;; run in the server process to use the
                         ;; environment of the client process.
                         ;; It has no effect now and to make it work
                         ;; we'd need to decide how to make
                         ;; process-environment interact with client
                         ;; envvars, and then to change the
                         ;; C functions `child_setup' and
                         ;; `getenv_internal' accordingly.
                         (environment . ,(process-get proc 'env)))))))

    ;; ttys don't use the `display' parameter, but callproc.c does to set
    ;; the DISPLAY environment on subprocesses.
    (set-frame-parameter frame 'display
                         (getenv-internal "DISPLAY" (process-get proc 'env)))
    (select-frame frame)
    (process-put proc 'frame frame)
    (process-put proc 'terminal (frame-terminal frame))

    ;; Display *scratch* by default.
    (switch-to-buffer (get-buffer-create "*scratch*") 'norecord)

    ;; Reply with our pid.
    (remacs-send-string proc (concat "-emacs-pid "
                                     (number-to-string (emacs-pid)) "\n"))
    frame))

(defmacro remacs-with-environment (env vars &rest body)
  (declare (indent 2))
  (let ((var (make-symbol "var"))
        (value (make-symbol "value")))
    `(let ((process-environment process-environment))
       (dolist (,var ,vars)
         (let ((,value (getenv-internal ,var ,env)))
           (push (if (stringp ,value)
                     (concat ,var "=" ,value)
                   ,var)
                 process-environment)))
       (progn ,@body))))

(defun remacs-clients-with (property value)
  (let (result)
    (dolist (proc remacs-clients result)
      (when (equal value (process-get proc property))
        (push proc result)))))

(defun remacs-running-p (&optional name)
  (unless name (setq name remacs-name))
  (condition-case nil
      (progn
        (delete-process
         (make-network-process
          :name "remacs-client-test" :family 'local :server nil :noquery t
          :service remacs-file))
        t)
    (file-error nil)))

(defun remacs-ensure-safe-dir (dir)
  (setq dir (directory-file-name dir))
  (let ((attrs (file-attributes dir)))
    (unless attrs
      (letf (((default-file-modes) ?\700)) (make-directory dir t))
      (setq attrs (file-attributes dir)))
    ;; Check that it's safe for use.
    (unless (and (eq t (car attrs)) (eql (nth 2 attrs) (user-uid))
                 (or (eq system-type 'windows-nt)
                     (zerop (logand ?\077 (file-modes dir)))))
      (error "The directory %s is unsafe" dir))))

(defun remacs-kill-emacs-query-function ()
  (when remacs-clients
    (yes-or-no-p "This Emacs session has clients; exit anyway? ")))

(defun remacs-return-error (proc err)
  (ignore-errors
    (remacs-send-string
     proc (concat "-error " (remacs-quote-arg
                             (error-message-string err))))
    (remacs-log (error-message-string err) proc)
    (delete-process proc)))

(defun remacs-send-string (proc string)
  (remacs-log (concat "Sent " string) proc)
  (process-send-string proc string))

(defun remacs-unquote-arg (arg)
  (replace-regexp-in-string
   "&." (lambda (s)
          (case (aref s 1)
            (?& "&")
            (?- "-")
            (?n "\n")
            (t " ")))
   arg t t))

(defun remacs-quote-arg (arg)
  (replace-regexp-in-string
   "[-&\n ]" (lambda (s)
               (case (aref s 0)
                 (?& "&&")
                 (?- "&-")
                 (?\n "&n")
                 (?\s "&_")))
   arg t t))

(defun remacs-log (string &optional client)
  (when remacs-log
    (with-current-buffer (get-buffer-create remacs-buffer)
      (goto-char (point-max))
      (insert (current-time-string)
              (cond
               ((null client) " ")
               ((listp client) (format " %s: " (car client)))
               (t (format " %s: " client)))
              string)
      (or (bolp) (newline)))))

(defun remacs-process-log(server client msg)
  (remacs-log msg client))

(defun remacs-test ()
  (interactive)
  (setq remacs-log t)
  (toggle-debug-on-error)
  (remacs-start)
  (get-buffer-create remacs-buffer)
  (switch-to-buffer remacs-buffer)
)