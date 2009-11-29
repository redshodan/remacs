(require 'cl)
(require 'server)

(defun server-start-e (&optional leave-dead)
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

  (add-hook 'suspend-tty-functions 'server-handle-suspend-tty)
  (add-hook 'delete-frame-functions 'server-handle-delete-frame)
  (add-hook 'kill-buffer-query-functions
            'server-kill-buffer-query-function)
  (add-hook 'kill-emacs-query-functions
            'server-kill-emacs-query-function)
  (add-hook 'kill-emacs-hook
            (lambda () (server-mode -1))) ;Cleanup upon exit.
  (setq eserver-process
        (start-process "remacs" "*remacs*" "remacs" "--server"))
  (unless eserver-process (error "Could not start remacs server process"))
  (set-process-filter eserver-process 'eserver-process-filter)
  (with-current-buffer "*remacs*"
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'no-conversion)))

(defun* eserver-process-filter (proc string)
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

`-auth AUTH-STRING'
  Authenticate the client using the secret authentication string
  AUTH-STRING.

`-env NAME=VALUE'
  An environment variable on the client side.

`-dir DIRNAME'
  The current working directory of the client process.

`-current-frame'
  Forbid the creation of new frames.

`-nowait'
  Request that the next frame created should not be
  associated with this client.

`-display DISPLAY'
  Set the display name to open X frames on.

`-position LINE[:COLUMN]'
  Go to the given line and column number
  in the next file opened.

`-file FILENAME'
  Load the given file in the current frame.

`-eval EXPR'
  Evaluate EXPR as a Lisp expression and return the
  result in -print commands.

`-window-system'
  Open a new X frame.

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

`-window-system-unsupported'
  Signals that the server does not support creating X frames;
  the client must try again with a tty frame.

`-print STRING'
  Print STRING on stdout.  Used to send values
  returned by -eval.

`-error DESCRIPTION'
  Signal an error (but continue processing).

`-suspend'
  Suspend this terminal, i.e., stop the client process.
  Sent when the user presses C-z."
  (server-log (concat "Received " string) proc)
  ;; First things first: let's check the authentication
  ;; (unless (process-get proc :authenticated)
  ;;   (if (and (string-match "-auth \\([!-~]+\\)\n?" string)
  ;;            (equal (match-string 1 string) (process-get proc :auth-key)))
  ;;       (progn
  ;;         (setq string (substring string (match-end 0)))
  ;;         (process-put proc :authenticated t)
  ;;         (server-log "Authentication successful" proc))
  ;;     (server-log "Authentication failed" proc)
  ;;     (server-send-string
  ;;      proc (concat "-error " (server-quote-arg "Authentication failed")))
  ;;     (delete-process proc)
  ;;     ;; We return immediately
  ;;     (return-from server-process-filter)))
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

          ;; In earlier versions of server.el (where we used an `emacsserver'
          ;; process), there could be multiple lines.  Nowadays this is not
          ;; supported any more.
          (assert (eq (match-end 0) (length string)))
          (let ((request (substring string 0 (match-beginning 0)))
                (coding-system (and default-enable-multibyte-characters
                                    (or file-name-coding-system
                                        default-file-name-coding-system)))
                nowait ; t if emacsclient does not want to wait for us.
                frame ; The frame that was opened for the client (if any).
                display		     ; Open the frame on this display.
                dontkill       ; t if the client should not be killed.
                commands
                dir
                use-current-frame
                tty-name       ;nil, `window-system', or the tty name.
                tty-type             ;string.
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

               ;; -nowait:  Emacsclient won't wait for a result.
               ((equal "-nowait" arg) (setq nowait t))

               ;; -current-frame:  Don't create frames.
               ((equal "-current-frame" arg) (setq use-current-frame t))

               ;; -display DISPLAY:
               ;; Open X frames on the given display instead of the default.
               ((and (equal "-display" arg) command-line-args-left)
                (setq display (pop command-line-args-left))
                (if (zerop (length display)) (setq display nil)))

               ;; -window-system:  Open a new X frame.
               ((equal "-window-system" arg)
                (setq dontkill t)
                (setq tty-name 'window-system))

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
                  (server-log (format "New file: %s %s"
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
                (message "eserver msg: %s" (pop command-line-args-left)))

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
                 (with-current-buffer (get-buffer-create server-buffer)
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

(server-start-e)
(switch-to-buffer "*remacs*")