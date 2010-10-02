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
;;     <env>
;;     <dir>
;;   </setup>
;;   <eval>str</eval>
;;   <msg>str</msg>
;;   <notify id='12345' type='invoke|read'/>
;;

(require 'cl)
(require 'xml)

(defvar remacs-name "remacs")
(defvar remacs-socket-dir
  (format "%s/remacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid)))
(defvar remacs-file (expand-file-name remacs-name remacs-socket-dir))
(defvar remacs-buffer "*remacs*")
(defvar remacs-proto-buffer "*remacs-proto*")
(buffer-disable-undo (get-buffer-create remacs-proto-buffer))
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
  (add-to-list 'remacs-clients proc)
  (remacs-log (concat "Received " string) proc)
  (let ((prev (process-get proc 'previous-string)))
    (when prev
      (setq string (concat prev string))
      (process-put proc 'previous-string nil)))
  ;;(condition-case err
  (progn
    (if (not (string-match "\000" string))
        ;; Save for later any partial line that remains.
        (when (> (length string) 0)
          (process-put proc 'previous-string string))

      (let ((request (substring string 0 (match-beginning 0)))
            (coding-system (and default-enable-multibyte-characters
                                (or file-name-coding-system
                                    default-file-name-coding-system)))
            xml
            frame ; The frame that was opened for the client.
            commands
            dir
            tty-name       ; the tty name.
            tty-term       ; string.
            files
            command-line-args-left
            arg)
        ;; Remove this line from STRING.
        (setq string (substring string (match-end 0)))
        ;; Parse the request
        (with-current-buffer remacs-proto-buffer
          (insert request)
          (setq xml (xml-parse-region (point-min) (point-max)))
          (erase-buffer))
        (cond
         ;; <setup>
         ((eq (car (xml-node-name xml)) 'setup)
          (let ((tty
                 (xml-node-name (xml-get-children (xml-node-name xml) 'tty))))
            (setq tty-name (xml-get-attribute tty 'name)
                  tty-term (xml-get-attribute tty 'term))))
           
           ;; ;; -eval EXPR:  Evaluate a Lisp expression.
           ;; ((and (equal "-eval" arg)
           ;;       command-line-args-left)
           ;;  (lexical-let ((expr (pop command-line-args-left)))
           ;;    (if coding-system
           ;;        (setq expr (decode-coding-string expr coding-system)))
           ;;    (push (lambda () (remacs-eval-and-print expr proc))
           ;;          commands)))
           
           ;; ;; -env NAME=VALUE:  An environment variable.
           ;; ((and (equal "-env" arg) command-line-args-left)
           ;;  (let ((var (pop command-line-args-left)))
           ;;    ;; XXX Variables should be encoded as in getenv/setenv.
           ;;    (process-put proc 'env
           ;;                 (cons var (process-get proc 'env)))))
           
           ;; ;; -dir DIRNAME:  The cwd of the emacsclient process.
           ;; ((and (equal "-dir" arg) command-line-args-left)
           ;;  (setq dir (pop command-line-args-left))
           ;;  (if coding-system
           ;;      (setq dir (decode-coding-string dir coding-system)))
           ;;  (setq dir (command-line-normalize-file-name dir)))
           
           ;; ;; -msg MESSAGE:  Display a message
           ;; ((and (equal "-msg" arg) command-line-args-left)
           ;;  (message "remacs msg: %s" (pop command-line-args-left)))
           
           ;; ;; -notify-invoke ID:  Invoke a notification
           ;; ((and (equal "-notify-invoke" arg) command-line-args-left)
           ;;  (remacs-notify-invoke (pop command-line-args-left)))
           
         ;; Unknown command.
         (t (error "Unknown command: %s" arg)))
        
        (setq frame (remacs-create-tty-frame tty-name tty-term proc))
        
        (process-put
         proc 'continuation
         (lexical-let ((proc proc)
                       (files files)
                       (commands commands)
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
        (remacs-send-string proc "<suspend/>")
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
    (remacs-send-string proc (format "<emacs pid='%d'/>" (emacs-pid)))
    
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
     proc (format "<error>%s</error>" (error-message-string err)))
    (remacs-log (error-message-string err) proc)
    (delete-process proc)))

(defun remacs-send-string (proc string)
  (remacs-log (concat "Sent " string) proc)
  (process-send-string proc (concat string "\000")))

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

;;;
;;; Notification
;;;

(defvar remacs-notify-counter 0)
(defvar remacs-notify-alist '())

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
        (cons (cons key value) alist)))))

(defun remacs-notify (title body &optional cb)
  (setq remacs-notify-counter (+ 1 remacs-notify-counter))
  (set-alist 'remacs-notify-alist (symbol-value 'remacs-notify-counter)
             `(,(symbol-value 'remacs-notify-counter) title body cb))
  (let ((msg (format "<notify id='%d'><title>%s</title><body>%s</body></notify>"
                     remacs-notify-counter title body)))
    (dolist (proc remacs-clients)
      (remacs-send-string proc msg))))

(defun remacs-notify-invoke (args)
  (let ((id (cdr args))
        (notif (get-alist id remacs-notify-alist)))
    (if (not notif)
        (message (format "Failed to find notification: %s" id))
      (remacs-log (format "invoked %s" id)))))

;;;
;;; Test code
;;;

(defun remacs-test ()
  (interactive)
  (setq remacs-log t)
  (toggle-debug-on-error)
  (remacs-start)
  (get-buffer-create remacs-buffer)
  (switch-to-buffer remacs-buffer)
  )

(defun remacs-notify-test ()
  (interactive)
  (remacs-notify "1 title" "1 bod")
  (remacs-notify "2 title" "2 body"))
