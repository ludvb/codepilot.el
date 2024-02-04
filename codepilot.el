;;; codepilot.el --- LLM-based code completion -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup codepilot nil
  "LLM-based code completion"
  :prefix "codepilot-"
  :group 'tools)

(defcustom codepilot-completion-start-hook nil
  "Hook run when starting a completion request"
  :type 'hook
  :group 'codepilot)

(defcustom codepilot-completion-finish-hook nil
  "Hook run after a completion request is completed"
  :type 'hook
  :group 'codepilot)

(defcustom codepilot-completion-post-command-ignore-list
  '(codepilot-clear
    codepilot-complete
    codepilot-complete-next
    codepilot-complete-previous)
  "List of commands that do not trigger completion after being executed."
  :type '(repeat symbol)
  :group 'codepilot)

(defcustom codepilot-context-before 2000
  "The number of characters of context to include before the cursor"
  :type 'integer
  :group 'codepilot)

(defcustom codepilot-context-after 200
  "The number of characters of context to include after the cursor"
  :type 'integer
  :group 'codepilot)

(defcustom codepilot-completion-max-length 200
  "Maximum length of completion. Set to `nil' to allow completions of arbitrary
length."
  :type 'integer
  :group 'codepilot)

(defcustom codepilot-debounce-time 0.2
  "Debounce time for completion requests"
  :type 'float
  :group 'codepilot)

(defcustom codepilot-dont-stop t
  "Continue generating a longer completion as it is accepted"
  :type 'boolean
  :group 'codepilot)

(defcustom codepilot-backend 'llamacpp
  "Which backend to use for completions."
  :type 'symbol
  :group 'codepilot)

(defun codepilot--notevil-or-evilinsert ()
  (or (not (featurep 'evil))
      (evil-insert-state-p)))

(defcustom codepilot-display-p '(codepilot--notevil-or-evilinsert)
  "The conditions under which to display completions"
  :type '(repeat function)
  :group 'codepilot)

(cl-defgeneric codepilot--request-completion (backend ctx-before ctx-after callback)
  "Requests completion from the backend and calls CALLBACK with the result as it
arrives. CTX-BEFORE and CTX-AFTER are strings containing the text before and
after the cursor, respectively.")

(cl-defgeneric codepilot--cancel-completion (backend)
  "Cancels any pending completion request.")

(defvar-local codepilot--overlay nil
  "The code completion overlay.")

(defvar-local codepilot--completion-buffer '()
  "Completion buffer")

(defvar-local codepilot--completion-buffer-idx 0
  "The index of the current completion in `codepilot--completion-buffer'.")

(defun codepilot--clear-completion-buffer ()
  "Clears the completion buffer"
  (setq codepilot--completion-buffer '())
  (setq codepilot--completion-buffer-idx 0))

(defun codepilot--get-current-completion ()
  "Returns the current completion or an empty string if none is available."
  (if (< codepilot--completion-buffer-idx 0) ""
    (or (nth codepilot--completion-buffer-idx codepilot--completion-buffer) "")))

(defun codepilot--set-current-completion (value)
  "Sets the current completion to VALUE."
  (when (>= codepilot--completion-buffer-idx (length codepilot--completion-buffer))
    (setq codepilot--completion-buffer (append codepilot--completion-buffer '("")))
    (setq codepilot--completion-buffer-idx (1- (length codepilot--completion-buffer))))
  (when (< codepilot--completion-buffer-idx 0)
    (setq codepilot--completion-buffer (append '("") codepilot--completion-buffer))
    (setq codepilot--completion-buffer-idx 0))
  (setf (nth codepilot--completion-buffer-idx codepilot--completion-buffer) value))

(defun codepilot--set-completion-overlay (value)
  "Sets the completion overlay text to VALUE."
  (if (cl-every #'funcall codepilot-display-p)
      (progn
        (codepilot--set-current-completion value)
        (unless codepilot--overlay
          (setq codepilot--overlay (make-overlay 0 0)))
        (move-overlay codepilot--overlay (point) (point))
        (overlay-put
         codepilot--overlay
         'after-string
         (when (> (length value) 0)
           ;; If completion starts with a newline, we append a space to make
           ;; cursor positioning work, see
           ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html#index-cursor-_0028text-property_0029
           (when (= (aref value 0) ?\n)
             (setq value (concat " " value)))
           (put-text-property 0 1 'cursor t value)
           (propertize value 'face 'font-lock-comment-face))))
    (codepilot-clear)))

(defun codepilot--append-completion (value)
  "Appends VALUE to the current completion. If completion exceeds
`codepilot-completion-max-length', it is truncated and pending completion
requests are cancelled."
  (let ((value (concat (codepilot--get-current-completion) value)))
    (when (and codepilot-completion-max-length (> (length value) codepilot-completion-max-length))
      (setq value (substring value 0 codepilot-completion-max-length))
      (codepilot--cancel-completion codepilot-backend))
    (codepilot--set-completion-overlay value)))

(cl-defun codepilot--completion-callback (result &key (done nil))
  "Callback for completion result."
  (when result (codepilot--append-completion result))
  (when done
    (run-hook-with-args
     'codepilot-completion-finish-hook
     (codepilot--get-current-completion))))

(defun codepilot--request-completion-at-point ()
  "Requests completion at current point."
  (when (cl-every #'funcall codepilot-display-p)
    (let* ((cursor-point (point))
           (ctx-before (save-excursion
                         (condition-case nil
                             (backward-char codepilot-context-before)
                           (beginning-of-buffer))
                         (buffer-substring-no-properties (point) cursor-point)))
           (ctx-before (concat ctx-before (codepilot--get-current-completion)))
           (ctx-before (substring ctx-before (max 0 (- (length ctx-before) codepilot-context-before))))
           (ctx-after (save-excursion
                        (condition-case nil
                            (forward-char codepilot-context-after)
                          (end-of-buffer))
                        (buffer-substring-no-properties cursor-point (point)))))
      (run-hook-with-args 'codepilot-completion-start-hook ctx-before ctx-after)
      (codepilot--request-completion
       codepilot-backend ctx-before ctx-after #'codepilot--completion-callback))))

(defvar codepilot--request-debounce-timer nil
  "The timer used to debounce completion requests.")

(defun codepilot--request-completion-at-point-with-debounce ()
  "Requests completion at current point with debouncing."
  (when codepilot--request-debounce-timer
    (cancel-timer codepilot--request-debounce-timer))
  (setq codepilot--request-debounce-timer
        (run-at-time codepilot-debounce-time nil #'codepilot--request-completion-at-point)))

(defmacro codepilot--make-accept-fun (name movement)
  (let ((docstring
         (format "Accepts completion by stepping N times through the completion with `%s'."
                 (nth 1 movement))))
    `(defun ,name (&optional n)
       ,docstring
       (interactive "p")
       (pcase-let*
           ((completion (codepilot--get-current-completion))
            (`(,accepted-completion ,remaining-completion)
             (with-temp-buffer
               (insert completion)
               (goto-char (point-min))
               (funcall ,movement (or n 1))
               (list (buffer-substring-no-properties (point-min) (point))
                     (buffer-substring-no-properties (point) (point-max))))))
         (insert accepted-completion)
         (codepilot--clear-completion-buffer)
         (codepilot--set-completion-overlay remaining-completion)))))

(codepilot--make-accept-fun codepilot-accept-char #'forward-char)
(codepilot--make-accept-fun codepilot-accept-word #'forward-word)
(codepilot--make-accept-fun codepilot-accept-line #'forward-line)
(codepilot--make-accept-fun codepilot-accept-all #'end-of-buffer)

(defun codepilot-clear ()
  "Clears the current completion and cancels any pending completion request."
  (interactive)
  (codepilot--cancel-completion codepilot-backend)
  (codepilot--clear-completion-buffer)
  (when codepilot--overlay
    (delete-overlay codepilot--overlay)
    (setq codepilot--overlay nil)))

(defun codepilot-complete ()
  "Resets the completion buffer and generates a new completion."
  (interactive)
  (codepilot-clear)
  (codepilot--request-completion-at-point))

(defun codepilot-complete-next (&optional n)
  "Takes N steps forward through the completion buffer. If there are no further
completions, a new completion will be generated."
  (interactive "p")
  (setq codepilot--completion-buffer-idx (+ codepilot--completion-buffer-idx (or n 1)))
  (codepilot--set-completion-overlay (codepilot--get-current-completion))
  (codepilot--request-completion-at-point))

(defun codepilot-complete-previous (&optional n)
  "Takes N steps backward through the completion buffer. If there are no further
completions, a new completion will be generated."
  (interactive "p")
  (setq codepilot--completion-buffer-idx (- codepilot--completion-buffer-idx (or n 1)))
  (codepilot--set-completion-overlay (codepilot--get-current-completion))
  (codepilot--request-completion-at-point))

(defun codepilot--post-command ()
  "Post command hook for codepilot mode."
  (unless (member this-command codepilot-completion-post-command-ignore-list)
    (unless (string-prefix-p "codepilot-" (symbol-name this-command))
      ;; If user inserts the same character as the first character of the
      ;; completion, we update the completion accordingly without resetting
      (if (and (eq this-command 'self-insert-command)
               (> (length (codepilot--get-current-completion)) 0)
               (string= (this-command-keys)
                        (substring (codepilot--get-current-completion) 0 1)))
          (let ((new-completion (substring (codepilot--get-current-completion) 1)))
            (codepilot--set-completion-overlay new-completion))
        (codepilot-clear)))
    (codepilot--request-completion-at-point-with-debounce)))

(define-minor-mode codepilot-mode
  "Minor mode for code completion."
  :lighter " codepilot"
  :init-value nil
  (if codepilot-mode
        (add-hook 'post-command-hook #'codepilot--post-command nil 'local)
    (codepilot-clear)
    (remove-hook 'post-command-hook #'codepilot--post-command 'local)))

(provide 'codepilot)
;;; codepilot.el ends here
