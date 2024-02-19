;;; codepilot-llamacpp.el --- llama.cpp backend for codepilot -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'codepilot)
(require 'codepilot-common)
(require 'codepilot-logging)

(defgroup codepilot-llamacpp nil
  "Customization options for llama.cpp."
  :group 'codepilot)

(defcustom codepilot-llamacpp-address "http://localhost:8080"
  "Address to the llama.cpp API server."
  :type 'string
  :group 'codepilot-llamacpp)

(defcustom codepilot-llamacpp-stream t
  "Whether to use a streaming response."
  :type 'boolean
  :group 'codepilot-llamacpp)

(defcustom codepilot-llamacpp-max-tokens 2048
  "Max number of tokens that the server is allowed to send in a response. If
`codepilot-llamacpp-stream' is `t', then this can safely be set to a high value,
since codepilot will cancel the request after receiving
`codepilot-completion-max-length' characters."
  :type 'integer
  :group 'codepilot-stream)

(defcustom codepilot-llamacpp-extra-args nil
  "Alist of additional arguments to pass to the llama.cpp server."
  :type '(repeat (cons symbol sexp))
  :group 'codepilot-llamacpp)

(defun codepilot-llamacpp--get-endpoint ()
  (concat codepilot-llamacpp-address "/v1/completions"))

(cl-defun codepilot-llamacpp--request-data (prompt)
  "Generates the data to send to the llama.cpp server."
  (json-encode
   (append `((prompt . ,prompt)
             (stream . ,(if codepilot-llamacpp-stream t :json-false))
             (max_tokens . ,codepilot-llamacpp-max-tokens))
           codepilot-llamacpp-extra-args)))

(defvar-local codepilot-llamacpp-req-process nil)

(defun codepilot-llamacpp--get-req-process ()
  "Returns the request process if it is alive and nil otherwise."
  (when codepilot-llamacpp-req-process
    (if (process-live-p codepilot-llamacpp-req-process)
        codepilot-llamacpp-req-process
      (setq codepilot-llamacpp-req-process nil))))

(cl-defmethod codepilot--request-completion
  ((backend (eql 'llamacpp)) ctx-before ctx-after callback)
  (codepilot--cancel-completion 'llamacpp)
  (let* ((prompt (funcall codepilot-prompt-fun :ctx-before ctx-before :ctx-after ctx-after))
         (data (codepilot-llamacpp--request-data prompt))
         (command (append codepilot-curl-command
                          (list (codepilot-llamacpp--get-endpoint)
                                "-HAccept: application/json"
                                "-HContent-Type: application/json"
                                "-XPOST"
                                (concat "-d" data)))))
    (codepilot-log 'info "Requesting llama.cpp completion...")
    (codepilot-log 'debug "Running: %s" (mapconcat #'shell-quote-argument command " "))
    (let ((process (apply #'start-process "codepilot-llamacpp" "codepilot-llamacpp-req" command)))
      (setq codepilot-llamacpp-req-process process)
      (set-process-filter
       process
       (lambda (process data)
         (codepilot-log 'debug "Got: %s" data)
         (mapc (lambda (line)
                 ;; Response has format "data: {"choices":[{"text":<response>, ...}, ...], ...}"
                 (condition-case nil
                     (let* ((response (string-trim line))
                            (response (string-remove-prefix "data: " response))
                            ;; ^ remove prefix if exists
                            (response (json-read-from-string response))
                            (response (cdr (assoc 'choices response)))
                            ;; ^ get "choices" property
                            (response (aref response 0))
                            ;; ^ take first choice
                            (response (cdr (assoc 'text response)))
                            ;; ^ get "text" property
                            (result (funcall codepilot-postprocess-fun :response response)))
                       (codepilot-log 'info "Response: %s" result)
                       (funcall callback result))
                   (error nil)))
               (split-string data "\n" t))))
      (set-process-sentinel
       process
       (lambda (process event)
         (unless (process-live-p process))
         (setq codepilot-llamacpp-req-process nil)
         (codepilot-log 'info "Llama.cpp request exited.")
         (funcall callback nil :done t))))))

(cl-defmethod codepilot--cancel-completion ((backend (eql 'llamacpp)))
  (when (codepilot-llamacpp--get-req-process)
    (delete-process (codepilot-llamacpp--get-req-process))))

(provide 'codepilot-llamacpp)
;;; codepilot-llamacpp.el ends here
