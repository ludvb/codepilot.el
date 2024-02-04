;;; codepilot-ollama.el --- ollama backend for codepilot -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'codepilot)
(require 'codepilot-common)
(require 'codepilot-logging)

(defgroup codepilot-ollama nil
  "Group for ollama related variables."
  :group 'codepilot)

(defcustom codepilot-ollama-hostname "localhost"
  "Hostname of the ollama server."
  :type 'string
  :group 'codepilot-ollama)

(defcustom codepilot-ollama-port 11434
  "Port of the ollama server."
  :type 'integer
  :group 'codepilot-ollama)

(defcustom codepilot-ollama-model "deepseek-coder:6.7b-base"
  "The model to use for completion."
  :type 'string
  :group 'codepilot-ollama)

(defcustom codepilot-ollama-stream t
  "Whether to use a streaming response."
  :type 'boolean
  :group 'codepilot-ollama)

(defcustom codepilot-ollama-extra-args nil
  "Alist of additional arguments to pass to the llama.cpp server."
  :type '(repeat (cons symbol sexp))
  :group 'codepilot-llamacpp)

(defun codepilot-ollama--get-endpoint ()
  (concat "http://"
          codepilot-ollama-hostname
          ":"
          (number-to-string codepilot-ollama-port)
          "/api/generate"))

(cl-defun codepilot-ollama--request-data (prompt &key (stream t))
  "Generates the data to send to the ollama server."
  (json-encode
   (append `((model . ,codepilot-ollama-model)
             (prompt . ,prompt)
             (stream . ,(if stream t :json-false)))
           codepilot-ollama-extra-args)))

(defvar-local codepilot-ollama-req-process nil)

(defun codepilot-ollama--get-req-process ()
  "Returns the request process if it is alive and nil otherwise."
  (when codepilot-ollama-req-process
    (if (process-live-p codepilot-ollama-req-process)
        codepilot-ollama-req-process
      (setq codepilot-ollama-req-process nil))))

(cl-defmethod codepilot--request-completion
  ((backend (eql 'ollama)) ctx-before ctx-after callback)
  (codepilot--cancel-completion 'ollama)
  (let* ((prompt (funcall codepilot-prompt-fun :ctx-before ctx-before :ctx-after ctx-after))
         (data (codepilot-ollama--request-data prompt :stream codepilot-ollama-stream))
         (command (append codepilot-curl-command
                          (list (codepilot-ollama--get-endpoint) (concat "-d" data)))))
    (codepilot-log 'info "Requesting ollama completion...")
    (codepilot-log 'debug "Running: %s" (mapconcat #'shell-quote-argument command " "))
    (let ((process (apply #'start-process "codepilot-ollama" nil command)))
      (setq codepilot-ollama-req-process process)
      (set-process-filter
       process
       (lambda (process data)
         (codepilot-log 'debug "Got: %s" data)
         (mapc (lambda (data)
                 (condition-case nil
                     (let* ((response (cdr (assoc 'response (json-read-from-string data))))
                            (result (funcall codepilot-postprocess-fun :response response)))
                       (codepilot-log 'info "Response: %s" result)
                       (funcall callback result))
                   (error nil)))
               (split-string data "\n" t))))
      (set-process-sentinel
       process
       (lambda (process event)
         (unless (process-live-p process))
           (setq codepilot-ollama-req-process nil)
           (codepilot-log 'info "Ollama request exited.")
           (funcall callback nil :done t))))))

(cl-defmethod codepilot--cancel-completion ((backend (eql 'ollama)))
  (when (codepilot-ollama--get-req-process)
    (delete-process (codepilot-ollama--get-req-process))))

(provide 'codepilot-ollama)
;;; codepilot-ollama.el ends here
