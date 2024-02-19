;;; codepilot-ollama-server.el --- Utilities for starting ollama from emacs -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'url-parse)
(require 'codepilot-common)
(require 'codepilot-ollama)

(defgroup codepilot-ollama-server nil
  "Utilities for starting ollama from emacs."
  :prefix "codepilot-ollama-server-"
  :group 'codepilot)

(defcustom codepilot-ollama-server-command '("ollama" "serve")
  "Command to use for running the ollama server."
  :type 'string
  :group 'codepilot-ollama-server)

(defcustom codepilot-ollama-ping-interval 15
  "Ping interval for the ollama server in seconds."
  :type 'integer
  :group 'codepilot-ollama-server)

(defcustom codepilot-ollama-server-logging-buffer "*codepilot-log-ollama*"
  "Name of the ollama log buffer."
  :type 'string
  :group 'codepilot-ollama-server)

(defun codepilot-ollama--ping (&optional callback)
  "Pings the ollama server to keep it alive or check if it is running."
  (let ((command (append codepilot-curl-command
                         (list
                          (codepilot-ollama--get-endpoint)
                          (concat "-d" (codepilot-ollama--request-data "" :stream nil))
                          "-so/dev/null"
                          "-m2"
                          "-w%{http_code}"))))
    (codepilot-log 'debug "Running: %s" (mapconcat #'shell-quote-argument command " "))
    (let ((p (apply #'start-process "codepilot-ollama-ping" "codepilot-ollama-ping" command)))
      (set-process-sentinel
       p (lambda (process event)
           (when (eq (process-status process) 'exit)
             (let ((output (with-current-buffer (process-buffer process) (buffer-string))))
               (kill-buffer (process-buffer process))
               (when callback
                 (funcall callback (string-to-number output))))))))))

(defvar codepilot-ollama-process nil)
(defvar codepilot-ollama-process-ping-timer nil)

(defun codepilot-ollama--ping-callback (http-status)
  "Handle response from ping request"
  (cl-case http-status
    (0 ;; server is unresponsive
     (progn (codepilot-log 'info "Starting ollama server...")
            (when codepilot-ollama-process (delete-process codepilot-ollama-process))
            (setq codepilot-ollama-process
                  (apply #'start-process "codepilot-ollama"
                         codepilot-ollama-server-logging-buffer
                         codepilot-ollama-server-command))))
    (t (codepilot-log 'debug "Ping response: %d" http-status))))

(defun codepilot-ollama-start ()
  "Starts ollama server (unless already running)"
  (interactive)
  (if (not (and (member (url-host (url-generic-parse-url codepilot-ollama-address))
                        '("localhost" "127.0.0.1" "::1"))
                codepilot-ollama-server-command))
      (codepilot-log
       'warning
       (concat "Either `codepilot-ollama-address' is not a loopback address"
               " or `codepilot-ollama-server-command' is `nil'."
               " Won't start ollama server."))
    (unless codepilot-ollama-process-ping-timer
      (setq codepilot-ollama-process-ping-timer
            (run-with-timer
             0 codepilot-ollama-ping-interval
             'codepilot-ollama--ping 'codepilot-ollama--ping-callback)))))

(defun codepilot-ollama-stop ()
  "Stops the ollama server (unless started externally)."
  (interactive)
  (when codepilot-ollama-process-ping-timer
    (cancel-timer codepilot-ollama-process-ping-timer)
    (setq codepilot-ollama-process-ping-timer nil))
  (when codepilot-ollama-process
    (codepilot-log 'info "Stopping ollama server...")
    (delete-process codepilot-ollama-process)
    (setq codepilot-ollama-process nil)))

(defun codepilot-ollama-restart ()
  "Restarts the ollama server."
  (interactive)
  (codepilot-ollama-stop)
  (codepilot-ollama-start))

(provide 'codepilot-ollama-server)
;;; codepilot-ollama-server.el ends here
