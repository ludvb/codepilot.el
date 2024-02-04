;;; codepilot-logging.el --- Logging utilities for codepilot -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup codepilot-logging nil
  "Codepilot logging functions."
  :prefix "codepilot-logging-"
  :group 'codepilot)

(defcustom codepilot-logging-buffer "*codepilot-log*"
  "Buffer used by codepilot for logging."
  :type 'string
  :group 'codepilot-logging)

(defcustom codepilot-logging-level 'info
  "Logging level. If nil, suppress logging."
  :type '(choice (const :tag "Debug" 'debug)
                 (const :tag "Info" 'info)
                 (const :tag "Warn" 'warn)
                 (const :tag "Error" 'error))
  :group 'codepilot-logging)

(defun codepilot-logging-level-as-number (level)
  "Convert LEVEL to a number."
  (cond ((eq level 'debug) 0)
        ((eq level 'info) 1)
        ((eq level 'warn) 2)
        ((eq level 'error) 3)
        (t 4)))

(cl-defun codepilot-log (level message &rest args)
  "Write MESSAGE with ARGS to BUFFER. Only write if `codepilot-logging-level' is
equal to or greater than LEVEL."
  (when (<= (codepilot-logging-level-as-number level)
            (codepilot-logging-level-as-number codepilot-logging-level))
    (let ((formatted-message
            (apply #'format (string-join (list "[%s %s]" message "\n") " ")
                   (upcase (symbol-name level))
                   (format-time-string "%Y-%m-%d %H:%M:%S") args)))
      (with-current-buffer (get-buffer-create codepilot-logging-buffer)
        (goto-char (point-max))
        (insert formatted-message)))))

(provide 'codepilot-logging)
;;; codepilot-logging.el ends here
