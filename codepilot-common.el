;;; codepilot-common.el --- Shared code utilities for codepilot.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defcustom codepilot-curl-command '("curl")
  "Command for calling cURL."
  :type 'string
  :group 'codepilot)

(cl-defun codepilot-ctx-before-prompt-fun (&key ctx-before &allow-other-keys)
  "Prompt function for non-finetuned, autoregressive models."
  ctx-before)

(defmacro codepilot-make-fim-prompt-fun (name begin-token hole-token end-token)
  "Construct a prompt function for fill-in-the-middle models."
  `(cl-defun ,name (&key ctx-before ctx-after &allow-other-keys)
     (concat ,begin-token ctx-before ,hole-token ctx-after ,end-token)))

(codepilot-make-fim-prompt-fun
 codepilot-deepseek-coder-fim-prompt-fun "<｜fim▁begin｜>" "<｜fim▁hole｜>" "<｜fim▁end｜>")

(defcustom codepilot-prompt-fun #'codepilot-deepseek-coder-fim-prompt-fun
  "Function for constructing prompts."
  :type 'function
  :group 'codepilot)

(cl-defun codepilot-identity-postprocess-fun (&key response &allow-other-keys)
  "The identity post-process function."
  response)

(defcustom codepilot-postprocess-fun #'codepilot-identity-postprocess-fun
  "The function to use for postprocessing responses."
  :type 'function
  :group 'codepilot)

(provide 'codepilot-common)
;;; codepilot-common.el ends here
