<div align="center">

<img src="./assets/logo.svg" width="15%" />

<h1>codepilot.el</h1>

_AI-powered code completion for emacs_

</div>

Codepilot.el provides AI-based code completion for emacs in a similar way to [copilot.el](https://github.com/copilot-emacs/copilot.el) and GitHub Copilot but with open-source language models.

<div align="center">

<img src="./assets/demo.gif" width="768px" alt="demo.gif" />

</div>


## Getting started

### Example configuration

```elisp
(use-package codepilot
  :straight (:host github :repo "ludvb/codepilot.el" :files ("*.el") )
  :hook ((prog-mode . codepilot-mode)
         (text-mode . codepilot-mode)))
(add-hook 'codepilot-mode-hook
  (lambda ()
     (when codepilot-mode
       (define-key evil-insert-state-map (kbd "M-j") 'codepilot-accept-line)
       (define-key evil-insert-state-map (kbd "M-l") 'codepilot-accept-char)
       (define-key evil-insert-state-map (kbd "M-w") 'codepilot-accept-word)
       (define-key evil-insert-state-map (kbd "M-<return>") 'codepilot-accept-all)
       (define-key evil-insert-state-map (kbd "M-J") 'codepilot-complete-next)
       (define-key evil-insert-state-map (kbd "M-K") 'codepilot-complete-previous))))
```

### Backends

Codepilot.el requires one of the following backends, which needs to be installed separately:

<details>
<summary><b>llama.cpp</b></summary>

Requirements:

- [cURL](https://curl.se/)
- [llama.cpp](https://github.com/ggerganov/llama.cpp) or a wrapper, such as [llama-cpp-python](https://github.com/abetlen/llama-cpp-python)

Add the following to your config:
```elisp
(setq codepilot-backend 'llamacpp)
(setq codepilot-llamacpp-hostname "localhost")
(setq codepilot-llamacpp-port 8080)
```

The default prompt is designed for the [DeepSeek Coder base](https://deepseekcoder.github.io/) models, which can be found on [Hugging Face](https://huggingface.co/models?other=deepseek&sort=trending&search=base+gguf).
If you are using another model, you may need to customize `codepilot-prompt-fun` and `codepilot-postprocess-fun`.

The llama.cpp API server needs to be started manually.

</details>

<details>
<summary><b>Ollama</b></summary>

Requirements:

- [cURL](https://curl.se/)
- [Ollama](https://github.com/jmorganca/ollama)

Add the following to your config:
```elisp
(setq codepilot-backend 'ollama)
(setq codepilot-ollama-hostname "localhost")
(setq codepilot-ollama-port 11434)
```

By default, the Ollama backend uses the [`deepseek-coder:6.7b-base`](https://deepseekcoder.github.io/) model.
To use another model, customize `codepilot-ollama-model`.
You may also need to customize `codepilot-prompt-fun` and `codepilot-postprocess-fun` as needed.

The Ollama server can be started manually or automatically from emacs:
```elisp
(add-hook 'codepilot-mode-hook 'codepilot-ollama-start)
```

</details>


## Contributing

Contributions are always welcome!
Feel free to open an issue if you have found a bug or have a feature request, or submit a PR that implements a proposed change.
