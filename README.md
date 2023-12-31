# wiz for starting Emacs

A collection of macros that optimize startup performance by byte-compiling `init.el`.

## wiz.el

Shorthand macro for feature configuration for `init.el`. These are expanded to the minimum code by byte compilation.

```emacs-lisp
(eval-when-compile
  (require 'wiz))

(wiz nyan-mode
  :config
  (lambda ()
    (setopt nyan-bar-length 16))
  :init
  (lambda ()
    (nyan-mode)))
```

This expression is expanded into the following S-expression:

```emacs-lisp
(prog1 'nyan-mode
  (eval-after-load 'nyan-mode
    (lambda nil
      (setopt nyan-bar-length 16)))
  (progn
    (nyan-mode)))
```

> [!NOTE]
> You can check the macro expansion result with `M-x pp-macroexpand-last-sexp`.

### Config with mode hook

```emacs-lisp
(wiz php-mode
  :load-if-exists "~/repo/emacs/php-mode/lisp/php-mode-autoloads.el"
  :init
  (lambda ()
    (add-to-list 'auto-mode-alist '("\\.stub" . php-mode-maybe)))
  :config
  (lambda ()
    (setopt php-default-major-mode 'php-mode)
    (setopt php-manual-url 'ja)
    (setopt php-mode-coding-style 'psr2)
    (setopt php-mode-template-compatibility nil)
    (setopt php-imenu-generic-expression 'php-imenu-generic-expression-simple)
    (setopt php-project-auto-detect-etags-file t))
  :setup-hook
  (defun init-php-mode-setup ()
    (subword-mode 1)
    (add-hook 'hack-local-variables-hook 'php-ide-turn-on nil t)
    (setq show-trailing-whitespace t)
    (setq-local ac-disable-faces '(font-lock-comment-face font-lock-string-face php-string))
    (flycheck-mode t)
    (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
    (add-to-list 'flycheck-disabled-checkers 'php-phpcs)
    (php-format-auto-mode +1)
    (setq-local completion-at-point-functions
                (append (list #'php-complete-complete-function #'tempel-complete)
                        (list (cape-company-to-capf #'company-phpactor))
                        completion-at-point-functions))
    (when (eq 0 (buffer-size))
      (insert "<?php\n\n"))))
```

### Keywords

 * `:load-if-exists string`
 * `:load string`
 * `:init function`
 * `:config function`
 * `:hook-name symbol`
 * `:setup-hook function`

### vs use-package, leaf

This package provides macros with a similar purpose to both, but wiz is much simpler.

 * The name specified for `wiz` is only the **feature** name, ***not the package name***.
 * Must be set strictly as a property list.
   * In other words, write `:keyword` and `value` alternately.
 * The output code has no runtime library function calls.

## wiz-env.el

Optimize GUI Emacs startup overhead by importing environment variables during byte compilation.

> [!NOTE]
> This feature is a wrapper of [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).

```emacs-lisp
(eval-when-compile (require 'wiz-env))
(wiz-envs "PATH" "SSH_AUTH_SOCK" "MANPATH" "GOPATH")
```

<details>
<summary>Expression Expanding into the Following Lisp Code:</summary>

```emacs-lisp
(unless window-system
  (prog1
      (list "PATH" "TEST_SERVER" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "MANPATH" "GOROOT" "GOPATH")
    (setenv "PATH" "/opt/homebrew/bin:/opt/homebrew/sbin:/Users/megurine/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
    (setq exec-path
          (list "/opt/homebrew/bin/" "/opt/homebrew/sbin/" "/Users/megurine/local/bin/" "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/" exec-directory))
    (setenv "SSH_AUTH_SOCK" "/private/tmp/com.apple.launchd.hHAlJWPYt1/Listeners")
    (setenv "MANPATH" "/opt/homebrew/share/man:/usr/share/man:/usr/local/share/man:/opt/homebrew/share/man:")
    (setenv "GOPATH" "/Users/megurine/repo/go")))
```

</details>
