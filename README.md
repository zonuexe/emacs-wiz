# wiz for starting Emacs

A collection of macros that optimize startup performance by byte-compiling `init.el`.

## Installation

This package requires Emacs 29.1+.

### Use package.el

``` emacs-lisp
(package-vc-install "git@github.com:zonuexe/emacs-wiz.git" nil 'Git 'wiz)
```

### Use [Borg](https://github.com/emacscollective/borg)

```emacs-lisp
(borg-assimilate "wiz" "git@github.com:zonuexe/emacs-wiz.git")
```

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
> Other expansion examples can also be found in Emacs with `M-x shortdoc wiz`.

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

 * `:package`
   * `:package php-mode`: When specifying the package name explicitly.
   * `:package (gnu nameless)`: When explicitly installing from GNU ELPA.
   * `:package t`: If the feature name and package name are the same, just pass `t`.
 * `:load-if-exists string`
 * `:load string`
 * `:init function`
 * `:config function`
 * `:hook-names (list symbol)`
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

## wiz-key.el

It's just a thin wrapper around `define-key`.

```emacs-lisp
(eval-when-compile (require 'wiz-key))

(wiz-keys (("[" . (smartchr "[`!!']" "array(`!!')" "[[`!!']]"))
           ("]" . (smartchr "array " "]" "]]"))
           ("&" . (smartchr "&" "&& "))
           ("\\" . (smartchr "\\" "\\PHPStan\\dumpType(`!!');" "\\\\"))
           ("¥" . (smartchr "\\" "\\PHPStan\\dumpType(`!!');" "\\\\"))
           ("|" . (smartchr "|" "|| " ))
           ("." . (smartchr
                   (my-php-smartchr-dot "->" "." ". ")
                   (my-php-smartchr-dot ". " ".." "..")
                   "..."))
           ("^" . (smartchr "^" "fn() => " "function () {`!!'}"))
           ("@" . (smartchr "@" "$this->"))
           ("~" . (smartchr "~" "phpstan-"))
           ("C-c C-c" . 'psysh-eval-region)
           ("<f6>" . phpunit-current-project)
           ("C-c C--" . php-current-class)
           ("C-c C-=" . 'php-current-namespace))
          :map php-mode-map)
```

### vs `bind-keys`

`bind-keys` flexibly accepts multiple keymaps, while `wiz-keys` only binds to a single map.

```emacs-lisp
(bind-keys :map paredit-mode-map
	       ("C-<right>" . right-word)
	       ("C-<left>"  . left-word))

(wiz-keys (("C-<right>" . right-word)
           ("C-<left>"  . left-word))
          :map paredit-mode-map)
```

 * Multiple keys must be grouped together in parentheses as a single alist.
 * `:map` keyword must be written after alist.
 * Other keywords are not implemented because I don't use them, not because of policy.
   * If you want to use them please send Pull Requests.

## wiz-pkgs.el

This feature provides a typical workflow for installing a package if it is not installed.

It is designed for wiz's `:package` keyword, but can also be used as a standalone feature.

For example:

```emacs-lisp
(wiz-pkgs 'package-el 'php-mode) ;; Install from any ELPA using package-install
(wiz-pkgs 'nongnu 'php-mode)     ;; Install from NonGNU ELPA using package-install
```

The handler should install the package immediately when called.

When called from the wiz macro, it installs the package when byte-compiling `init.el`, so it does nothing at runtime.  Instead, the handler should return an S-expression that is evaluated at runtime.  If it does nothing at runtime, it simply returns `nil`.

> [!NOTE]
> Handlers other than package.el are not implemented.
> I'm not a strait user, so if anyone wants to use it, please send me Pull Request.
