# 🧙 wiz for starting Emacs

A collection of macros that optimize startup performance by byte-compiling `init.el`.

These macros have **no runtime library** and compile to byte-compiler friendly code.

## 💾 Installation

This package requires Emacs 29.1+.

### Use package.el

``` emacs-lisp
(package-vc-install
 '(wiz :url "git@github.com:zonuexe/emacs-wiz.git"
       :main-file "wiz.el"))
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
  (setopt nyan-bar-length 16)
  :init
  (nyan-mode))
```

This expression is expanded into the following S-expression:

```emacs-lisp
(prog1 'nyan-mode
  (with-eval-after-load 'nyan-mode
    (setopt nyan-bar-length 16))
  (prog1 nil
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
  (add-to-list 'auto-mode-alist '("\\.stub" . php-mode-maybe))
  :config
  (setopt php-default-major-mode 'php-mode)
  (setopt php-manual-url 'ja)
  (setopt php-mode-coding-style 'psr2)
  (setopt php-mode-template-compatibility nil)
  (setopt php-imenu-generic-expression 'php-imenu-generic-expression-simple)
  (setopt php-project-auto-detect-etags-file t)
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
 * `:init ...exprs`
 * `:config ...exprs`
 * `:hook-names (repeat symbol)`
 * `:setup-hook (defun ...)`

> [!NOTE]
> `...exprs` can contain multiple S-expressions. If you write a single `(lambda () ...)` there, its contents will be expanded.
>
> Include one function definition in `(defun ...)`. wiz reads the function name from defun and transparently adds it to the hook.

### vs use-package, leaf

This package provides macros with a similar purpose to both, but wiz is much simpler.

 * The name specified for `wiz` is only the **feature** name, ***not the package name***.
 * ~~Must be set strictly as a property list.~~
   * `:config` and `:init` can now be written on multiple lines without needing to be enclosed in `lambda` like use-package.
 * The output code has no runtime library function calls.

### 💡 Tips

If the elements of the variable do not change dynamically, you can rewrite the code as follows and the code will be evaluated at compile time and further optimized by the byte compiler.

```emacs-lisp
;; Evaluated at runtime
(mapc (lambda (it)
        (add-hook (intern (concat (symbol-name it) "-hook"))
                  #'my/disable-trailing-mode-hook))
      my/disable-trailing-modes)

;; Evaluated at compile time and even more optimizer friendly
(wiz-map my/disable-trailing-modes
  (lambda (it)
    `(add-hook (quote ,(intern (concat (symbol-name it) "-hook")))
               #'my/disable-trailing-mode-hook)))
```

## wiz-env.el

Optimize GUI Emacs startup overhead by importing environment variables during byte compilation.

> [!NOTE]
> This feature is a wrapper of [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).

```emacs-lisp
(eval-when-compile (require 'wiz-env))
(wiz-env* "PATH" "SSH_AUTH_SOCK" "MANPATH" "GOPATH")
```

<details>
<summary>Expression Expanding into the Following Lisp Code:</summary>

```emacs-lisp
(prog1
    (list "PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "MANPATH" "GOROOT" "GOPATH")
  (setenv "PATH" "/opt/homebrew/bin:/opt/homebrew/sbin:/Users/megurine/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
  (setq exec-path
        (list "/opt/homebrew/bin/" "/opt/homebrew/sbin/" "/Users/megurine/local/bin/" "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/" exec-directory))
  (setenv "SSH_AUTH_SOCK" "/private/tmp/com.apple.launchd.hHAlJWPYt1/Listeners")
  (setenv "MANPATH" "/opt/homebrew/share/man:/usr/share/man:/usr/local/share/man:/opt/homebrew/share/man:")
  (setenv "GOPATH" "/Users/megurine/repo/go"))
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

```diff
-(bind-keys :map paredit-mode-map
-	       ("C-<right>" . right-word)
-	       ("C-<left>"  . left-word))

 (wiz-keys (("C-<right>" . right-word)
            ("C-<left>"  . left-word))
           :map paredit-mode-map)
```

 * Multiple keys must be grouped together in parentheses as a single alist.
 * `:map` keyword must be written after alist.
 * Other keywords are not implemented because I don't use them, not because of policy.
   * If you want to use them please send Pull Requests.

### Where should I write `wiz-keys`, in `:init` or `:config`?

If you are specifying `:map` for `wiz-keys`, it should be added in the `:config` section of the feature where that map is defined. If you are not specifying a `:map` (i.e., setting it to `global-map`), it is recommended to add it in the `:init` section.

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

## Copyright

This package is released under GPL-3.0.  See [`LICENSE`](LICENSE) file.

> Copyright (C) 2024  USAMI Kenta
>
> This program is free software; you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
>
> This program is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
> GNU General Public License for more details.
>
> You should have received a copy of the GNU General Public License
> along with this program.  If not, see <https://www.gnu.org/licenses/>.
