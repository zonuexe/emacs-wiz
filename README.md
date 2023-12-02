# wiz for starting Emacs

A collection of macros to optimize init.el for starting Emacs.

## wiz.el

*wip*

## wiz-env.el

Optimize GUI Emacs startup overhead by importing environment variables during byte compilation.

> [!NOTE]
> This feature is a wrapper of [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).

``` emacs-lisp
(eval-when-compile (require 'wiz-env))
(wiz-envs "PATH" "SSH_AUTH_SOCK" "MANPATH" "GOPATH")
```

This expression expands as the Lisp code below:

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

