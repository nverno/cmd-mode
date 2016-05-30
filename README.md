cmd-mode provides an extension to base bat-mode, providing
indentation, additional syntax and font-locking, completion-at-point
functions, basic interactive shell, and some addition user functions.

Coupled with company-cmd, there is completion and company help support
for most of the builtin commands.

# Setup

To use cmd-mode instead of bat-mode (assuming it is in your
load-path), you can add 

```lisp
(defalias 'bat-mode 'cmd-mode)
```

to your init file, or manually load it for specific file types.  
To enable company support,

```lisp
;; optional if in load-path
(require company-cmd)

;; add company-cmd to company-backends
(add-hook 'cmd-mode-hook
          #'(lambda ()
              (make-local-variable 'company-backends)
              (push 'company-cmd 'company-backends)))

;; or something like the following in the hook
(push '(company-capf :with company-bat) company-backends)
;; instead, which is similar to what I use in my init.
```

# example
![example](/test-cmd.png)
