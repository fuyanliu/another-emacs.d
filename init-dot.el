;;; Time-stamp: <2012-11-08 19:45:37 ryan>

(require 'graphviz-dot-mode)

(add-hook 'graphviz-dot-mode-hook
          (lambda ()
            "DOCSTRING"
            (setq graphviz-dot-indent-width 4)
            (setq comment-start "/* ")
            (setq comment-end " */")))

(provide 'init-dot)
