;;; Time-stamp: <2013-03-02 14:04:11 ryan>

(require 'graphviz-dot-mode)

(add-hook 'graphviz-dot-mode-hook
          (lambda ()
            "DOCSTRING"
            (setq graphviz-dot-indent-width 4)
            (setq comment-start "/* ")
            (setq comment-end " */")
            (inhibit-autopair)))

(provide 'init-dot)
