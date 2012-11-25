(autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)

(require 'python-mode)

(setq auto-mode-alist
      (append '(("\\.py$" . python-mode)
		("SConstruct$" . python-mode)
		("SConscript$" . python-mode))
              auto-mode-alist))

(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))


;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
(eval-after-load 'python
  '(progn
     (require 'flymake-python-pyflakes)
     (require 'ipython)
     ))

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(provide 'init-python-mode)
