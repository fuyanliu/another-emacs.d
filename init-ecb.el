;; Maintainer: Yudi Shi
;; Time-stamp: <2012-10-21 22:57:42 ryan>

;;; Emacs Coder Browser.
(require 'ecb-autoloads)
(when (require 'ecb nil 'noerror)
  (setq stack-trace-on-error t)
  (setq ecb-tip-of-the-day nil)
  (setq ecb-auto-compatibility-check nil)
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
  (ecb-layout-define "my-cscope-layout" left nil
                     (ecb-set-methods-buffer)
                     (ecb-split-ver 0.4 t)
                     (other-window 1)
                     (ecb-set-history-buffer)
                     (ecb-split-ver 0.5 t)
                     (other-window 1)
                     (ecb-set-cscope-buffer))
  (defecb-window-dedicator-to-ecb-buffer 
      ecb-set-cscope-buffer " *ECB cscope-buf*"
      t (switch-to-buffer "*cscope*"))
  (setq ecb-layout-name "left6")
  ;; Disable buckets so that history buffer can display more entries
  (setq ecb-history-make-buckets 'never)
  ;;(ecb-activate)
  )

(provide 'init-ecb)
