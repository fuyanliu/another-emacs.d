;; Maintainer: Yudi Shi
;; Time-stamp: <2012-10-21 19:53:59 ryan>

;; here is global gtags settings.
(require 'gtags)

(define-key gtags-mode-map (kbd "C-c g f") 'gtags-find-tag)
(define-key gtags-mode-map (kbd "C-c g l") 'gtags-pop-stack)
(define-key gtags-select-mode-map (kbd "g") 'gtags-select-tag)
(define-key gtags-select-mode-map (kbd "SPC") 'gtags-select-tag)
(define-key gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
(define-key gtags-select-mode-map (kbd "l") 'gtags-pop-stack)
(define-key gtags-select-mode-map (kbd "q")
  (lambda () (interactive) (kill-buffer)))

(setq gtags-select-mode-hook nil)
(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (setq hl-line-face 'hl-line)
             (hl-line-mode 1)
             (local-set-key (kbd "g") 'gtags-select-tag)
             ))

(provide 'init-gtags)
