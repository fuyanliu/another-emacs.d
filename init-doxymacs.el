;;; Time-stamp: <2012-07-31 20:14:04 sydi>
;;; author: Ryan Shi

;; - Default key bindings are:
;;   - C-c d ? will look up documentation for the symbol under the point.
;;   - C-c d r will rescan your Doxygen tags file.
;;   - C-c d f will insert a Doxygen comment for the next function.
;;   - C-c d i will insert a Doxygen comment for the current file.
;;   - C-c d ; will insert a Doxygen comment for the current member.
;;   - C-c d m will insert a blank multiline Doxygen comment.
;;   - C-c d s will insert a blank singleline Doxygen comment.
;;   - C-c d @ will insert grouping comments around the current region.

(require 'doxymacs)

;;; {{{ doxymacs auto load.
(add-hook 'c-mode-common-hook 'doxymacs-mode)
;;; }}}

;;; {{{ doxymacs font-lock
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
;;; }}}

(provide 'init-doxymacs)
