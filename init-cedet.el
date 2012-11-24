;; Maintainer: Yudi Shi
;; Time-stamp: <2012-11-05 14:48:22 ryan>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; remove emacs-inner cedet path and set owe path than load it.

(setq load-path
      (remove (concat "/usr/share/emacs/"
		      (substring emacs-version 0 -2)
		      "/lisp/cedet")
	      load-path))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/cedet"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/cedet/contrib"))

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
(require 'cedet-devel-load)
(semantic-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Use function one of follows to decide which components to active.
;;
;;(semantic-load-enable-minimum-features)
;;(semantic-load-enable-code-helpers)
(semantic-load-enable-gaudy-code-helpers)
;;(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Semantic Interativ Analysis, GCC part configuration.
(require 'semantic/ia)

;;; (require 'semantic-gcc)
(semantic-add-system-include "~/programs/MeeCoder/include" 'c++-mode)
(semantic-add-system-include "~/programs/larbin-2.6.3/src" 'c++-mode)
(semantic-add-system-include "~/projects/ob-dev/src" 'c++-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Jump between source code file and header file.
;;; (require 'eassist)
(require 'cedet-contrib-load)

(defun my-c-mode-common-hook ()
   (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
   (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;; 
(setq eassist-header-switches
      '(("h" . ("cpp" "cxx" "c++" "CC" "cc" "C" "c" "mm" "m"))
	("hh" . ("cc" "CC" "cpp" "cxx" "c++" "C"))
	("hpp" . ("cpp" "cxx" "c++" "cc" "CC" "C"))
	("hxx" . ("cxx" "cpp" "c++" "cc" "CC" "C"))
	("h++" . ("c++" "cpp" "cxx" "cc" "CC" "C"))
	("H" . ("C" "CC" "cc" "cpp" "cxx" "c++" "mm" "m"))
	("HH" . ("CC" "cc" "C" "cpp" "cxx" "c++"))
	("cpp" . ("hpp" "hxx" "h++" "HH" "hh" "H" "h"))
	("cxx" . ("hxx" "hpp" "h++" "HH" "hh" "H" "h"))
	("c++" . ("h++" "hpp" "hxx" "HH" "hh" "H" "h"))
	("CC" . ("HH" "hh" "hpp" "hxx" "h++" "H" "h"))
	("cc" . ("hh" "HH" "hpp" "hxx" "h++" "H" "h"))
	("C" . ("hpp" "hxx" "h++" "HH" "hh" "H" "h"))
	("c" . ("h"))
	("m" . ("h"))
	("mm" . ("h"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code Jump.
(global-set-key [f8] 'semantic-ia-fast-jump)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Visual Bookmarks.
;;; (require 'eieio)
(enable-visual-studio-bookmarks)

(setq senator-minor-mode-name "SN")
(setq semantic-imenu-auto-rebuild-directory-indexes nil)
(global-srecode-minor-mode 1)
(global-semantic-mru-bookmark-mode 1)

;;; (require 'semantic-decorate-include)

;; gcc setup
;;; (require 'semantic-gcc)

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local erlang-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

;;; (require 'eassist)

;; customisation of modes
(defun alexott/cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
;; (add-hook 'semantic-init-hooks 'alexott/cedet-hook)
(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'scheme-mode-hook 'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook 'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
 ;; (local-set-key "." 'semantic-complete-self-insert)
 ;; (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\M-o" 'eassist-switch-h-cpp)
  ;; (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\M-m" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

;; hooks, specific for semantic
(defun alexott/semantic-hook ()
;; (semantic-tag-folding-mode 1)
  (imenu-add-to-menubar "TAGS")
 )
(add-hook 'semantic-init-hooks 'alexott/semantic-hook)

;; (require 'semanticdb-global)
;; (semanticdb-enable-gnu-global-databases 'c-mode)
;; (semanticdb-enable-gnu-global-databases 'c++-mode)

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;; Enable Semantic
(semantic-mode 1)

;; Enable EDE (Project Management) features
(global-ede-mode 1)

(provide 'init-cedet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mycedet.el ends here

