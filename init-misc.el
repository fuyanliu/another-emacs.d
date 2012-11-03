;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile$")
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'find-file-hooks 'goto-address-prog-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq goto-address-mail-face 'link)

(column-number-mode 1)

; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

;Ctrl-X, u/l  to upper/lowercase regions without confirm
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(put 'scroll-left 'disabled nil)

;; no annoying beep on errors
(setq backup-directory-alist '(("." . "~/.backups")))

(put 'narrow-to-region 'disabled nil)

; from RobinH
;Time management
(display-time)
(defface sydi-display-time-face
  '((((type x w32 mac))
     ;; #060525 is the background colour of my default face.
     (:foreground "#7F00FF" :inherit bold))
    (((type tty))
     (:foreground "blue")))
  "Face used to display the time in the mode line."
  :group 'sydi-display-time)

;; This causes the current time in the mode line to be displayed in
;; `sydi-display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '("["
        (propertize (format-time-string "%a %h %d %R" (current-time))
 		    'face 'sydi-display-time-face)
        "]"))

(global-set-key [f8] 'calendar)
(global-set-key [f12] 'list-bookmarks)

;; M-x ct ENTER
(defun ct (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s/TAGS -e -R %s" dir-name (directory-file-name dir-name)))
  )

; @see http://xahlee.blogspot.com/2012/01/emacs-tip-hotkey-for-repeat-complex.html
(global-set-key [f2] 'repeat-complex-command)

;effective emacs item 3
;;; kill region if mark active, otherwise backwark kill a word.
(global-set-key "\C-w"
		(lambda () (interactive)
		  (if mark-active
		      (call-interactively 'kill-region)
		    (call-interactively 'backward-kill-word))))
;; (global-set-key "\C-x\C-k" 'kill-region)
;; (global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'tags-search)
;; (global-set-key "\C-x\C-n" 'find-file-other-frame) ;open new frame with a file

;;a no-op function to bind to if you want to set a keystroke to null
(defun void () "this is a no-op" (interactive))

;convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;vice versa
(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;show ascii table
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
           (setq i (+ i 1))
           (insert (format "%4d %c\n" i i))))
  (goto-char (point-min)))

;insert date into buffer
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

;;compute the length of the marked region
(defun region-length ()
  "length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

;; gdb
(global-set-key "\C-x\C-a\C-g" 'gud-run)
;;; WINDOW SPLITING
;; @see http://xahlee.org/emacs/effective_emacs.html
(global-set-key (kbd "M-3") 'split-window-horizontally);was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ;was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap
(defalias 'list-buffers 'ibuffer)
;KEYBOARD SECTION
;global keyb maps
(global-set-key "\C-xc" 'clipboard-kill-ring-save)
(global-set-key "\C-cc" 'copy-region-as-kill)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [\C-home] '(goto-char (point-min)))
(global-set-key [\C-end] 'end-of-buffer)
; (global-set-key [?\C-/] 'void) ;forward reference

;; @see http://www.emacswiki.org/emacs/BetterRegisters
;; This is used in the function below to make marked points visible
(defface register-marker-face '((t (:background "grey")))
      "Used to mark register positions in a buffer."
      :group 'faces)

;effective emacs item 7; no scrollbar, no menubar, no toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;effiective emacs item9
(defalias 'qrr 'query-replace-regexp)

;@see http://stackoverflow.com/questions/3509919/ \
;emacs-c-opening-corresponding-header-file
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(setq-default regex-tool-backend 'perl)

;shortcut 'ytx', if smex installed
(defun yank-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
    (progn
     ; my clipboard manager only intercept CLIPBOARD
      (shell-command-on-region (region-beginning) (region-end)
        (cond
         (*cygwin* "putclip")
         (*is-a-mac* "pbcopy")
         (t "xclip -selection clipboard")
         )
        )
      (message "Yanked region to clipboard!")
      (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(defun paste-from-x-clipboard()
  (interactive)
    (shell-command
        (cond
         (*cygwin* "getclip")
         (*is-a-mac* "pbpaste")
         (t "xclip -o")
         )
     1)
  )

(eval-after-load "speedbar" '(if (load "mwheel" t)
                               ;; Enable wheelmouse support by default
                               (cond (window-system
                                       (mwheel-install)))))

(track-closed-files-mode)

; @see http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)

; if emacs-nox, use C-@, else, use C-2;
(if t ;; window-system
 (progn
   (define-key global-map (kbd "C-2") 'er/expand-region)
   (define-key global-map (kbd "C-M-2") 'er/contract-region)
   )
 (progn
   (define-key global-map (kbd "C-@") 'er/expand-region)
   (define-key global-map (kbd "C-M-@") 'er/contract-region)
 )
)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; add time stamp if need.
(add-hook 'before-save-hook 'time-stamp)

;; xcscope load
(require 'xcscope)

(define-prefix-command 'ctl-z-map)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'ctl-z-map)

(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-`") (lambda () (interactive) (compile "make -j 10")))
(global-set-key (kbd "C-~") 'compile)
; http://tapoueh.org/emacs/switch-window.html
(require 'switch-window)

;;move-text stuff, move line up/down by pressing hotkey
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; move window
;; (require 'window-numbering)
;; (window-numbering-mode 1)

;; sig-quote
;(require 'sig-quote)

(defun find-file-as-root ()
  (interactive)
  (find-file
   (read-file-name
    "sudo: "
    "/sudo:root@localhost:/etc/")))

(global-set-key (kbd "C-x C-r") 'find-file-as-root)

(provide 'init-misc)
