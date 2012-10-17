(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

<<<<<<< HEAD
(eval-after-load 'org-clock
  '(progn
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persistence-insinuate t)
    (setq org-clock-persist t)
    (setq org-clock-in-resume t)
=======
;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80
      org-startup-indented t)

; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(eval-after-load 'org-clock
  '(progn
     (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
     (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)))


;; ;; Show iCal calendars in the org agenda
;; (when *is-a-mac*
;;   (eval-after-load "org"
;;     '(if *is-a-mac* (require 'org-mac-iCal)))
;;   (setq org-agenda-include-diary t)
;; 
;;   (setq org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook
;;              (lambda ()
;;                (org-mac-iCal)))))))
;; 
;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0)))))
;;   )
>>>>>>> redguardtoo

    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)
    ))

(eval-after-load 'org
  '(progn
     ;; (require 'org-exp)
     ;; (require 'org-clock)
     ;; (require 'org-latex)

     ;; Various preferences
     (setq org-log-done t
           org-completion-use-ido t
           org-edit-timestamp-down-means-later t
           org-agenda-start-on-weekday nil
           org-agenda-span 14
           org-agenda-include-diary t
           org-agenda-window-setup 'current-window
           org-fast-tag-selection-single-key 'expert
           org-tags-column 80
           org-src-fontify-natively t)

     ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
     (setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
     ;; Targets start with the file name - allows creating level 1 tasks
     (setq org-refile-use-outline-path (quote file))
     ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
     (setq org-outline-path-complete-in-steps t)

     (setq org-todo-keywords
           (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                   (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))
     
                                        ; @see http://irreal.org/blog/?p=671
     ;; (require 'org-checklist)
     (require 'org-fstree)
     (setq org-ditaa-jar-path (format "%s%s" (if *cygwin* "c:/cygwin" "")
                                      (expand-file-name "~/.emacs.d/elpa/contrib/scripts/ditaa.jar")) )
     (add-hook 'org-mode-hook 'soft-wrap-lines)
     (defun soft-wrap-lines ()
       "Make lines wrap at window edge and on word boundary,
        in current buffer."
       (interactive)
       (setq truncate-lines nil)
       (setq word-wrap t)
       )

     (custom-set-variables
      '(org-agenda-files (quote ("~/personal/todo.org")))
      '(org-default-notes-file "~/personal/notes.org")
      '(org-agenda-span 7)
      '(org-deadline-warning-days 14)
      '(org-agenda-show-all-dates t)
      '(org-agenda-skip-deadline-if-done t)
      '(org-agenda-skip-scheduled-if-done t)
      '(org-agenda-start-on-weekday nil)
      '(org-reverse-note-order t)
      '(org-fast-tag-selection-single-key (quote expert))
      '(org-agenda-custom-commands
        (quote (("d" todo "DELEGATED" nil)
                ("c" todo "DONE|DEFERRED|CANCELLED" nil)
                ("w" todo "WAITING" nil)
                ("W" agenda "" ((org-agenda-span 21)))
                ("A" agenda ""
                 ((org-agenda-skip-function
                   (lambda nil
                     (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
                  (org-agenda-span 1)
                  (org-agenda-overriding-header "Today's Priority #A tasks: ")))
                ("u" alltodo ""
                 ((org-agenda-skip-function
                   (lambda nil
                     (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                               (quote regexp) "\n]+>")))
                  (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
      '(org-remember-store-without-prompt t)
      '(org-remember-templates
        (quote ((?t "* TODO %?\n  %u" "~/personal/todo.org" "Tasks")
                (?n "* %u %?" "~/personal/notes.org" "Notes"))))
      '(remember-annotation-functions (quote (org-remember-annotation)))
      '(remember-handler-functions (quote (org-remember-handler))))

     (setq org-export-with-sub-superscripts nil) ; 取消^和_字体上浮和下沉的特殊性
     (setq org-export-html-style-include-scripts nil) ; 不加载默认js
     (setq org-export-html-style-include-default nil) ; 不加载默认css
     (setq org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"/site.css\" />\n")
     (setq org-export-html-style-extra
           (concat
            "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js\"></script>"
            "\n"
            "<script type=\"text/javascript\" src=\"/site.js\"></script>"))
     (message "load org-mode")
     ))

(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map [(control meta ?r)] 'remember)

;; org-mode latex settings.
(eval-after-load 'org-latex
  '(progn
    (setq org-export-latex-listings t)
    (setq org-latex-to-pdf-process
          '("xelatex -interaction nonstopmode %b"
            "xelatex -interaction nonstopmode %b"))
    (add-to-list 'org-export-latex-classes
                 '("org-article"
                   "\\documentclass{org-article}
                 [NO-DEFAULT-PACKAGES]
                 [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    ))

(eval-after-load 'org-html
  '(progn
;;; add a horizontal line before footnotes
     (setq org-export-html-footnotes-section
           (concat "<hr />" org-export-html-footnotes-section))

;;; add a horizontal line before postamble.
     (setq org-export-html-postamble "<hr /><p id=\"postamble-line\">
<a href=\"http://www.sydi.org/\">%a</a> @ %d</p>")
     (add-to-list 'org-export-language-setup
                  '("zh-CN" "作者" "日期" "目录" "脚注"))
     (setq org-export-default-language "zh-CN")
     ))

;;;###autoload
(defun sydi/refact-html ()
  (when (string-match ".*\\.html" buffer-file-name)
    (goto-char (point-min))
    (while (search-forward "<body>" nil t)
      (replace-match "<body>\n<div id=\"frame-table\"><div id=\"frame-table-row\"><div id=\"content-wrapper\">" nil t))
    (goto-char (point-min))
    (while (search-forward "</body>" nil t)
      (replace-match "</div><div id=\"sidebar\"></div></div></div>\n</body>" nil t)))
  )

(defun test-comment ()
  (message (plist-get opt-plist :author))
  (if (plist-get opt-plist :comment-box)
                     (message "comment-box")
                   (message "no-comment-box")))

(add-hook 'org-export-html-final-hook
          'test-comment)

;;;###autoload
(defun sydi/sync-server ()
  (message "sync file to server")
  (async-shell-command "update_sydi_org.sh")
  (message "sync file to server complete")
  )

(eval-after-load "org-publish"
  '(progn
     (setq org-publish-project-alist
           '(("sydi.org.html"
              :base-directory "~/personal/sydi.org/org"
              :base-extension "org"
              :publishing-directory "~/personal/sydi.org/html"
              :recursive t
              :publishing-function org-publish-org-to-html
              :headline-levels 4  ; Just the default for this project.
              :auto-preamble t
              :auto-sitemap t
              :sitemap-filename "sitemap.org"
              :exclude ".*my-wife.*\.org"
              :sitemap-title "站点地图 for 本网站"
              :htmlize-source t
              :completion-function (sydi/sync-server)
              )
             ("sydi.org.org"
              :base-directory "~/personal/sydi.org/org"
              :base-extension "org"
              :publishing-directory "~/personal/sydi.org/html/org"
              :recursive t
              :publishing-function org-publish-attachment
              :completion-function (sydi/sync-server)
              )
             ("org-static"
              :base-directory "~/personal/sydi.org/org"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html\\|div"
              :publishing-directory "~/personal/sydi.org/html"
              :recursive t
              :publishing-function org-publish-attachment
              )
             ("org"
              :components ("org-notes" "org-static")
              )
             ))
     (add-hook 'org-publish-after-export-hook 'sydi/refact-html)
     ))

(provide 'init-org)
