(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(require 'org-capture)

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
      org-startup-indented t
      )

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
     ;; (require 'org-fstree)
     (setq org-ditaa-jar-path (expand-file-name "~/.emacs.d/elpa/contrib/scripts/ditaa.jar"))
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

     (message "load org-mode")
     ))

(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map [(control meta ?r)] 'remember)

(require 'org-latex)
;; org-mode latex settings.
(setq org-export-with-sub-superscripts nil) ; 取消^和_字体上浮和下沉的特殊性
(setq org-export-latex-listings t)
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %b"
        "xelatex -interaction nonstopmode %b"))
(add-to-list 'org-export-latex-classes
             '("org-article"
               "\\documentclass{org-article}
                 \\usepackage{zhfontcfg}
                 [NO-DEFAULT-PACKAGES]
                 [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(eval-after-load 'org-html
  '(progn
;;; add a horizontal line before footnotes
     (setq org-export-html-footnotes-section
           (concat "<hr />" org-export-html-footnotes-section))

;;; add a horizontal line before postamble.
     (setq org-export-html-postamble "<hr /><p id=\"postamble-line\">
<a href=\"https://plus.google.com/112098239943590093765?rel=author\">By %a</a> @ %d <a href=\"http://sydi.org\">SYDI.ORG</a></p>")
     (add-to-list 'org-export-language-setup
                  '("zh-CN" "作者" "日期" "目录" "脚注"))
     (setq org-export-default-language "zh-CN")
     (setq org-export-htmlize-output-type "css")
     (setq org-export-htmlize-css-font-prefix "")
     (setq org-export-allow-BIND t)
     (setq org-export-html-style-include-scripts nil) ; 不加载默认js
     (setq org-export-html-style-include-default nil) ; 不加载默认css
     (setq org-export-html-link-home "http://sydi.org/")
     (setq org-export-with-section-numbers nil)
     ;; (setq org-export-page-keywords "施宇迪 sydi.org")
     ;; (setq org-export-page-description "施宇迪 sydi.org")
     (setq org-export-html-preamble (lambda () "<g:plusone></g:plusone>"))
     (setq org-export-html-home/up-format
           "<div id=\"home-and-up\"> [ <a href=\"%s\"> UP </a> ] [ <a href=\"%s\"> HOME </a> ] <button class='btn btn-inverse' onclick='show_org_source()'>查看Org源文件</button></div>")))

;;;###autoload
(defun sydi/after-export-org ()
  (when (and buffer-file-name (string-match ".*\\.html" buffer-file-name))
    (save-excursion
      (while (search-forward "<body>" nil t)
        (replace-match "<body>\n<div id=\"frame-table\"><div id=\"frame-table-row\"><div id=\"content-wrapper\">" nil t)))
    (save-excursion
      (while (search-forward "</body>" nil t)
        (replace-match "</div><div id=\"sidebar\"></div></div></div>\n</body>" nil t)))
    ;; if need add comment box...
    (save-excursion
      (if (boundp 'comment-box)
          (progn
            (goto-char (point-min))
            (while (search-forward "<div id=\"postamble\">" nil t)
              (replace-match "<script type='text/javascript' charset='utf-8' src='http://open.denglu.cc/connect/commentcode?appid=21489dengpEAtSRBbxboLxnwPmaqRA'></script>\n<div id=\"postamble\">" nil t)))))
    ;; add site-wide title
    (save-excursion
      (while (search-forward "</title>" nil t)
        (replace-match " - 施宇迪的另一片空间</title>" nil t)))

    (todochiku-message (nth 2 (assoc 'title (assoc 'head (libxml-parse-html-region (point-min) (point-max)))))  "" (todochiku-icon 'bell))))

;;;###autoload
(defun sydi/sync-server ()
  (message "sync file to server")
  (async-shell-command "update_sydi_org.sh")
  (message "sync file to server complete")
  )

(defun sydi/htmlize-buffer ()
  (with-current-buffer (htmlize-buffer)
    (goto-char (point-min))
    (let ((p1 (if (re-search-forward "<style" nil t) (match-beginning 0)) )
          (p2 (if (re-search-forward "</style>" nil t) (1+ (match-end 0)))))
      (delete-region p1 p2)
      (current-buffer))))

(defun sydi/org-publish-org-to-orghtml (plist filename pub-dir)
  (unless (file-exists-p pub-dir)
    (make-directory pub-dir t))
  (let ((visiting (find-buffer-visiting filename))
        (htmlize-output-type "css"))
    (save-excursion
      (org-pop-to-buffer-same-window (or visiting (find-file filename)))
      (let* ((plist (cons :buffer-will-be-killed (cons t plist)))
	     (init-buf (current-buffer))
	     (init-point (point))
	     (init-buf-string (buffer-string))
	     (export-file (concat pub-dir
                                  (file-name-nondirectory filename)
                                  ".html")))
	;; run hooks before exporting
	(run-hooks 'org-publish-before-export-hook)
	;; export the possibly modified buffer
        (let ((export-buffer (sydi/htmlize-buffer)))
          (when (and (bufferp export-buffer)
                     (buffer-live-p export-buffer))
            (with-current-buffer export-buffer
              (goto-char (point-min))
              (if (search-forward "</head>" nil t)
                  (replace-match
                   (concat org-export-html-style-extra "</head>") nil nil))
              (write-file export-file)
              (run-hooks 'org-publish-after-export-hook)
              (if (buffer-modified-p) (save-buffer))
              (kill-buffer export-buffer))))
        ;; maybe restore buffer's content
        (set-buffer init-buf)
        (when (buffer-modified-p init-buf)
          (erase-buffer)
          (insert init-buf-string)
          (save-buffer)
          (goto-char init-point))
        (unless visiting
          (kill-buffer init-buf))))))

;;;###autoload
(defun set-org-publish-project-alist ()
  (setq org-publish-project-alist
        '(("sydi"
           :components ("sydi-pages" "sydi-static"))
          ("sydi-static"
           :base-directory "~/sydi.org/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html\\|div"
           :publishing-directory "~/sydi.org/html"
           :recursive t
           :publishing-function org-publish-attachment)
          ("sydi-pages"
           :base-directory "~/sydi.org/org/"
           :base-extension "org"
           :publishing-directory "~/sydi.org/html/"
           :html-extension "html"
           :recursive t
           :makeindex t
           :auto-sitemap t
           :sitemap-ignore-case t
           :sitemap-filename "sitemap.org"
           :htmlized-source t
           :table-of-contents nil
           :auto-preamble t
           :exclude ".*my-wife.*\.org"
           :sitemap-title "站点地图 for 本网站"
           :author "施宇迪"
           :email "a@sydi.org"
           :language "zh-CN"
           :style "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js\"></script>
<script type=\"text/javascript\" src=\"/images/site.js\"></script>
<link rel=\"stylesheet\" href=\"/images/site.css\" />
<link rel=\"stylesheet\" href=\"/images/me/mediaelementplayer.css\" />
<link rel=\"stylesheet\" href=\"/images/bootstrap/css/bootstrap.css\" />
<link rel=\"stylesheet\" href=\"/images/bootstrap/css/bootstrap-responsive.css\" />
<script type=\"text/javascript\" href=\"/images/bootstrap/js/bootstrap.min.js\"></script>
<link href='http://sydi.org/images/logo.png' rel='icon' type='image/x-icon'/>
"
           :publishing-function (org-publish-org-to-html
                                 org-publish-org-to-org)
           :completion-function (sydi/sync-server)))))

(eval-after-load "org-publish"
  (add-hook 'org-publish-after-export-hook 'sydi/after-export-org))

(defun worg-fix-symbol-table ()
  (when (string-match "org-symbols\\.html" buffer-file-name)
    (goto-char (point-min))
    (while (re-search-forward "<td>&amp;\\([^<;]+;\\)" nil t)
      (replace-match (concat "<td>&" (match-string 1)) t t))))

(setq sydi-base-directory "~/sydi.org/org/")
(setq sydi-base-code-directory "~/sydi.org/html/code/")
;; (sydi-base-color-themes-directory "~/sydi.org/worg/color-themes/")
(setq sydi-base-images-directory "~/sydi.org/html/images/")
(setq sydi-publish-directory "~/sydi.org/html/")

(defun publish-sydi nil
  "Publish Worg in htmlized pages."
  (interactive)
  (add-hook 'org-publish-after-export-hook 'worg-fix-symbol-table)
  (let ((org-format-latex-signal-error nil)
        (org-startup-folded nil)
        (sydi-base-directory "~/sydi.org/org/")
        (sydi-base-code-directory "~/sydi.org/html/code/")
        ;; (sydi-base-color-themes-directory "~/sydi.org/worg/color-themes/")
        (sydi-base-images-directory "~/sydi.org/html/images/")
        (sydi-publish-directory "~/sydi.org/html/"))
    (set-org-publish-project-alist)
    (message "Emacs %s" emacs-version)
    (org-version)
    (org-publish-project "sydi")))

; external browser should be firefox
(setq browse-url-generic-program
      (executable-find "chromium"))

(defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
  (let ((browse-url-browser-function
         (cond ((equal (ad-get-arg 0) '(4))
                'browse-url-generic)
               ((equal (ad-get-arg 0) '(16))
                'choose-browser)
               (t
                (lambda (url &optional new)
                  (w3m-browse-url url t))))))
    ad-do-it))

;; (add-hook 'org-mode-hook 'inhibit-autopair)

(defun sort-by-date ()
  (interactive)
  (require 'find-lisp)
  (save-excursion
    (let* ((org-files (sort
                      (find-lisp-find-files "~/sydi.org/org/" "\\.org$")
                      'org-date-compare))
           (rss-filename "rss2.xml")
           (visiting (find-buffer-visiting rss-filename))
           (rss-buffer (or visiting (find-file rss-filename))))
      (with-current-buffer rss-buffer
        (kill-region (point-min) (point-max))
        (insert "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<rss version=\"2.0\">
  <channel>
    <title>SYDI.ORG 施宇迪的另一片空间</title>
    <link>http://sydi.org</link>
    <description>施宇迪的个人网站博客，设计Archlinux、Emacs、Awesome、Perl、分布式数据</description>")
        (dolist (file org-files)
          (with-current-buffer rss-buffer
            (insert "<item>
  <title>title</title>
  <link>http://sydi.org/</link>
  <description>")
            )
          (with-current-buffer (find-file file)
            (org-export-as-html t nil nil "*tmp-rss*" t))
          (with-current-buffer rss-buffer
            (insert-buffer-substring-no-properties "*tmp-rss*")
            (kill-buffer "*tmp-rss*")
            (insert "</description></item>")
            (insert "</channel></rss>")
            (save-buffer)))))))

(defun org-date-compare (a b)
  (require 'org-publish)
  (let* ((adate (org-publish-find-date a))
         (bdate (org-publish-find-date b))
         (A (+ (lsh (car adate) 16) (cadr adate)))
         (B (+ (lsh (car bdate) 16) (cadr bdate))))
    (>= A B)))

(provide 'init-org)
