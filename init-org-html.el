(defvar sydi/base-directory "~/sydi.org/org/"
  "base org files directory")
(defvar sydi/base-code-directory "~/sydi.org/html/code/")
;; (defvar sydi/base-color-themes-directory "~/sydi.org/worg/color-themes/")
(defvar sydi/base-images-directory "~/sydi.org/html/images/")
(defvar sydi/publish-directory "~/sydi.org/html/")
(defvar sydi/site-url "http://sydi.org/")
(defvar sydi/google-id "112098239943590093765")
(defvar sydi/site-name "MiScratch")

(defvar sydi/comment-box-p t "Should add commnet box for this page")
(defvar sydi/homepage-p nil "Indicate whether the page is home page")
(defvar sydi/single-p t "Indicate whether a single post page or not")

(add-to-list 'org-export-plist-vars '(:comment-box "comment-box" sydi/comment-box-p))
(add-to-list 'org-export-plist-vars '(:homepage "homepage" sydi/homepage-p))
(add-to-list 'org-export-plist-vars '(:single "single" sydi/single-p))

(eval-after-load 'org-html
  '(progn
;;; add a horizontal line before footnotes
     (add-to-list 'org-export-language-setup
                  '("zh-CN" "作者" "日期" "目录" "脚注"))
     (setq org-export-default-language "zh-CN")
     (setq org-export-htmlize-output-type "css")
     (setq org-export-htmlize-css-font-prefix "")
     (setq org-export-allow-BIND t)
     (setq org-export-html-style-include-scripts nil) ; 不加载默认js
     (setq org-export-html-style-include-default nil) ; 不加载默认css
     (setq org-export-html-link-home sydi/site-url)
     (setq org-export-with-section-numbers nil)
     (setq org-export-page-keywords "施宇迪 sydi.org")
     (setq org-export-page-description "施宇迪 sydi.org")
     (setq org-export-html-preamble (lambda () "<g:plusone></g:plusone>"))))


;;;###autoload
(defun sydi/sync-server ()
  (message "sync file to server")
  ;; (async-shell-command "update_sydi_org.sh")
  (message "sync file to server complete"))

;;;###autoload
;;; The hook is run after org-html export html done and
;;; still stay on the output html file.
(defun sydi/final-export ()
  ;; declear free-varible
  (defvar opt-plist)
  (save-excursion
    (let* ((title (plist-get opt-plist :title))
          (email (plist-get opt-plist :email))
          (author (plist-get opt-plist :author))
          (body-only (plist-get opt-plist :body-only))
          (date (plist-get opt-plist :date))
          (language    (plist-get opt-plist :language))
          (keywords    (org-html-expand (plist-get opt-plist :keywords)))
          (description (org-html-expand (plist-get opt-plist :description)))
          (style (plist-get opt-plist :style))
          (charset (and coding-system-for-write
                        (fboundp 'coding-system-get)
                        (coding-system-get coding-system-for-write
                                           'mime-charset)))
          (comment-box (if sydi/comment-box-p
                           "<div class=\"comments ds-thread\"></div>" ""))
          (header (if sydi/homepage-p "" "<div id=\"header\"></div>"))
          (content-title (if sydi/homepage-p ""
                           (format "<h1 class=\"title\">%s</h1>" title)))
          (content (prog1 (buffer-substring-no-properties (point-min) (point-max))
                     (kill-region (point-min) (point-max)))))
      (if body-only
          (insert (format "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\">
<head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>
<meta name=\"title\" content=\"%s\"/>
<meta name=\"generator\" content=\"Org-mode modify by Yudi Shi\"/>
<meta name=\"generated\" content=\"%s\"/>
<meta name=\"author\" content=\"%s\"/>
<meta name=\"description\" content=\"%s\"/>
<meta name=\"keywords\" content=\"%s\"/>
%s
</head>
<body>
<div id=\"wrapper\">
  <div id=\"sidebar\"></div>
  <div id=\"main\">
    %s
    <div id=\"content\">
      <div id=\"page-content\">
        <div class=\"post single\">
          <ul class=\"meta\"></ul>
          <div class=\"feature-image\"></div>
          <div class=\"the-content\">
            <!-- content tilte -->
            %s
            <!-- content -->
            %s
            <!-- comment box -->
            %s
          </div>
        </div>
      </div>
    </div>
  </div>
</div>
<!-- ENS WRAPPER -->
<div id=\"footer\"></div>
</body></html>"
                          language
                          language
                          title
                          charset
                          title
                          date
                          author
                          description
                          keywords
                          style
                          header
                          content-title
                          content
                          comment-box
                          sydi/google-id
                          author
                          date
                          sydi/site-name))
        (insert content)))))

;;;###autoload
(defun set-org-publish-project-alist ()
  (setq org-publish-project-alist
        `(("sydi"
           :components ("sydi-pages" "sydi-static"))
          ("sydi-static"
           :base-directory "~/sydi.org/org/"
           :base-extension "xml\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html\\|div\\|pl\\|template"
           :publishing-directory "~/sydi.org/html"
           :recursive t
           :publishing-function org-publish-attachment)
          ("sydi-pages"
           :base-directory ,sydi/base-directory
           :base-extension "org"
           :publishing-directory ,sydi/publish-directory
           :html-extension "html"
           :recursive t
           :makeindex t
           :auto-sitemap t
           :sitemap-ignore-case t
           :sitemap-filename "sitemap.org"
           :htmlized-source t
           :table-of-contents nil
           :auto-preamble t
           ;; :exclude ".*my-wife.*\.org"
           :sitemap-title "站点地图 for 本网站"
           :author "施宇迪"
           :email "a@sydi.org"
           :language "zh-CN"
           :style "
<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js\"></script>
<script type=\"text/javascript\" src=\"/images/site.js\"></script>
<script type=\"text/javascript\" src=\"/javascripts/custom.js\"></script>
<script type=\"text/javascript\" src=\"/images/js/bootstrap.min.js\"></script>
<link rel=\"stylesheet\" href=\"/images/site.css\" />
<link href='/images/logo.png' rel='icon' type='image/x-icon' />
<link href=\"atom.xml\" type=\"application/atom+xml\" rel=\"alternate\" title=\"sydi.org atom\" />
"
           :publishing-function (org-publish-org-to-html
                                 org-publish-org-to-org)
           :body-only t
           :completion-function (sydi/sync-server)))))

(defun sydi/publish ()
  "Publish Worg in htmlized pages."
  (interactive)
  (add-hook 'org-export-html-final-hook 'sydi/final-export)
  (let ((org-format-latex-signal-error nil)
        (org-startup-folded nil))
    (set-org-publish-project-alist)
    (message "Emacs %s" emacs-version)
    (org-version)
    (org-publish-project "sydi")))

(add-hook 'org-mode-hook 'inhibit-autopair)

;; external browser should be chromium
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

(defun sydi/generate-atom ()
  (interactive)
  (generate-atom "~/sydi.org/org" "~/sydi.org/org/atom.xml"))

;;;###autoload
(defun generate-atom (root-dir atom-file)
  "generate a atom style page"
  (save-excursion
    (let* ((org-export-allow-BIND t)
           (org-files (sydi/get-sorted-org-files root-dir))
           (atom-filename atom-file)
           (visiting (find-buffer-visiting atom-filename))
           (atom-buffer (or visiting (find-file atom-filename)))
           (title "MiScratch")
           (subtitle "About Linux, Distributed System, Data Base, High Performance System")
           (self-link "http://sydi.org/atom.xml")
           (link "http://sydi.org/")
           (id "http://sydi.org")
           (author "施宇迪")
           (email "a@sydi.org"))
      (with-current-buffer atom-buffer
        (kill-region (point-min) (point-max))
        (insert (format
                 "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>%s</title>
  <subtitle>%s</subtitle>
  <link href=\"%s\" rel=\"self\"/>
  <link href=\"%s\"/>
  <updated>%s</updated>
  <id>%s</id>
  <author>
    <name><![CDATA[%s]]></name><email>%s</email>
  </author>
  <generator uri=\"%s\">orgmode4sydi</generator>"
                 title subtitle self-link link
                 (format-time-string "%Y-%m-%dT%T%z")
                 id author email sydi/site-url))
        (dolist (file org-files)
          (let ((org-file-buffer (find-file file)))
            (set-buffer org-file-buffer)
            (let* ((plist (org-infile-export-plist))
                   (title (or
                           (plist-get plist :title)
                           "UNTITLED"))
                   (date (format-time-string
                          "%Y-%m-%dT%T%z"
                          (org-time-string-to-time (or
                                                    (plist-get plist :date)
                                                    (format-time-string
                                                     (org-time-stamp-format)
                                                     (cons 0 0))))))
                   (url (concat
                         (replace-regexp-in-string
                          (file-truename sydi/base-directory)
                          sydi/site-url
                          (file-name-sans-extension (buffer-file-name)))
                         ".html"))
                   (entry (format
                           "<entry>
  <title>%s</title>
  <link href=\"%s\" />
  <updated>%s</updated>
  <id>%s</id>
  <content type=\"html\"><![CDATA[%s]]></content></entry>"
                           title
                           url
                           date
                           url
                           (org-export-as-html 3 nil 'string t)
                           )))
              (kill-buffer org-file-buffer)
              (set-buffer atom-buffer)
              (insert entry))))
        (insert "</feed>")
        (save-buffer)
        (kill-buffer)))))

(defun sydi/get-org-file-date (file &optional other)
  "Return org file date in #+date header line using `current-time' format.

If #+date keyword is not set and `other' equals to \"modify\", return the file system's modification time instead, if `other' equals to \"change\" return the file system's last change time instead, if `other' equals to \"access\" return the file systems's access time instead, otherwise return 0 as 1970-01-01 00:00:00, the minimal time.
"
  (let ((visiting (find-buffer-visiting file)))
    (save-excursion
      (org-pop-to-buffer-same-window (or visiting (find-file-noselect file nil t)))
      (let* ((plist (org-infile-export-plist))
	     (date (plist-get plist :date)))
	(unless visiting
	  (kill-buffer (current-buffer)))
	(if date
	    (org-time-string-to-time date)
	  (when (file-exists-p file)
            (cond ((equal other "access") (nth 4 (file-attributes file)))
                  ((equal other "modify") (nth 5 (file-attributes file)))
                  ((equal other "change") (nth 6 (file-attributes file)))
                  (t '(0 0)))))))))

(defun sydi/get-sorted-org-files (root-dir)
  "return a sorted org files list"
  (require 'find-lisp)
  (let ((org-files (find-lisp-find-files root-dir "\\.org$"))
        (org-alist))
    (mapc
     (lambda (file)
       (set 'org-alist (cons
                        (cons file (sydi/get-org-file-date file))
                        org-alist)))
     org-files)
    (mapcar 'car
            (sort org-alist
                  (lambda (a b)
                    (let* ((adate (sydi/get-org-file-date (car a)))
                           (bdate (sydi/get-org-file-date (car b)))
                           (A (+ (lsh (car adate) 16) (cadr adate)))
                           (B (+ (lsh (car bdate) 16) (cadr bdate))))
                      (>= A B)))))))

(provide 'init-org-html)
