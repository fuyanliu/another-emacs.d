; @see http://blog.nethazard.net/post-to-wordpress-blogs-with-emacs-org-mode/
(require 'org2blog-autoloads)
; setup proxy
;(setq url-proxy-services '(("http" . "127.0.0.1:8580"))) ; fr*egate
;(setq url-proxy-services '(("http" . "127.0.0.1:8087")))
;(setq url-proxy-services '(("http" . "127.0.0.1:8118"))) ; privoxy');
(setq org2blog/wp-blog-alist
      '(("blog.sydi.org"
         :url "http://blog.sydi.org/xmlrpc.php"
         :username "sylvester"
         :default-title "Untitled"
         :default-categories ("Linux")
         :tags-as-categories nil
         :show t
         )))
(setq org2blog/wp-use-crayon-shortcode t)
(setq org2blog/wp-use-sourcecode-shortcode nil)

(defun org2blog/wp-update-local-dir nil
  "update all org files recursly in a specify directory and publish them to wp"
  (interactive)
  (require 'find-lisp)
  (let ((org-files (find-lisp-find-files "~/sydi.org/org/" "\\.org$")))
    (dolist (file org-files)
      (find-file file)
      (goto-char (point-min))
      (if (search-forward "#+BLOG:" nil t 1)
          (org2blog/wp-post-buffer-and-publish)
        (message "no blog tag"))
      (save-buffer)
      (kill-buffer))))


(provide 'init-org2blog)

