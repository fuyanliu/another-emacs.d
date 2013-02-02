;;; Time-stamp: <2013-01-20 21:28:56 ryan>

(require 'org-capture)

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("w" "Work Journal" entry (file+datetree "~/org/work_journal.org")
         "* %?\nEntered on %U\n  %i\n")
        ("j" "Web Journal" entry (file "~/sydi.org/org/journal.org")
         "* %U\n%?%i" :prepend t)))

(provide 'init-org-capture)
