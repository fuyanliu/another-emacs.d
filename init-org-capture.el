;;; Time-stamp: <2013-07-01 15:15:49 ryan>

(require 'org-capture)

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("w" "Work Journal" entry (file+datetree "~/org/work_journal.org")
         "* %?\nEntered on %U\n  %i\n")
        ("j" "Web Journal" entry (file "~/sydi.org/org/journal.org")
         "* %U\n%?%i" :prepend t)
        ("o" "Reading Oceanbase" entry (file "~/documents/reading/oceanbase.org")
         "* %?\n [[%l][follow]] -- [%U]\n  %i")
        ("l" "Reading SQLite" entry (file "~/documents/reading/sqlite.org")
         "* %?\n [[%l][follow]] -- [%U]\n  %i")))

(provide 'init-org-capture)
