;; written by zhuweng@taobao-inc

(require 'cc-langs)
(defsubst md-at-inclass/topmost-intro (sintax)
  (and
   (eq (caar sintax) 'inclass)
   (eq (car (cadr sintax)) 'topmost-intro)))

(defun md-ind-prot-class ()
  "Give an extra level of indentation to class thingies under an access
specifier, e.g.:

    public
        A();        <========== extra level of indentation.

This should really be properly implemented in CC Mode, but it's not."
  (and
   (md-at-inclass/topmost-intro c-syntactic-context)
   (let (m-type)
     (when
	 (save-excursion
	   (back-to-indentation)
	   ;; Go back one "statement" at a time till we reach a label or something
	   ;; which isn't an inclass/topmost-intro
	   (while
	       (and (eq (setq m-type (c-beginning-of-statement-1)) 'previous)
		    (md-at-inclass/topmost-intro (c-guess-basic-syntax))))
	   ;; Have we found "private:", "public": or "protected"?
	   (and (eq m-type 'label)
		(looking-at
		 (eval-when-compile
		   (c-make-keywords-re nil (c-lang-const c-protection-kwds 
							 c++))))))
       (save-excursion
	 (back-to-indentation)
	 (c-shift-line-indentation c-basic-offset))))))


;; Create my personal c style which follows the coding standard of the OceanBase project
(defconst oceanbase-c-style
  '((c-basic-offset . 2)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open before after)
				   (brace-list-open)))
    (c-offsets-alist (statement-block-intro . +)
		     (substatement-open . 0)
		     (inline-open . 0)
		     (substatement-label . 0)
		     (statement-cont . +)
		     (case-label . +)
		     (inclass . +)
		     (access-label . 0)
		     (topmost-intro . 0)
		     )
    (c-special-indent-hook . md-ind-prot-class)
    )
  "C/C++ Programming Style for OceanBase project\nThis style is a modification of stroustrup style. ")
(c-add-style "oceanbase" oceanbase-c-style)

(add-hook 'c-mode-hook
	  '(lambda ()
	     (c-set-style "oceanbase")))
(add-hook 'c++-mode-hook
	  '(lambda()
	     (c-set-style "oceanbase")))

(provide 'oceanbase-style)
