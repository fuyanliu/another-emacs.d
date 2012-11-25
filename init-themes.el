(if window-system
    ;; (color-theme-sanityinc-solarized-dark)
    ;; (color-theme-sanityinc-tomorrow-bright)
    (color-theme-sanityinc-tomorrow-night)
  )

;;------------------------------------------------------------------------------
;; New-style theme support, in which per-frame theming is not possible
;;------------------------------------------------------------------------------

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme t t)
      (message "Consider using 'M-x customize-themes' to save your preferred theme.")))
  (custom-set-variables '(custom-enabled-themes custom-enabled-themes)))

(add-hook 'after-init-hook 'reapply-themes)

;;------------------------------------------------------------------------------
;; Toggle between color themes
;;------------------------------------------------------------------------------
;;;###autoload
(defun light ()
  "Activate a light color theme."
  (interactive)
  (if (boundp 'custom-enabled-themes)
      (custom-set-variables '(custom-enabled-themes '(sanityinc-tomorrow-day)))
    (color-theme-sanityinc-tomorrow-day)))

;;;###autoload
(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (if (boundp 'custom-enabled-themes)
      (custom-set-variables
       '(custom-enabled-themes '(sanityinc-tomorrow-bright)))
    (color-theme-sanityinc-tomorrow-bright)))

;;;###autoload
(defun grey ()
  "Activate a grey color theme."
  (interactive)
  (if (boundp 'custom-enabled-themes)
      (custom-set-variables
       '(custom-enabled-themes '(sanityinc-tomorrow-night)))
    (color-theme-sanityinc-tomorrow-night)))

;;;###autoload
(defun dark-blue ()
  "Activate a dark blue color theme."
  (interactive)
  (if (boundp 'custom-enabled-themes)
      (custom-set-variables
       '(custom-enabled-themes '(sanityinc-solarized-dark)))
    (color-theme-sanityinc-solarized-dark)))

;;;###autoload
(defun pink ()
  "Activate a pink color theme."
  (interactive)
  (if (boundp 'custom-enabled-themes)
      (custom-set-variables
       '(custom-enabled-themes '(sanityinc-solarized-light)))
    (color-theme-sanityinc-solarized-light)))


(provide 'init-themes)
