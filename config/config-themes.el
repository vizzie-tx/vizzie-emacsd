;;;config-themes.el --- Theme-related configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-09-03
;;.
;; Commentary:
;; 
;; Code:

;;;; General settings
;;;;; Mark all themes as safe
(setq custom-safe-themes t)

;;;;; Select a theme
;;(defvar lrd/theme-family 'ef)
(defvar lrd/theme-family 'ef)

;;;; Helper functions
;; On 256-color terminals, tty-color-approximate aggressively shuns
;; grayscale values, which is bad for background colors. Fix that here.
(defun lrd/grayscale (color)
  "Given a color, return the closest gray"
  (let ((val (/ (apply '+ (tty-color-standard-values color)) (* 3 256))))
    (format "#%02x%02x%02x" val val val)))

(defun lrd/get-face-background (face)
  (let (background)
    (dolist (elt (reverse (get 'default 'theme-face)))
      (and (not (eq (car elt) 'user))
           (let ((tail (cadr elt)))
             (while tail
               (let* ((entry (pop tail))
                      (display (car entry))
                      (attrs (cdr entry)))
                 (and (plist-get attrs :background)
                      (setq background (plist-get attrs :background))))))))
    background))

(defun lrd/theme-fix-background ()
  (let ((default-bg (lrd/get-face-background 'default))
        (hl-bg (lrd/get-face-background 'hl-line)))
    (and default-bg
         (custom-theme-set-faces
          'user
          `(default ((((type tty)) . (:background ,(lrd/grayscale default-bg))))))
         (custom-theme-recalc-face 'default))
    (and hl-bg
         (custom-theme-set-faces
          'user
          `(hl-line ((((type tty)) . (:background ,(lrd/grayscale hl-bg))))))
         (custom-theme-recalc-face 'hl-line))))

;;;; Other colorful stuff
;;;;; Rainbow Delimiters
;; Color parentheses in multiple colors
;; so that we have an easier time matching
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight the current line
(global-hl-line-mode 1)

;;;; Modus-themes: Standard
(when (eq lrd/theme-family 'modus)
  (use-package modus-themes
    :ensure t
    :defines modus-themes-mode-line modus-themes-paren-match modus-themes-region
    :init
    (setq modus-themes-bold-constructs t
	      modus-themes-disable-other-themes t
	      modus-themes-common-palette-overrides
	      '(;; See https://protesilaos.com/emacs/modus-themes#h:df1199d8-eaba-47db-805d-6b568a577bf3
	        ;; Make the mode line borderless
	        (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
	        ;; Allow font-lock to work in the region
	        (fg-region unspecified)
	        ;; More intense, underlined paren matches
	        (bg-paren-match bg-cyan-intense)
	        (underline-paren-match fg-main)
	        )
	      )
    :config
    (modus-themes-load-theme 'modus-vivendi)
    )
  )

;;;; Alternate: EF themes

(when (eq lrd/theme-family 'ef)
  (use-package ef-themes
    :init
    (setq ef-themes-disable-other-themes t)
    (setq ef-themes-headings ; read the manual's entry or the doc string
          '((0 . (fixed-pitch light 1.9))
            (1 . (fixed-pitch light 1.8))
            (2 . (fixed-pitch light 1.7))
            (3 . (fixed-pitch semilight 1.6))
            (4 . (fixed-pitch semilight 1.5))
            (5 . (fixed-pitch regular 1.4))
            (6 . (fixed-pitch regular 1.3))
            (7 . (fixed-pitch regular 1.2))    ; absence of weight means `bold'
            (agenda-date . (semilight 1.5))
            (agenda-structure . (variable-pitch light 1.9))
            (t . (variable-pitch regular 1.1)))
          modus-themes-bold-constructs t
          modus-themes-italic-constructs t
          modus-themes-common-palette-overrides
          '(
	        ;; Make the mode line borderless
	        (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
	        ;; Allow font-lock to work in the region
	        (fg-region unspecified)
	        ;; More intense, underlined paren matches
	        (bg-paren-match bg-cyan-intense)
	        (underline-paren-match fg-main)
            )
          )
    
    :config
    (require 'hl-line)
    (add-hook 'modus-themes-post-load-hook 'lrd/theme-fix-background)
    ;; Themes with potential...
    ;; (ef-themes-select 'ef-dark)
    ;; (ef-themes-load-theme 'ef-symbiosis)
    ;; (ef-themes-load-theme 'ef-duo-dark)
    ;; (ef-themes-load-theme 'ef-trio-dark)
    ;; (ef-themes-load-theme 'ef-maris-dark)
    ;; (ef-themes-load-theme 'ef-cherie)
    ;; (ef-themes-load-theme 'ef-winter)
    (ef-themes-load-theme 'ef-autumn)
    
    )
  )


(provide 'config-themes)
;;;; Postscript
;; Local Variables:
;; outline-regexp: ";;;;*"
;; End:
;; config-themes.el ends here
