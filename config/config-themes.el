;;;config-themes.el --- Theme-related configuration  -*- lexical-binding: t; -*-

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

;;;;; Select a theme family
(defvar lrd/theme-family 'ef)
;(defvar lrd/theme-family 'modus)

;;;; Helper functions
;; On 256-color terminals, tty-color-approximate aggressively shuns
;; grayscale values, which is bad for background colors. Fix that here.

(defun lrd/grayscale (color)
  "Given a color, return the closest gray"
  (let ((val (/ (apply '+ (tty-color-standard-values color)) (* 3 256))))
    (format "#%02x%02x%02x" val val val)))

(defun lrd/get-theme-spec (theme face)
  "Gretrieve the current spec in theme for a face"
  (let ((oldspec (assq theme (get face 'theme-face))))
    (cadr oldspec)))

(defun lrd/filter-spec (spec)
  "Filter out the restricted color stanza from a spec"
  (seq-filter (lambda (elt)
                (not (equal '((min-colors 88)) (car elt)))) spec))

(defun lrd/theme-fix-background ()
  "set backgrounds to the closest grayscale on text terminals"
  (let* ((bg-fix `((((min-colors 257))
                    :background ,(face-attribute
                                  'default :background))
                   (((min-colors 88))
                    :background ,(lrd/grayscale
                                  (face-attribute
                                   'default :background)))))
         (hl-fix `((((min-colors 257))
                    :background ,(face-attribute 'hl-line :background))
                   (((min-colors 88))
                    :background ,(lrd/grayscale
                                  (face-attribute
                                   'hl-line :background))))))
    (message "Fixing background colors...")
    (custom-push-theme 'theme-face 'default 'user 'set
                       (append (lrd/filter-spec
                                (lrd/get-theme-spec 'user 'default))
                               bg-fix))
    (custom-theme-recalc-face 'default)
    ;; this face may not exist
    (and (facep 'hl-line)
         (custom-push-theme 'theme-face 'hl-line 'user 'set
                            (append (lrd/filter-spec
                                     (lrd/get-theme-spec 'user 'hl-line))
                                    hl-fix))
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
	  ;; Faint yellow comments and green for strings
	  (comment yellow-faint)
          (string green-warmer)
	  )
	)
  :config
  (load-theme 'modus-vivendi)
  )
  )

;;;; Alternate: EF themes

(when (eq lrd/theme-family 'ef)
  (use-package ef-themes
    :init
    (setq ef-themes-disable-other-themes t)
    (setq ef-themes-headings ; read the manual's entry or the doc string
          '((0 . (variable-pitch light 1.9))
            (1 . (variable-pitch light 1.8))
            (2 . (variable-pitch light 1.7))
            (3 . (variable-pitch semilight 1.6))
            (4 . (variable-pitch semilight 1.5))
            (5 . (variable-pitch regular 1.4))
            (6 . (variable-pitch regular 1.3))
            (7 . (variable-pitch regular 1.2))    ; absence of weight means `bold'
            (agenda-date . (semilight 1.5))
            (agenda-structure . (variable-pitch light 1.9))
            (t . (variable-pitch regular 1.1))))
    :config
    (require 'hl-line)
    ;; If in daemon mode, do the fixup after we create a frame. Otherwise,
    ;; we can fix it in the post-load hook
    (if (daemonp)
        (add-hook 'after-make-frame-functions
                  (lambda (frame)
                    (with-selected-frame frame
                      (lrd/theme-fix-background))))
      (add-hook 'ef-themes-post-load-hook 'lrd/theme-fix-background))
    ;; Themes with potential...
    ;; (ef-themes-select 'ef-dark)
    (ef-themes-select 'ef-symbiosis)
    ;; (ef-themes-select 'ef-duo-dark)
    ;; (ef-themes-select 'ef-trio-dark)
    ;; (ef-themes-select 'ef-maris-dark)
    ;; (ef-themes-select 'ef-cherie)
    )
  )


(provide 'config-themes)
;;;; Postscript
;; Local Variables:
;; outline-regexp: ";;;;*"
;; End:
;; config-themes.el ends here
