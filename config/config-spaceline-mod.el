;;; config-spaceline-mod.el - My custom spaceline -*- lexical-binding:t -*-
;;;
;;; My personal spaceline configuration, customized to my tastes.

;; For Flymake mode (mostly for eglot)
;; Disable changing the flymake lighter. Instead, we create a special
;; spaceline segment to neatly print the flymake error stats in a separate
;; location

;; Return either a unicode character or an all-the-icons graphical icon,
;; colored appropriately for the severity of the error.
(defun lrd/flymake-icon-or-char (type)
  (let ((fm-icon-text `((error "\u2297"
			                   ,(all-the-icons-faicon
				                 "times-circle" :v-adjust 0.03))
			            (warning "\u26A0"
				                 ,(all-the-icons-faicon
				                   "exclamation-triangle" :v-adjust 0.03))
			            (info "\u24d8"
			                  ,(all-the-icons-faicon
				                "info" :v-adjust 0.03))))
        ;; Reuse the modeline flycheck faces, they serve the same purpose
	    (face (intern (format "spaceline-flycheck-%S" type))))
    (cond ((display-graphic-p)
	       (propertize (caddr (assq type fm-icon-text))
		               'face `(:foreground
			                   ,(face-foreground face)
			                   :family
			                   ,(all-the-icons-icon-family
				                 (caddr (assq type fm-icon-text))))))
	      (t (propertize (cadr (assq type fm-icon-text)) 'face face )))))

;; Each of the states gets their own segment, we put them all together below.
(dolist (state '(error warning info))
  (let* ((segment-name (intern (format "flymake-%S" state)))
         (face (intern (format "spaceline-flycheck-%S" state)))
	 (icon (all-the-icons-material (symbol-name state)))
	 (icon-family (all-the-icons-icon-family icon))
	 (icon-face `(:foreground ,(face-foreground face)
				  :family ,(format "%s" icon-family))))
    (eval
     `(spaceline-define-segment ,segment-name
        ,(format "Information for flymake-%S. Requires flymake-mode to be enabled" state)
        (when (and (bound-and-true-p flymake-mode)
                   (flymake-running-backends))
          (let ((fm-stats (lrd/flymake-count-errors)))
            (when (assq ',state fm-stats)
	      (concat (lrd/flymake-icon-or-char ',state)
		      " "
		      (powerline-raw
		       (format "%d" (cdr (assq ',state fm-stats)))
		       ',face)))))))))

(spaceline-define-segment "eglot"
    (if (bound-and-true-p eglot--mode-line-format)
	    (powerline-raw (eglot-mode-line-format))))


;; Finally, our custom modeline.
;; buffer information, modes, and flymake errors on the left
;; vc status, line and column and global error segment on the right.
(spaceline-compile
  '(((buffer-modified
      buffer-size)
     :face highlight-face
     :priority 100)
    ((buffer-id remote-host)
     :priority 98)
    (major-mode :priority 79)
    (process :when active)
    (minor-modes :when active
                 :priority 9)
    (eglot)
    ((flymake-error flymake-warning flymake-info)
     :when active
     :priority 89))

  '((version-control :when active
		     :priority 78)
    ((line-column)
     :face 'isearch-group-1
     :priority 96)
    (global :when active)
    ))

;; Apply our new spaceline format
(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
(force-mode-line-update)

(provide 'config-spaceline-mod)
;; config-spaceline-mod.el ends here
