;;; post-init.el --- User-specific init  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-09-03
;;
;; Commentary:
;; 
;; Code:


;; workaround for Emacs frame size bug
(add-hook 'after-make-frame-functions
   (defun +resize-after-make-frame (frame)
     "HACK Resize new frames shortly after creation.  Works around 
https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67654"
     (let ((width (/ (frame-text-width frame) (frame-char-width frame)))
           (height (/ (frame-text-height frame) (frame-char-height 
frame))))
       (sleep-for 0 1)
       (set-frame-size frame width height))))

(setq help-window-select t)

;; TRAMP remote files
(setq tramp-default-method "ssh")

;; Default unknown buffers to text mode as a more reasonable default
(setq magic-fallback-mode-alist
      (append magic-fallback-mode-alist '((".*" . text-mode))))

;;Make server buffers for temp files go away when done with them
(defvar lrd/transient-regexp
  "^/tmp/Re\\|/Mail/drafts/\\|^/tmp/ed\.[0-9a-f]+$\\|^/tmp/mcs[0-9a-f]+$")
(and (fboundp 'server-start)
     (customize-set-value 'server-temp-file-regexp lrd/transient-regexp))

(defvar lrd/auto-save-dir
  (expand-file-name "autosave/" user-emacs-directory))
(setq auto-save-file-name-transforms `((".*" ,lrd/auto-save-dir t)))

;;Tag the end of file buffers
(defun lrd-mark-end-emacs ()
  (let ((old-overlays (overlays-in (point-max) (point-max)))
                      (eob-mark (make-overlay (point-max) (point-max) nil t t))
                      (eob-text "[END]"))
    (dolist (next-overlay old-overlays)
            (if (overlay-get next-overlay 'lrd-buffer-overlay)
                  (delete-overlay next-overlay)))
    (put-text-property 0 (length eob-text)
                       'face '(:weight bold :background "white" :foreground "maroon") eob-text)
    (overlay-put eob-mark 'after-string eob-text)
    (overlay-put eob-mark 'lrd-buffer-overlay t)))
;;
(add-hook 'find-file-hook 'lrd-mark-end-emacs)

;; Set some more UI/UX defaults
(require 'config-ui)
(require 'config-themes)

;; Various utility packages and modes
(require 'config-completion)
(require 'config-isearch)
(require 'config-vterm)

;; Use spaceline for the mode line, jazz things up
;;(require 'config-spaceline)
;;(require 'config-telephone-line)
(require 'config-modeline)

;; Support for various programming languages
(require 'config-languages)
(require 'config-tex)

;; Load this last to take advantage of functions in other mdolues
(require 'config-keys)

(provide 'post-init)
;; post-init.el ends here
