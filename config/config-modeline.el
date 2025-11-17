;;; config-modeline-new.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2025-11-16
;;
;; Commentary:
;; 
;; Code:

(require 'all-the-icons)

;;; Settings:
;; Since we hide the fringes, this will prevent the modeline from going
;; offstage

(setq mode-line-right-align-edge 'right-margin)

;;; Faces:
(make-face 'lrd/mode-line-modified-face)
(set-face-attribute 'lrd/mode-line-modified-face nil
                    :inherit 'dired-marked :weight 'bold)
(make-face 'lrd/mode-line-read-only-face)
(set-face-attribute 'lrd/mode-line-read-only-face nil
                    :inherit 'dired-flagged :weight 'bold)
(make-face 'lrd/mode-line-warning-face)
(set-face-attribute 'lrd/mode-line-warning-face nil :inherit 'warning :weight 'bold)

;;; Components:
(defvar-local lrd/mode-line-modified
    '(:eval (cond
             (buffer-read-only
              (propertize "RO" 'face 'lrd/mode-line-read-only-face))
             ((buffer-modified-p)
              (propertize "MD" 'face 'lrd/mode-line-modified-face))
             (t "--"))))

(defvar lrd/mode-line-client
  '(:eval (cond
           ((not buffer-file-name) "*")
           ((and server-buffer-clients
                (file-remote-p buffer-file-name)) "")
           (server-buffer-clients "^")
           ((file-remote-p buffer-file-name) "@")
           (t " "))))

(defvar lrd/mode-line-buffer-id-map (make-sparse-keymap)
  "Mouse map for clicking on modeline buffer ID")
(define-key lrd/mode-line-buffer-id-map
            [mode-line down-mouse-1] 'mouse-buffer-menu)

(defvar-local lrd/mode-line-buffer-identification
    '(:eval
      (propertize (buffer-name)
                  'face 'mode-line-buffer-id
                  'mouse-face 'mode-line-highlight
                  'local-map lrd/mode-line-buffer-id-map)))

(defvar-local lrd/mode-line-modes t)

(defvar lrd/mode-line-fancy-icons t)

(defun lrd/flymake-make-indicator (type count face)
  (if count
      (let ((fm-icon-text `((error "\u2297"
			                       ,(all-the-icons-faicon
				                     "times-circle" :v-adjust 0.03))
			                (warning "\u26A0"
				                     ,(all-the-icons-faicon
				                       "exclamation-triangle" :v-adjust 0.03))
			                (info "\u24d8"
			                      ,(all-the-icons-faicon
				                    "info" :v-adjust 0.03)))))
        (if (and (display-graphic-p) lrd/mode-line-fancy-icons)
	        (concat (propertize (caddr (assq type fm-icon-text))
		                        'face `(:foreground
			                            ,(face-foreground face nil t)
			                            :family
			                            ,(all-the-icons-icon-family
				                          (caddr (assq type fm-icon-text)))))
                    (propertize (format "%d" count) 'face face))
	      (propertize
           (format "%s:%d"
                   (cadr (assq type fm-icon-text)) count)
           'face face )))))

(defvar-local lrd/mode-line-flymake
    '(:eval (when (bound-and-true-p flymake-mode)
              (let* ((text (let-alist (lrd/flymake-count-errors)
                             (if (or .error .warning .info)
                                 (string-join
                                  (list
                                   (lrd/flymake-make-indicator 'error .error 'flymake-error-echo)
                                   (lrd/flymake-make-indicator 'warning .warning 'flymake-warning-echo)
                                   (lrd/flymake-make-indicator 'info .info 'flymake-note-echo))
                                  " ")))))
                text
                ))))

;; Required for mode line security
(dolist (var '(lrd/mode-line-modified
               lrd/mode-line-client
               lrd/mode-line-flymake
               lrd/mode-line-buffer-identification
               lrd/mode-line-modes))
  (put var 'risky-local-variable t))

;;; Modeline definition
(setq-default mode-line-format
            '("%e" mode-line-front-space
              lrd/mode-line-modified
              lrd/mode-line-client
              " "
              lrd/mode-line-buffer-identification
              "  "
              mode-name
              "   "
              lrd/mode-line-flymake
              mode-line-format-right-align
              (vc-mode vc-mode)
              " "
              (line-number-mode "%l:%c")))

(provide 'config-modeline)

