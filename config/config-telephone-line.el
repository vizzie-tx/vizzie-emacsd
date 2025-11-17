;;; config-telephone-line.el ---
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2025-11-15
;;
;; Commentary:
;; 
;; Code:


(make-face 'mode-line-modified-face)
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'font-lock-warning-face :weight 'bold :reverse-video t)
(make-face 'mode-line-read-only-face)
(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'font-lock-cconstant-face :weight 'bold :reverse-video t)

(use-package telephone-line
  :config
  (set-face-attribute 'telephone-line-accent-active nil :background (ef-themes-get-color-value 'bg-char-0))
  (defvar lrd/telephone-line-semi-left
    (make-instance 'telephone-line-separator
                   :axis-func (lambda (x) (sqrt (- (expt float-pi 2) (expt x 2))))
                   :alt-separator telephone-line-utf-abs-left))
  (defvar lrd/telephone-line-semi-right
    (make-instance 'telephone-line-separator
                   :axis-func (lambda (x) (- (sqrt (- (expt float-pi 2) (expt x 2)))))
                   :alt-separator telephone-line-utf-abs-right))
  (defvar lrd/telephone-line-semi-hollow-left
    (make-instance 'telephone-line-subseparator
                   :axis-func (lambda (x) (sqrt (- (expt float-pi 2) (expt x 2))))
                   :alt-separator telephone-line-utf-abs-hollow-left))
  (defvar lrd/telephone-line-semi-hollow-right
    (make-instance 'telephone-line-subseparator
                   :axis-func (lambda (x) (- (sqrt (- (expt float-pi 2) (expt x 2)))))
                   :alt-separator telephone-line-utf-abs-hollow-right))
  (setq telephone-line-primary-left-separator 'lrd/telephone-line-semi-left
        telephone-line-primary-right-separator 'lrd/telephone-line-semi-right
        telephone-line-secondary-left-separator 'lrd/telephone-line-semi-hollow-left
        telephone-line-secondary-right-separator 'lrd/telephone-line-semi-hollow-right)
  (telephone-line-defsegment* lrd/telephone-line-buffer-modified-segment ()
    (cond (buffer-read-only (propertize "RO" 'face 'mode-line-read-only-face))
          ((buffer-modified-p) (propertize "ED" 'face 'mode-line-modified-face))
          (t "  ")))

  (telephone-line-defsegment* lrd/telephone-line-network-segment ()
    (cond ((and buffer-file-name (file-remote-p buffer-file-name) server-buffer-clients) "<")
          (server-buffer-clients "^")
          ((and buffer-file-name (file-remote-p buffer-file-name)) "@")
          (t "")))

  (telephone-line-defsegment* lrd/telephone-line-position-segment ()
    (when (bound-and-true-p line-number-mode)
      "%l:%c")
    )

  (telephone-line-defsegment lrd/telephone-line-flymake-segment ()
    (when (bound-and-true-p flymake-mode)
      (let* ((text (let-alist (lrd/flymake-count-errors)
                     (if (or .error .warning .note)
                         (propertize (format "!:%s ?:%s i:%s"
                                             (or .error 0) (or .warning 0) (or .note 0))
                                     'face 'telephone-line-warning)
                       (propertize ":)" 'face 'telephone-line-unimportant)))))
        text
        )))
  
  (setq telephone-line-lhs
        '((nil . (lrd/telephone-line-buffer-modified-segment
                     lrd/telephone-line-network-segment))
          (accent . (telephone-line-buffer-name-segment
                  telephone-line-simple-major-mode-segment
                  telephone-line-process-segment
                  telephone-line-simple-minor-mode-segment))
          (nil . (lrd/telephone-line-flymake-segment
                  telephone-line-misc-info-segment))
          ))
  (setq telephone-line-rhs
        '((nil . ())
          (accent . (telephone-line-vc-segment
                     lrd/telephone-line-position-segment))
          ))

  (telephone-line-mode 1))

(provide 'config-telephone-line)
