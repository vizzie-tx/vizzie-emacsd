;;; config-keys.el --- Misc global key bindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-09-02
;;
;; Commentary:
;; 
;; Code:

;; Toggle the dedicated status of a window, to prevent it from being reused
(defun lrd/toggle-window-dedicated () nil
       (interactive)
       (set-window-dedicated-p
	(get-buffer-window)
	(not (window-dedicated-p (get-buffer-window)))))

;; Set up a personal prefix on "C-x c" for useful shortcuts
(define-prefix-command 'lrd/personal-map)
 
(define-key 'lrd/personal-map (kbd "SPC") #'just-one-space)
(define-key 'lrd/personal-map (kbd "l") #'lrd/toggle-window-dedicated)
(define-key 'lrd/personal-map (kbd "v") #'vterm)
(define-key 'lrd/personal-map (kbd "m") #'mc/mark-all-dwim)

(use-package emacs
  :ensure nil
  :bind
  (:map global-map
    ("C-x C-c" . nil) ; avoid accidentally exiting Emacs
    ("C-x C-c C-c" . save-buffers-kill-emacs) ; more cumbersome, less error-prone
    ("C-x C-r" . restart-emacs) ; override `find-file-read-only'
    ("C-x C-b" . #'ibuffer)
    ("C-;" . #'other-window)
    ("M-`" . nil)
    )
  )

;; Set up prefix for personal bindings on an unused key
(global-set-key (kbd "C-c l") 'lrd/personal-map)

;; For reference:
;; C-t is bound to transpose-chars
(provide 'config-keys)
