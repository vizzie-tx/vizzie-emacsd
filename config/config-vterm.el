;;; config-vterm.el --- Vterm terminal config  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-09-02
;;
;; Commentary:
;; 
;; Code:

;; Vterm is a real terminal emulator, not a buffer interface to
;; a shell. More convenient for ng serve and the like.
(use-package vterm
  :ensure nil
  :config
  (setq vterm-copy-mode-remove-fake-newlines t
	vterm-copy-exclude-prompt t
	vterm-kill-buffer-on-exit t)
  :bind
  (:map vterm-mode-map
   ("C-q" . #'vterm-send-next-key))
  )

(provide 'config-vterm)
;; config-vterm.el ends here
