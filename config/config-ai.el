;;; config-ai.el --- AI in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2026-01-07
;;
;; Commentary:
;; 
;; Code:

(use-package popup
  :ensure t)

(use-package eat
  :ensure t)

(use-package projectile
  :ensure t)

(use-package gemini-cli
  :ensure t
  :vc (:url "https://github.com/linchen2chris/gemini-cli.el" :rev :newest))

(use-package ai-code
  :config
  (ai-code-set-backend 'gemini)
  (global-set-key (kbd "C-c C-a") #'ai-code-menu)
  (global-auto-revert-mode 1)
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

(provide 'config-ai)

