;;; config-latex.el --- Latex configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-10-05
;;
;; Commentary:
;; 
;; Code:

(use-package auctex
  :config
  (put 'LaTeX-mode 'eglot-language-id "latex")
  (put 'ConTeXt-mode 'eglot-language-id "context")
  (put 'TeX-mode 'eglot-language-id "tex")
  :hook
  (TeX-mode . eglot-ensure)
  )

(provide 'config-tex)
