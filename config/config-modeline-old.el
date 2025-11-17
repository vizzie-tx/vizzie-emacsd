;;; config-modeline.el --- Mode line config  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-09-11
;;
;; Commentary:
;; 
;; Code:

(make-face 'mode-line-modified-face)
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'font-lock-warning-face :weight 'bold :reverse-video t)
(make-face 'mode-line-read-only-face)
(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'font-lock-constant-face :weight 'bold :reverse-video t)

(setq-default mode-line-modified
              (list
               '(:eval (cond
                        (buffer-read-only
                         (propertize "RO" 'face 'mode-line-read-only-face
                                     'help-echo "Buffer is read-only"))
                        ((buffer-modified-p)
                         (propertize "ED" 'face 'mode-line-modified-face
                                     'help-echo "Buffer has beem modified"))
                        (t "  ")))))

(defvar-local mode-line-network 
    (list 
     '(:eval (cond 
              ((and server-buffer-clients
                    (file-remote-p buffer-file-name)) "<")
              (server-buffer-clients "^")
              ((file-remote-p buffer-file-name) "@")
              (t " ")))))
(put 'mode-line-network 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                (:propertize
                 ("" mode-line-modified mode-line-network))
                mode-line-buffer-identification "  "
                mode-line-modes
                (global-mode-line-string ("" global-mode-line-string))
                (vc-mode vc-mode)
                (line-number-mode "%l:%c")
                ))

(provide 'config-mode-line)

