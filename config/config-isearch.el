;;; config-isearch.el --- Search in buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-09-02
;;
;; Commentary:
;; 
;; Code:

;;;; Hook function definition
;; This function runs as a hook to return the cursor to the start
;; of the match when exiting isearch normally
(defun lrd/goto-match-beginning ()
    "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
    (when (and isearch-forward
	       (number-or-marker-p isearch-other-end)
	       (not mark-active)
	       (not isearch-mode-end-hook-quit))
          (goto-char isearch-other-end)))

;;;; ISearch
;; Configuration for isearch.
(use-package isearch
  :ensure nil
  :init
  (setq isearch-allow-scroll 1)
  (add-hook 'isearch-mode-end-hook #'lrd/goto-match-beginning))

(provide 'config-isearch)
;;; Postscript
;; Local variables:
;; outline-regexp: "^;;;;*"
;; End:
;; config-isearch.el ends here
