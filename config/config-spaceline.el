;; Spaceline configuration options -*- lexical-binding:t -*-
(use-package spaceline :ensure t
  :defer nil
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'curve
	spaceline-highlight-face-func 'spaceline-highlight-face-modified
	spaceline-minor-modes-separator nil
	spaceline-buffer-size-p nil
	spaceline-buffer-encoding-abbrev-p nil
	spaceline-input-method-p nil
	spaceline-buffer-position-p nil)
  )

(require 'config-spaceline-mod)

(provide 'config-spaceline)
;; config-spaceline.el ends here
