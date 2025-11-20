;;; config-languages.el --- Support for programming languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-09-02
;;
;; Commentary:
;; 
;; Code:

;;; Higher-order packages
;;;
;; eglot - Connect to LSP server for all the goodies it has
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio")
               '(ng2-html-mode
                 "ngserver"
                 "--stdio"
			     "--tsProbeLocations"
			     "/usr/local/lib/node_modules/typescript/lib"
			     "--ngProbeLocations"
			     "/usr/local/lib/node_modules/@angular/language-server/bin"))
  (setq-default
   eglot-workspace-configuration
   '( :basedpyright
      ( :typeCheckingMode "recommended")
      :basedpyright.analysis
      ( :diagnosticSeverityOverrides
        (:reportExplicitAny "information")
        )
      )
   )
  :bind
  (:map eglot-mode-map
        ("C-c r" . 'eglot-rename)
        ("C-c a" . 'eglot-code-actions)
        ("C-c o" . 'eglot-code-action-organize-imports)
        ("C-c q" . 'eglot-code-action-quickfix)
        ("C-c w" . 'eglot-code-action-rewrite)
        ("C-c x" . 'eglot-code-action-extract)
        ("C-c d" . 'eglot-find-declaration)
        ("C-c D" . 'eglot-find-typeDefinition)
        ("C-c i" . 'eglot-find-implementation)
        ;; available: eglot-format / eglot-format-buffer
        ("C-c h" . 'eldoc)
	    )
  )

;; Configure flymake, use M-n and M-p to move between errors
(use-package flymake
  :functions flymake-goto-next-error flymake-goto-prev-error
  :bind
  (:map flymake-mode-map
   ("M-n" . 'flymake-goto-next-error)
   ("M-p" . 'flymake-goto-prev-error))
  :config (setq flymake-mode-line-format nil)
  )

(defun lrd/flymake-count-errors ()
  "Return a list of the flymake error, warn, and note counts"
  (let ((flymake-counters-alist ()))
    (dolist (d (flymake-diagnostics))
      (let* ((sev (flymake--severity (flymake-diagnostic-type d)))
             (level (cond ((eq sev (flymake--severity :error)) 'error)
                          ((eq sev (flymake--severity :warning)) 'warning)
                          (t 'info)))
             (item (assq level flymake-counters-alist)))
        (if item
            (cl-incf (cdr item))
          (push (cons level 1) flymake-counters-alist))))
    flymake-counters-alist))


;; Languages: angular, typescript, html, css, javascript, java, rust?
;; Web-mode: Enhanced HTML mode that can handle embedded javascript and CSS sensibly
(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.x?html?\\'" . web-mode))
  :bind
  (:map web-mode-map
        ("C-c /" . web-mode-element-close)
        )
  :config
  (customize-set-variable 'web-mode-enable-css-colorization t)
  (customize-set-variable 'web-mode-enable-auto-pairing t))

;; javascript
(use-package js)

;; All C-like languages, java, C, C++. Make sure we use eglot
(use-package cc-mode
  :config
  (setq c-default-style '((java-mode . "java") (other . "linux")))
  (setq c-basic-offset 4)
  (setq c-recognize-knr-p nil)

  :hook
  (c-mode-common . eglot-ensure)
  )

;; Typescript. Use the typescript lsp with eglot.
(use-package typescript-mode
  :after eglot
  :hook
  (typescript-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  )

;; ng2-mode for Angular.
;; The ngserver program is only for the html template files.
;; typescript component files are handled capably by the pre-configured
;; typescript LSP.
(use-package ng2-mode
  :hook
  (ng2-ts-mode . eglot-ensure)
  (ng2-html-mode . eglot-ensure)
  )

;; Fix the order of ng2-mode and web-mode so that Angular gets first crack
;; at handling its files
(defun lrd/ngmode-p (cell)
  "Is the cons cell an ng2-mode alist element"
  (or (eq (cdr cell) 'ng2-html-mode)
      (eq (cdr cell) 'ng2-ts-mode)))

(setq auto-mode-alist (let ((ngmodes
                             (seq-filter 'lrd/ngmode-p auto-mode-alist)))
                        (append ngmodes
                                (seq-remove 'lrd/ngmode-p auto-mode-alist))))

;;;;; Python
;;;;;; Basic Setup
(use-package python
  :after eglot
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "python3")
  (setq python-interpreter "python3")
  :hook
  (python-mode . eglot-ensure)
)


;;;;;;
;; https://gist.github.com/Nathan-Furnal/b327f14e861f009c014af36c1790ec49
;;<OPTIONAL> I use poetry (https://python-poetry.org/) to manage my python environments.
;; See: https://github.com/galaunay/poetry.el.
;; There are alternatives like https://github.com/jorgenschaefer/pyvenv.
(use-package poetry
  :ensure t
  :defer t
  :config
  ;; Checks for the correct virtualenv. Better strategy IMO because the default
  ;; one is quite slow.
  (setq poetry-tracking-strategy 'switch-buffer)
  :hook
  (python-mode . poetry-tracking-mode)
  )

;;;;; Emacs Lisp
;; Set up outline minor mode by default. 
(use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode . (lambda () (outline-minor-mode t))))

;;;;; Rust
(use-package rustic
  :after eglot
  :config
  (setq rustic-lsp-client 'eglot)
  (add-to-list 'eglot-server-programs
               `(rust-mode . ("rust-analyzer" :initializationOptions
                              ( :procMacro (:enable t)
                                :cargo ( :buildScripts (:enable t)
                                         :features "all"))))))

(provide 'config-languages)
;; config-languages.el ends here
