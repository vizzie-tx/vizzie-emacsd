;;; config-languages.el --- Support for programming languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-09-02
;;
;; Commentary:
;; 
;; Code:

;; configure python lsp server
(defvar lrd/python-lang-server 'rass)
(setq lrd/python-lang-server 'basedrass)
;; (setq lrd/python-lang-server 'basedpyright)


;;; Higher-order packages
;;;

;; Detect some additional project roots
(load-library "project-rootfile.el")
(add-hook 'project-find-functions 'project-rootfile-try-detect)
  
(defvar lrd/project-compilation-alist
  '(("pyproject.toml" . "python3 -m build")
    ("uv.lock" . "uv build")
    ("poetry.lock" . "poetry build")
    ("build.xml" . "ant")
    ("angular.json" . "ng build")))

(defun lrd/build-command ()
  (let ((dir (project-root (project-current)))
        project-compile-command)
    (dolist (pair lrd/project-compilation-alist)
      (let ((file (car pair))
            (cmd (cdr pair)))
        (and (file-exists-p (file-name-concat dir file))
             (setq project-compile-command cmd))))
    project-compile-command))

(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers
        '("pyproject.toml" "pom.xml" "build.xml" "Cargo.toml" "build.gradle" "angular.json"))
  (defadvice project-compile
      (before project-set-compile-command activate)
    "Configure project compile commands based on file existence"
    (let ((pcc (lrd/build-command)))
      (and pcc (setq-local compile-command pcc)))))

(defun lrd/get-python-lsp-command ()
  (pcase lrd/python-lang-server
    ('ty '("ty" "server"))
    ('rass '("rass" "python"))
    ('basedrass '("rass" "basedruff"))
    ('basedpyright '("basedpyright-langserver" "--stdio"
                     :initializationOptions
                     (:basedpyright
                      (:typeCheckingMode: "strict"))))))
         
;; eglot - Connect to LSP server for all the goodies it has
(use-package eglot
  :ensure t
  :config
  (dolist (cfg '(((python-mode python-ts-mode)
                  . (lambda (&rest _) (lrd/get-python-lsp-command)))
                 (ng2-html-mode
                   "ngserver"
                   "--stdio"
			       "--tsProbeLocations"
			       "/usr/local/lib/node_modules/typescript/lib"
			       "--ngProbeLocations"
			       "/usr/local/lib/node_modules/@angular/language-server/bin")
                 (text-mode . ("harper-ls" "--stdio"))
                 ))
    (add-to-list 'eglot-server-programs cfg))
  (setq-default
   eglot-workspace-configuration
   '( :basedpyright
      ( :typeCheckingMode "recommended"
        :analysis
        ( :diagnosticSeverityOverrides
          (:reportExplicitAny "information")
        )
      )
   ))
  (dolist (face '(eglot-inlay-hint-face eglot-parameter-hint-face eglot-type-hint-face))
    (set-face-attribute face nil :family "Monaspace Radon Frozen"))
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
  :config
  (setq flymake-mode-line-format nil)
  (setq flymake-wrap-around t)
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

;; All C-like languages, java, C, C++. Make sure we use eglot
(use-package cc-mode
  :config
  (setq c-default-style '((java-mode . "java") (other . "linux")))
  (setq c-basic-offset 4)
  (setq c-recognize-knr-p nil)

  :hook
  (c-mode-common . eglot-ensure)
  )

(use-package c-ts-mode
  :init
  (add-to-list 'auto-mode-alist
               '("\\(\\.ii\\|\\.\\(CC?\\|HH?\\)\\|\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\|\\.\\(cc\\|hh\\)\\)\\'"
                 . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode c++-ts-mode))
  :config
  (setq c-ts-mode-indent-style 'linux)
  (setq c-ts-mode-indent-offset 4)
  :hook
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  )

(use-package java-ts-mode
  :init
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  :hook
  (java-ts-mode . eglot-ensure)
  )

;; Typescript. Use the typescript lsp with eglot.
(use-package typescript-mode
  :after eglot
  :init
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
  :hook
  (typescript-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  )

;; ng2-mode for Angular.
;; The ngserver program is only for the html template files.
;; typescript component files are handled capably by the pre-configured
;; typescript LSP.
(use-package ng2-mode
  :after web-mode
  :hook
  (ng2-ts-mode . eglot-ensure)
  (ng2-html-mode . eglot-ensure)
  )

;;;;; Python
;;;;;; Basic Setup
(use-package python
  :after eglot
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "python3")
  (setq python-interpreter "python3")
  :hook
  (python-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
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
  (python-ts-mode . poetry-tracking-mode)
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

;;;;; Javascript
(use-package js
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
  :hook
  (js-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  )

;;;; Remap some languages to use appropriate -ts mode
(dolist (setting '((javascript-mode . js-ts-mode)
                   (json-mode . json-ts-mode)
                   (css-mode . css-ts-mode)))
  (add-to-list 'major-mode-remap-alist setting))


(provide 'config-languages)
;; config-languages.el ends here
