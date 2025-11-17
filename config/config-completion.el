;;; config-completion.el --- Minibuffer and buffer completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-09-02
;;
;; Commentary:
;;
;;   We use vertico, orderless and marginalia to provide a pleasant minibuffer experience
;;   For in-buffer completion, we use corfu to provide a popup interface to
;;   completion-at-point and hippie-expand to expand things we don't want in our way, such
;;   as dabbrev and yasnippet.
;;
;; Code:

;;;; Minibuffer Completion
;;;;; Vertico
;; Vertico displays potential minibuffer completions as a vertical
;; list, under the modeline. 
(use-package vertico
  :init
  (vertico-mode)
  :bind
  ;; No modifier: tab expands longest common prefix, return accepts selected completion
  ;; Meta-modifier: tab expands to the current completion, return accepts minibuffer contents
  (:map vertico-map
        ("M-TAB" . #'vertico-insert)
        ("TAB" . #'minibuffer-complete))
)

;;;;;; Savehist
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;;;;; Orderless
;; Orderless - Space-separated orderless substring search for minibuffer completion
(use-package orderless
  :ensure t
  :custom
  ;; basic will match mostly like you expect.
  ;; orderless matches space-separated substrings in any order
  ;; Substring is present for partial tab-complete to function
  (completion-styles '(substring orderless basic))
  ;; When matching files, match in a more orderly manner
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;;; Marginalia
;; Marginalia uses the extra space to the right of the vertico list
;; to display all manner of useful contextual information to the
;; right of the matches. A very powerful combination
(use-package marginalia
  :init
  (marginalia-mode 1))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  )

;;;; in-buffer completion and expansion
;;
;; Separate 2 concepts here.
;;   EXPANSION is deliberate and only happens when triggered.
;;   COMPLETION is incidental and happens any time emacs thinks it knows what we want next.


;;;;; EXPANSION
;; On-demand" style, triggered by a key combination
;;;;;;; yasnippet
(use-package yasnippet
  :demand nil
  :delight yas-minor-mode "y"
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil))
  :config
  (yas-global-mode t)
  (yas-reload-all)
  :hook
  (term-mode . (lambda() (yas-minor-mode -1)))
) 

;;;;;; yasnippet-snippets
;; a large list of snippets for yasnippet. 
(use-package yasnippet-snippets
  :after yasnippet
  )

;;;;;; hippie-expand
;; subsumes and replaces dabbrev-expand for expansion
(use-package hippie-exp
  :ensure nil
  :bind ("M-/" . hippie-expand)
  :init
  (setq hippie-expand-try-functions-list

        '(yas-hippie-try-expand
          try-complete-file-name-partially
          try-complete-file-name
          ;try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-expand-dabbrev
          ))
  )


;;;;; COMPLETION
;; The "Push" version of expansion. Leverage suggestions from contextual expertise

;;;;;; Corfu
;; Corfu gives a nice pop-up interface to completion-at-point. 
(use-package corfu
  :ensure t
  :demand t                      ; need this when using :bind or :hook
  :config
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-quit-no-match t) ; quit when the popup appears and I type anything else
  ;; Might want to customize corfu-sort-function
  :bind
  (("M-RET" . completion-at-point)
   )
  )


;;;;;; cape
;;extended list of completion-at-point functions to make corfu more useful
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  ;; :bind ("C-c c" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;;(add-hook 'completion-at-point-functions #'cape-dabbrev)
  (defun lrd/elisp-add-cape () nil
         (add-hook 'completion-at-point-functions #'cape-file)
         (add-hook 'completion-at-point-functions #'cape-elisp-block)
         (add-hook 'completion-at-point-functions #'cape-sgml)
         (add-hook 'completion-at-point-functions #'cape-emoji)

  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-sgml)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'emacs-lisp-mode-hook #'lrd/elisp-add-cape)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )
)

;;;;; More completion-adjacent, but... close enough.
;;;;;; Which-key
;; Display next key options for prefixes
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


(provide 'config-completion)
;;; Postscript
;; Local variables:
;; outline-regexp: "^;;;;*"
;; End:
;; config-completion.el ends here
