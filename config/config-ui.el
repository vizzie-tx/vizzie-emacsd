;;; config-ui.el --- UI tweaks and enhancements  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2024-09-02
;;
;; Commentary:
;; 
;; Code:

;; When killing a line, take the newline with it
(setq kill-whole-line t)

;; Delight - diminish minor modes
(use-package delight
  :init
  (delight '((abbrev-mode "A")
             (eldoc-mode nil "E")
             (overwrite-mode "O")
             (outline-minor-mode "*" outline)
             (emacs-lisp-mode "elisp " :major)
	         (lisp-interaction-mode "i-lisp " :major))
           )
  )
(setq eldoc-minor-mode-string "E")
(setq auto-revert-mode-text "R")

;; Recognize individual words in CamelCase and dash-separated
;; variable names
(use-package subword
  :delight "-"
  :init
  (global-subword-mode 1))

;; Use ligatures for fonts that have them available.
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Enable fancy icons where available
(use-package all-the-icons)

;; Show column number along with line number in modeline
(column-number-mode 1)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Ain't nobody got time for that
(fset 'yes-or-no-p 'y-or-n-p) ; less typing

(setq show-paren-context-when-offscreen 'overlay)

;; Nifty trick; highlight region when saving to kill-ring
(defun lrd/pulse-current-region (&rest _)
  "Pulse the current implicit or active region"
  (if mark-active
      (pulse-momentary-highlight-region (region-beginning) (region-end))
    (pulse-momentary-highlight-region (mark) (point))))

(advice-add #'kill-ring-save :before #'lrd/pulse-current-region)

(provide 'config-ui)
;; config-ui.el ends here

