;; default {'background': '#272822', 'caret': '#F8F8F0', 'foreground': '#F8F8F2', 'invisibles': '#49483E', 'lineHighlight': '#49483E', 'selection': '#49483E'}
;; comment {'foreground': '#75715E'}
;; string {'foreground': '#E6DB74'}
;; constant.numeric {'foreground': '#AE81FF'}
;; constant.language {'foreground': '#AE81FF'}
;; constant.character, constant.other {'foreground': '#AE81FF'}
;; variable {'fontStyle': ''}
;; keyword {'foreground': '#F92672'}
;; storage {'fontStyle': '', 'foreground': '#F92672'}
;; storage.type {'fontStyle': 'italic', 'foreground': '#66D9EF'}
;; entity.name.class {'fontStyle': 'underline', 'foreground': '#A6E22E'}
;; entity.other.inherited-class {'fontStyle': 'italic underline', 'foreground': '#A6E22E'}
;; entity.name.function {'fontStyle': '', 'foreground': '#A6E22E'}
;; variable.parameter {'fontStyle': 'italic', 'foreground': '#FD971F'}
;; entity.name.tag {'fontStyle': '', 'foreground': '#F92672'}
;; entity.other.attribute-name {'fontStyle': '', 'foreground': '#A6E22E'}
;; support.function {'fontStyle': '', 'foreground': '#66D9EF'}
;; support.constant {'fontStyle': '', 'foreground': '#66D9EF'}
;; support.type, support.class {'fontStyle': 'italic', 'foreground': '#66D9EF'}
;; support.other.variable {'fontStyle': ''}
;; invalid {'background': '#F92672', 'fontStyle': '', 'foreground': '#F8F8F0'}
;; invalid.deprecated {'background': '#AE81FF', 'foreground': '#F8F8F0'}
(deftheme monokai
  "Monokai emacs theme autoconverted from the textMate theme by unknown")
(custom-theme-set-faces
  'monokai
    '(trailing-whitespace ((t (:background "#49483e"))))
    '(whitespace-trailing ((t (:background "#49483e"))))

    '(border ((t (:background "#49483e" ))))
    '(css-property ((t (:italic t :foreground "#66d9ef" ))))
    '(css-proprietary-property ((t (:italic t :foreground "#66d9ef" ))))
    '(cursor ((t (:background "#f8f8f0" ))))
    '(default ((t (:background "#272822" :foreground "#f8f8f2" ))))
    '(font-lock-builtin-face ((t (:foreground "#66d9ef" ))))
    '(font-lock-comment-delimiter-face ((t (:foreground "#75715e" ))))
    '(font-lock-comment-face ((t (:foreground "#75715e" ))))
    '(font-lock-constant-face ((t (:foreground "#66d9ef" ))))
    '(font-lock-doc-face ((t (:foreground "#e6db74" ))))
    '(font-lock-function-name-face ((t (:foreground "#66d9ef" ))))
    '(font-lock-keyword-face ((t (:foreground "#f92672" ))))
    '(font-lock-negation-char-face ((t (:foreground "#f92672" ))))
    '(font-lock-regexp-grouping-backslash ((t (:foreground "#e6db74" ))))
    '(font-lock-regexp-grouping-construct ((t (:foreground "#e6db74" ))))
    '(font-lock-string-face ((t (:foreground "#e6db74" ))))
    '(font-lock-type-face ((t (:italic t :foreground "#66d9ef" ))))
    '(font-lock-variable-name-face ((t ())))
    '(font-lock-warning-face ((t (:background "#f92672" :foreground "#f8f8f0" ))))
    '(fringe ((t (:background "#49483e" ))))
    '(highlight ((t (:background "#49483e" ))))
    '(hl-line ((t (:background "#49483e" ))))
    '(linum ((t (:background "#49483e" ))))
    '(menu ((t (:background "#272822" :foreground "#f8f8f2" ))))
    '(mode-line ((t (:background "#49483e" ))))
    '(mode-line-buffer-id ((t (:foreground "#f92672" ))))
    '(mode-line-emphasis ((t (:bold t ))))
    '(mode-line-highlight ((t (:italic t ))))
    '(mode-line-inactive ((t (:background "#49483e" ))))
    '(org-agenda-dimmed-todo-face ((t (:foreground "#75715e" ))))
    '(org-column ((t (:background "#49483e" ))))
    '(org-column-title ((t (:background "#49483e" ))))
    '(org-hide ((t (:foreground "#272822" ))))
    '(org-todo ((t (:background "#f92672" :foreground "#f8f8f0" ))))
    '(org-upcoming-deadline ((t (:background "#ae81ff" :foreground "#f8f8f0" ))))
    '(org-warning ((t (:background "#f92672" :foreground "#f8f8f0" ))))
    '(region ((t (:background "#49483e" ))))
    '(secondary-selection ((t (:background "#49483e" ))))
    '(show-paren-match-face ((t (:background "#49483e" ))))
    '(show-paren-mismatch-face ((t (:background "#f92672" :foreground "#f8f8f0" ))))
)

(provide-theme 'monokai)
