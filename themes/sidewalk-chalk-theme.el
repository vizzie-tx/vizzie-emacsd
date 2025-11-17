;; default {'background': '#2B2D2E', 'caret': '#7F7F7F', 'foreground': '#FAFAFA', 'invisibles': '#525252', 'lineHighlight': '#00000012', 'selection': '#C6C6C68F'}
;; comment {'background': '#212223', 'foreground': '#535353'}
;; string {'background': '#010A2E40', 'foreground': '#5F88B8'}
;; constant.numeric {'foreground': '#E5DA39'}
;; constant.language {'fontStyle': '', 'foreground': '#D56E26'}
;; constant.character, constant.other {'foreground': '#E5DA39'}
;; variable.language, variable.other {'foreground': '#55A9DB'}
;; keyword {'foreground': '#62BA4D'}
;; storage {'fontStyle': ''}
;; entity.name.class {}
;; entity.other.inherited-class {}
;; entity.name.function, keyword.operator, keyword.other.name-of-parameter {'foreground': '#A3A3A3'}
;; variable.parameter {'fontStyle': ''}
;; entity.name.tag {'fontStyle': ''}
;; entity.other.attribute-name {'fontStyle': ''}
;; support.function {'fontStyle': '', 'foreground': '#66FFCC'}
;; support.constant {'fontStyle': ''}
;; support.type, support.class {'fontStyle': ''}
;; support.other.variable {'fontStyle': ''}
;; invalid {'fontStyle': ''}
;; support.class, support.type, entity.name {'fontStyle': '', 'foreground': '#5F88B8'}
;; string.unquoted.embedded, text source, string.unquoted {'background': '#00162B40'}
;; constant.character.escaped, string source - string.unquoted.embedded, string string source {'foreground': '#999999'}
(deftheme sidewalk-chalk
  "Sidewalk Chalk emacs theme autoconverted from the textMate theme by unknown")
(custom-theme-set-faces
  'sidewalk-chalk
    '(trailing-whitespace ((t (:background "#525252"))))
    '(whitespace-trailing ((t (:background "#525252"))))

    '(border ((t (:background "#282a2b" ))))
    '(css-property ((t (:foreground "#5f88b8" ))))
    '(css-proprietary-property ((t (:foreground "#5f88b8" ))))
    '(cursor ((t (:background "#7f7f7f" ))))
    '(default ((t (:background "#2b2d2e" :foreground "#fafafa" ))))
    '(font-lock-builtin-face ((t (:foreground "#66ffcc" ))))
    '(font-lock-comment-delimiter-face ((t (:background "#212223" :foreground "#535353" ))))
    '(font-lock-comment-face ((t (:background "#212223" :foreground "#535353" ))))
    '(font-lock-constant-face ((t ())))
    '(font-lock-doc-face ((t (:background "#20242e" :foreground "#5f88b8" ))))
    '(font-lock-function-name-face ((t (:foreground "#66ffcc" ))))
    '(font-lock-keyword-face ((t (:foreground "#62ba4d" ))))
    '(font-lock-negation-char-face ((t (:foreground "#a3a3a3" ))))
    '(font-lock-regexp-grouping-backslash ((t (:background "#20242e" :foreground "#5f88b8" ))))
    '(font-lock-regexp-grouping-construct ((t (:background "#20242e" :foreground "#5f88b8" ))))
    '(font-lock-string-face ((t (:background "#20242e" :foreground "#5f88b8" ))))
    '(font-lock-type-face ((t ())))
    '(font-lock-variable-name-face ((t (:foreground "#55a9db" ))))
    '(font-lock-warning-face ((t ())))
    '(fringe ((t (:background "#282a2b" ))))
    '(highlight ((t (:background "#828383" ))))
    '(hl-line ((t (:background "#282a2b" ))))
    '(linum ((t (:background "#282a2b" ))))
    '(menu ((t (:background "#2b2d2e" :foreground "#fafafa" ))))
    '(mode-line ((t (:background "#828383" ))))
    '(mode-line-buffer-id ((t ())))
    '(mode-line-emphasis ((t (:bold t ))))
    '(mode-line-highlight ((t (:italic t ))))
    '(mode-line-inactive ((t (:background "#282a2b" ))))
    '(org-agenda-dimmed-todo-face ((t (:background "#212223" :foreground "#535353" ))))
    '(org-column ((t (:background "#282a2b" ))))
    '(org-column-title ((t (:background "#282a2b" ))))
    '(org-hide ((t (:foreground "#2b2d2e" ))))
    '(org-todo ((t ())))
    '(org-upcoming-deadline ((t ())))
    '(org-warning ((t ())))
    '(region ((t (:background "#828383" ))))
    '(secondary-selection ((t (:background "#828383" ))))
    '(show-paren-match-face ((t (:background "#828383" ))))
    '(show-paren-mismatch-face ((t ())))
)

(provide-theme 'sidewalk-chalk)
