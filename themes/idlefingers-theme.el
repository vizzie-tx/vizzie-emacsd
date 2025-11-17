;; default {'background': '#323232', 'caret': '#91FF00', 'foreground': '#FFFFFF', 'invisibles': '#686868', 'lineHighlight': '#343536', 'selection': '#5A647EE0'}
;; text {'foreground': '#FFFFFF'}
;; source {'background': '#282828', 'foreground': '#CDCDCD'}
;; comment {'fontStyle': 'italic', 'foreground': '#BC9458'}
;; meta.tag, declaration.tag, meta.doctype {'foreground': '#FFE5BB'}
;; entity.name {'foreground': '#FFC66D'}
;; source.ruby entity.name {'foreground': '#FFF980'}
;; variable.other {'foreground': '#B7DFF8'}
;; support.class.ruby {'foreground': '#CCCC33'}
;; constant, support.constant {'foreground': '#6C99BB'}
;; keyword {'fontStyle': '', 'foreground': '#CC7833'}
;; other.preprocessor.c {'fontStyle': '', 'foreground': '#D0D0FF'}
;; entity.name.preprocessor {'fontStyle': ''}
;; entity.name.function {'fontStyle': ''}
;; variable.parameter {'fontStyle': 'italic'}
;; source comment.block {'background': '#575757', 'foreground': '#FFFFFF'}
;; string {'foreground': '#A5C261'}
;; string constant.character.escape {'foreground': '#AAAAAA'}
;; string.interpolated {'background': '#202020', 'foreground': '#AAAAAA'}
;; string.regexp {'foreground': '#CCCC33'}
;; string.literal {'foreground': '#CCCC33'}
;; string.interpolated constant.character.escape {'background': '#212121', 'foreground': '#787878'}
;; entity.name.class {'fontStyle': 'underline'}
;; entity.other.inherited-class {'fontStyle': 'italic underline'}
;; entity.name.tag {'fontStyle': ''}
;; entity.other.attribute-name {'fontStyle': ''}
;; support.function {'fontStyle': '', 'foreground': '#B83426'}
;; markup.list.unnumbered.textile {'foreground': '#6EA533'}
;; markup.list.numbered.textile {'foreground': '#6EA533'}
;; markup.bold.textile {'fontStyle': 'bold', 'foreground': '#C2C2C2'}
;; invalid {'background': '#FF0000', 'fontStyle': '', 'foreground': '#FFFFFF'}
(deftheme idlefingers
  "idleFingers emacs theme autoconverted from the textMate theme by unknown")
(custom-theme-set-faces
  'idlefingers
    '(trailing-whitespace ((t (:background "#686868"))))
    '(whitespace-trailing ((t (:background "#686868"))))

    '(border ((t (:background "#343536" ))))
    '(cursor ((t (:background "#91ff00" ))))
    '(default ((t (:background "#323232" :foreground "#ffffff" ))))
    '(font-lock-builtin-face ((t (:foreground "#b83426" ))))
    '(font-lock-comment-delimiter-face ((t (:italic t :foreground "#bc9458" ))))
    '(font-lock-comment-face ((t (:italic t :foreground "#bc9458" ))))
    '(font-lock-constant-face ((t (:foreground "#6c99bb" ))))
    '(font-lock-doc-face ((t (:foreground "#a5c261" ))))
    '(font-lock-function-name-face ((t (:foreground "#b83426" ))))
    '(font-lock-keyword-face ((t (:foreground "#cc7833" ))))
    '(font-lock-negation-char-face ((t (:foreground "#cc7833" ))))
    '(font-lock-regexp-grouping-backslash ((t (:foreground "#cccc33" ))))
    '(font-lock-regexp-grouping-construct ((t (:foreground "#cccc33" ))))
    '(font-lock-string-face ((t (:foreground "#a5c261" ))))
    '(font-lock-variable-name-face ((t (:foreground "#b7dff8" ))))
    '(font-lock-warning-face ((t (:background "#ff0000" :foreground "#ffffff" ))))
    '(fringe ((t (:background "#343536" ))))
    '(highlight ((t (:background "#555e75" ))))
    '(hl-line ((t (:background "#343536" ))))
    '(linum ((t (:background "#343536" ))))
    '(menu ((t (:background "#323232" :foreground "#ffffff" ))))
    '(minibuffer-prompt ((t (:foreground "#ffe5bb" ))))
    '(mode-line ((t (:background "#555e75" ))))
    '(mode-line-buffer-id ((t ())))
    '(mode-line-emphasis ((t (:bold t ))))
    '(mode-line-highlight ((t (:italic t ))))
    '(mode-line-inactive ((t (:background "#343536" ))))
    '(org-agenda-dimmed-todo-face ((t (:italic t :foreground "#bc9458" ))))
    '(org-column ((t (:background "#343536" ))))
    '(org-column-title ((t (:background "#343536" ))))
    '(org-hide ((t (:foreground "#323232" ))))
    '(org-todo ((t (:background "#ff0000" :foreground "#ffffff" ))))
    '(org-upcoming-deadline ((t (:background "#ff0000" :foreground "#ffffff" ))))
    '(org-warning ((t (:background "#ff0000" :foreground "#ffffff" ))))
    '(region ((t (:background "#555e75" ))))
    '(secondary-selection ((t (:background "#555e75" ))))
    '(show-paren-match-face ((t (:background "#555e75" ))))
    '(show-paren-mismatch-face ((t (:background "#ff0000" :foreground "#ffffff" ))))
)

(provide-theme 'idlefingers)
