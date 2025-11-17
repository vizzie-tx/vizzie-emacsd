;; default {'background': '#28211C', 'caret': '#A7A7A7', 'foreground': '#BAAE9E', 'invisibles': '#FFFFFF40', 'lineHighlight': '#FFFFFF08', 'selection': '#DDF0FF33'}
;; comment {'fontStyle': 'italic', 'foreground': '#666666'}
;; constant {'foreground': '#CF6A4C'}
;; entity {'fontStyle': '', 'foreground': '#937121'}
;; keyword {'fontStyle': '', 'foreground': '#5EA6EA'}
;; storage {'fontStyle': '', 'foreground': '#F9EE98'}
;; string {'fontStyle': '', 'foreground': '#54BE0D'}
;; support {'fontStyle': '', 'foreground': '#9B859D'}
;; variable {'foreground': '#7587A6'}
;; invalid.deprecated {'fontStyle': 'italic underline', 'foreground': '#D2A8A1'}
;; invalid.illegal {'background': '#562D56BF', 'foreground': '#F8F8F8'}
;; default {}
;; text source {'background': '#B0B3BA14'}
;; text.html.ruby source {'background': '#B1B3BA21'}
;; entity.other.inherited-class {'fontStyle': 'italic', 'foreground': '#9B5C2E'}
;; string source {'fontStyle': '', 'foreground': '#DAEFA3'}
;; string constant {'foreground': '#DDF2A4'}
;; string.regexp {'fontStyle': '', 'foreground': '#E9C062'}
;; string.regexp constant.character.escape, string.regexp source.ruby.embedded, string.regexp string.regexp.arbitrary-repitition {'foreground': '#CF7D34'}
;; string variable {'foreground': '#8A9A95'}
;; support.function {'fontStyle': '', 'foreground': '#DAD085'}
;; support.constant {'fontStyle': '', 'foreground': '#CF6A4C'}
;; meta.preprocessor.c {'foreground': '#8996A8'}
;; meta.preprocessor.c keyword {'foreground': '#AFC4DB'}
;; meta.tag.sgml.doctype, meta.tag.sgml.doctype entity, meta.tag.sgml.doctype string, meta.tag.preprocessor.xml, meta.tag.preprocessor.xml entity, meta.tag.preprocessor.xml string {'foreground': '#5EA6EA'}
;; declaration.tag, declaration.tag entity, meta.tag, meta.tag entity {'foreground': '#AC885B'}
;; meta.selector.css entity.name.tag {'fontStyle': '', 'foreground': '#CDA869'}
;; meta.selector.css entity.other.attribute-name.tag.pseudo-class {'foreground': '#8F9D6A'}
;; meta.selector.css entity.other.attribute-name.id {'foreground': '#8B98AB'}
;; meta.selector.css entity.other.attribute-name.class {'foreground': '#9B703F'}
;; support.type.property-name.css {'foreground': '#C5AF75'}
;; meta.property-group support.constant.property-value.css, meta.property-value support.constant.property-value.css {'foreground': '#F9EE98'}
;; meta.preprocessor.at-rule keyword.control.at-rule {'foreground': '#8693A5'}
;; meta.property-value support.constant.named-color.css, meta.property-value constant {'foreground': '#CA7840'}
;; meta.constructor.argument.css {'foreground': '#8F9D6A'}
;; meta.diff, meta.diff.header, meta.separator {'background': '#0E2231', 'fontStyle': 'italic', 'foreground': '#F8F8F8'}
;; markup.deleted {'background': '#420E09', 'foreground': '#F8F8F8'}
;; markup.changed {'background': '#4A410D', 'foreground': '#F8F8F8'}
;; markup.inserted {'background': '#253B22', 'foreground': '#F8F8F8'}
;; markup.list {'foreground': '#F9EE98'}
;; markup.heading {'foreground': '#CF6A4C'}
;; entity.name.tag {'foreground': '#5EA6EA'}
(deftheme bespin
  "Bespin emacs theme autoconverted from the textMate theme by Michael Diolosa")
(custom-theme-set-faces
  'bespin
    '(trailing-whitespace ((t (:background "#5e5955"))))
    '(whitespace-trailing ((t (:background "#5e5955"))))

    '(border ((t (:background "#2f2823" ))))
    '(css-property ((t (:foreground "#c5af75" ))))
    '(css-proprietary-property ((t (:foreground "#c5af75" ))))
    '(css-selector ((t (:foreground "#8b98ab" ))))
    '(cursor ((t (:background "#a7a7a7" ))))
    '(default ((t (:background "#28211c" :foreground "#baae9e" ))))
    '(diff-added ((t (:background "#253b22" :foreground "#f8f8f8" ))))
    '(diff-changed ((t (:background "#4a410d" :foreground "#f8f8f8" ))))
    '(diff-header ((t (:background "#0e2231" :italic t :foreground "#f8f8f8" ))))
    '(diff-hunk-header ((t (:background "#0e2231" :italic t :foreground "#f8f8f8" ))))
    '(diff-removed ((t (:background "#420e09" :foreground "#f8f8f8" ))))
    '(font-lock-builtin-face ((t (:foreground "#dad085" ))))
    '(font-lock-comment-delimiter-face ((t (:italic t :foreground "#666666" ))))
    '(font-lock-comment-face ((t (:italic t :foreground "#666666" ))))
    '(font-lock-constant-face ((t (:foreground "#cf6a4c" ))))
    '(font-lock-doc-face ((t (:foreground "#ddf2a4" ))))
    '(font-lock-function-name-face ((t (:foreground "#dad085" ))))
    '(font-lock-keyword-face ((t (:foreground "#5ea6ea" ))))
    '(font-lock-negation-char-face ((t (:foreground "#5ea6ea" ))))
    '(font-lock-preprocessor-face ((t (:foreground "#8996a8" ))))
    '(font-lock-regexp-grouping-backslash ((t (:foreground "#cf7d34" ))))
    '(font-lock-regexp-grouping-construct ((t (:foreground "#cf7d34" ))))
    '(font-lock-string-face ((t (:foreground "#ddf2a4" ))))
    '(font-lock-type-face ((t (:foreground "#f9ee98" ))))
    '(font-lock-variable-name-face ((t (:foreground "#7587a6" ))))
    '(font-lock-warning-face ((t (:background "#4a2a47" :foreground "#f8f8f8" ))))
    '(fringe ((t (:background "#2f2823" ))))
    '(highlight ((t (:background "#4c4a49" ))))
    '(hl-line ((t (:background "#2f2823" ))))
    '(isearch ((t (:background "#4a410d" :foreground "#f8f8f8" ))))
    '(isearch-fail ((t (:background "#420e09" :foreground "#f8f8f8" ))))
    '(iswitchb-current-match ((t (:background "#253b22" :foreground "#f8f8f8" ))))
    '(iswitchb-invalid-regexp ((t (:background "#420e09" :foreground "#f8f8f8" ))))
    '(iswitchb-single-match ((t (:background "#253b22" :foreground "#f8f8f8" ))))
    '(iswitchb-virtual-matches ((t (:background "#4a410d" :foreground "#f8f8f8" ))))
    '(linum ((t (:background "#2f2823" ))))
    '(match ((t (:background "#253b22" :foreground "#f8f8f8" ))))
    '(menu ((t (:background "#28211c" :foreground "#baae9e" ))))
    '(minibuffer-prompt ((t (:foreground "#ac885b" ))))
    '(mode-line ((t (:background "#0e2231" :italic t :foreground "#f8f8f8" ))))
    '(mode-line-buffer-id ((t (:foreground "#5ea6ea" ))))
    '(mode-line-emphasis ((t (:bold t ))))
    '(mode-line-highlight ((t (:italic t ))))
    '(mode-line-inactive ((t (:background "#4c4a49" ))))
    '(org-agenda-dimmed-todo-face ((t (:italic t :foreground "#666666" ))))
    '(org-column ((t (:background "#2f2823" ))))
    '(org-column-title ((t (:background "#2f2823" ))))
    '(org-done ((t (:background "#253b22" :foreground "#f8f8f8" ))))
    '(org-hide ((t (:foreground "#28211c" ))))
    '(org-todo ((t (:background "#4a2a47" :foreground "#f8f8f8" ))))
    '(org-upcoming-deadline ((t (:italic t :underline t :foreground "#d2a8a1" ))))
    '(org-warning ((t (:background "#4a2a47" :foreground "#f8f8f8" ))))
    '(region ((t (:background "#4c4a49" ))))
    '(secondary-selection ((t (:background "#4c4a49" ))))
    '(show-paren-match-face ((t (:background "#4c4a49" ))))
    '(show-paren-mismatch-face ((t (:background "#4a2a47" :foreground "#f8f8f8" ))))
)

(provide-theme 'bespin)
