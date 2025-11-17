;; default {'background': '#1B1B1B', 'caret': '#FFFA04', 'foreground': '#F8F8F8', 'invisibles': '#CAE2FB3D', 'lineHighlight': '#FFFFFF1A', 'selection': '#1FB6DC63'}
;; comment {'fontStyle': 'italic', 'foreground': '#939393'}
;; constant.other {'fontStyle': '', 'foreground': '#F8B3AE'}
;; entity.name.function.macro {'fontStyle': '', 'foreground': '#A8D6DC'}
;; constant.other.symbol.unquoted {'foreground': '#E3FF73'}
;; constant {'fontStyle': '', 'foreground': '#EBEBEB'}
;; entity {'fontStyle': '', 'foreground': '#A5EAFC'}
;; keyword {'fontStyle': '', 'foreground': '#D1FA72'}
;; constant.numeric {'fontStyle': '', 'foreground': '#F6DD6A'}
;; keyword.operator {'fontStyle': '', 'foreground': '#EBEBEB'}
;; storage {'fontStyle': '', 'foreground': '#AFEE7F'}
;; string {'fontStyle': '', 'foreground': '#F6DD6A'}
;; support {'fontStyle': '', 'foreground': '#A5EAFC'}
;; invalid.deprecated {'fontStyle': 'italic underline', 'foreground': '#FD5FF1'}
;; variable {'fontStyle': '', 'foreground': '#9EE4FE'}
;; invalid.illegal {'background': '#562D56BF', 'foreground': '#FD5FF1'}
;; default {}
;; text source {'background': '#B1B3BA08'}
;; entity.other.inherited-class {'fontStyle': 'italic', 'foreground': '#9EE4FE'}
;; string.quoted source {'fontStyle': '', 'foreground': '#40A8B6'}
;; string constant {'foreground': '#DDF2A4'}
;; string.regexp {'foreground': '#E9C062'}
;; string.regexp constant.character.escape, string.regexp source.ruby.embedded, string.regexp string.regexp.arbitrary-repitition {'foreground': '#CF7D34'}
;; string variable {'foreground': '#8A9A95'}
;; support.function {'fontStyle': '', 'foreground': '#F4F4F4'}
;; support.constant {'fontStyle': '', 'foreground': '#FAA49C'}
;; meta.preprocessor.c {'foreground': '#8996A8'}
;; meta.preprocessor.c keyword {'foreground': '#AFC4DB'}
;; entity.name.type {'fontStyle': '', 'foreground': '#9EE4FE'}
;; meta.cast {'fontStyle': 'italic', 'foreground': '#676767'}
;; meta.sgml.html meta.doctype, meta.sgml.html meta.doctype entity, meta.sgml.html meta.doctype string, meta.xml-processing, meta.xml-processing entity, meta.xml-processing string {'foreground': '#494949'}
;; meta.tag, meta.tag entity {'foreground': '#89BDFF'}
;; source entity.name.tag, source entity.other.attribute-name, meta.tag.inline, meta.tag.inline entity {'foreground': '#E0C589'}
;; entity.name.tag.namespace, entity.other.attribute-name.namespace {'foreground': '#E18964'}
;; meta.selector.css entity.name.tag {'foreground': '#CDA869'}
;; meta.selector.css entity.other.attribute-name.tag.pseudo-class {'foreground': '#8F9D6A'}
;; meta.selector.css entity.other.attribute-name.id {'foreground': '#8B98AB'}
;; meta.selector.css entity.other.attribute-name.class {'foreground': '#9B703F'}
;; support.type.property-name.css {'foreground': '#C5AF75'}
;; meta.property-group support.constant.property-value.css, meta.property-value support.constant.property-value.css {'foreground': '#F9EE98'}
;; meta.preprocessor.at-rule keyword.control.at-rule {'foreground': '#8693A5'}
;; meta.property-value support.constant.named-color.css, meta.property-value constant {'foreground': '#DD7B3B'}
;; meta.constructor.argument.css {'foreground': '#8F9D6A'}
;; meta.diff, meta.diff.header {'background': '#0E2231', 'fontStyle': 'italic', 'foreground': '#F8F8F8'}
;; markup.deleted {'background': '#420E09', 'foreground': '#F8F8F8'}
;; markup.changed {'background': '#4A410D', 'foreground': '#F8F8F8'}
;; markup.inserted {'background': '#253B22', 'foreground': '#F8F8F8'}
;; default {}
;; markup.italic {'fontStyle': 'italic', 'foreground': '#E9C062'}
;; markup.bold {'fontStyle': 'bold', 'foreground': '#E9C062'}
;; markup.underline {'fontStyle': 'underline', 'foreground': '#E18964'}
;; markup.quote {'background': '#FEE09C12', 'fontStyle': 'italic', 'foreground': '#E1D4B9'}
;; markup.heading, markup.heading entity {'background': '#632D04', 'fontStyle': '', 'foreground': '#FEED1E'}
;; markup.list {'foreground': '#E1D4B9'}
;; markup.raw {'background': '#B1B3BA08', 'fontStyle': '', 'foreground': '#578BB3'}
;; markup comment {'fontStyle': 'italic', 'foreground': '#F67B37'}
;; meta.separator {'background': '#242424', 'foreground': '#60A633'}
;; meta.line.entry.logfile, meta.line.exit.logfile {'background': '#EEEEEE29'}
;; meta.line.error.logfile {'background': '#751012'}
;; entity.name.function.definition.erlang {'foreground': '#E3FF73'}
(deftheme smyck
  "Smyck emacs theme autoconverted from the textMate theme by Stanley Rost")
(custom-theme-set-faces
  'smyck
    '(trailing-whitespace ((t (:background "#454b51"))))
    '(whitespace-trailing ((t (:background "#454b51"))))

    '(border ((t (:background "#323232" ))))
    '(css-property ((t (:foreground "#c5af75" ))))
    '(css-proprietary-property ((t (:foreground "#c5af75" ))))
    '(css-selector ((t (:foreground "#8b98ab" ))))
    '(cursor ((t (:background "#fffa04" ))))
    '(default ((t (:background "#1b1b1b" :foreground "#f8f8f8" ))))
    '(diff-added ((t (:background "#253b22" :foreground "#f8f8f8" ))))
    '(diff-changed ((t (:background "#4a410d" :foreground "#f8f8f8" ))))
    '(diff-header ((t (:background "#0e2231" :italic t :foreground "#f8f8f8" ))))
    '(diff-hunk-header ((t (:background "#0e2231" :italic t :foreground "#f8f8f8" ))))
    '(diff-removed ((t (:background "#420e09" :foreground "#f8f8f8" ))))
    '(font-lock-builtin-face ((t (:foreground "#f4f4f4" ))))
    '(font-lock-comment-delimiter-face ((t (:italic t :foreground "#939393" ))))
    '(font-lock-comment-face ((t (:italic t :foreground "#939393" ))))
    '(font-lock-constant-face ((t (:foreground "#faa49c" ))))
    '(font-lock-doc-face ((t (:foreground "#ddf2a4" ))))
    '(font-lock-function-name-face ((t (:foreground "#f4f4f4" ))))
    '(font-lock-keyword-face ((t (:foreground "#d1fa72" ))))
    '(font-lock-negation-char-face ((t (:foreground "#ebebeb" ))))
    '(font-lock-preprocessor-face ((t (:foreground "#8996a8" ))))
    '(font-lock-regexp-grouping-backslash ((t (:foreground "#cf7d34" ))))
    '(font-lock-regexp-grouping-construct ((t (:foreground "#cf7d34" ))))
    '(font-lock-string-face ((t (:foreground "#ddf2a4" ))))
    '(font-lock-type-face ((t (:foreground "#afee7f" ))))
    '(font-lock-variable-name-face ((t (:foreground "#9ee4fe" ))))
    '(font-lock-warning-face ((t (:background "#472847" :foreground "#fd5ff1" ))))
    '(fringe ((t (:background "#323232" ))))
    '(highlight ((t (:background "#1d5766" ))))
    '(hl-line ((t (:background "#323232" ))))
    '(isearch ((t (:background "#4a410d" :foreground "#f8f8f8" ))))
    '(isearch-fail ((t (:background "#420e09" :foreground "#f8f8f8" ))))
    '(iswitchb-current-match ((t (:background "#253b22" :foreground "#f8f8f8" ))))
    '(iswitchb-invalid-regexp ((t (:background "#420e09" :foreground "#f8f8f8" ))))
    '(iswitchb-single-match ((t (:background "#253b22" :foreground "#f8f8f8" ))))
    '(iswitchb-virtual-matches ((t (:background "#4a410d" :foreground "#f8f8f8" ))))
    '(linum ((t (:background "#323232" ))))
    '(match ((t (:background "#253b22" :foreground "#f8f8f8" ))))
    '(menu ((t (:background "#1b1b1b" :foreground "#f8f8f8" ))))
    '(minibuffer-prompt ((t (:foreground "#89bdff" ))))
    '(mode-line ((t (:background "#242424" :foreground "#60a633" ))))
    '(mode-line-buffer-id ((t (:foreground "#a5eafc" ))))
    '(mode-line-emphasis ((t (:bold t :foreground "#e9c062" ))))
    '(mode-line-highlight ((t (:italic t :foreground "#e9c062" ))))
    '(mode-line-inactive ((t (:background "#1d5766" ))))
    '(org-agenda-dimmed-todo-face ((t (:italic t :foreground "#939393" ))))
    '(org-column ((t (:background "#323232" ))))
    '(org-column-title ((t (:background "#323232" ))))
    '(org-done ((t (:background "#253b22" :foreground "#f8f8f8" ))))
    '(org-hide ((t (:foreground "#1b1b1b" ))))
    '(org-todo ((t (:background "#472847" :foreground "#fd5ff1" ))))
    '(org-upcoming-deadline ((t (:italic t :underline t :foreground "#fd5ff1" ))))
    '(org-warning ((t (:background "#472847" :foreground "#fd5ff1" ))))
    '(region ((t (:background "#1d5766" ))))
    '(secondary-selection ((t (:background "#1d5766" ))))
    '(show-paren-match-face ((t (:background "#1d5766" ))))
    '(show-paren-mismatch-face ((t (:background "#472847" :foreground "#fd5ff1" ))))
)

(provide-theme 'smyck)
