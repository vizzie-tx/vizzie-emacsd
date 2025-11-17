(deftheme lrd-base
  "Our basic customized theme")

(let ((class '((class color) (min-colors 88)))
      (main-fg "#f4ebd9")
      (main-bg "#000000")
      (dim-bg  "#3a3a3a")
      (green-fg "#17b890")
      (brown-fg "#c2847a")
      (blue-fg "#058ed9")
      (purple-fg "#a366e1")
      (brick-fg "#f21b3f")
      (steelblue-fg "#41739f")
      (teal-fg "#46acc2")
      (redviolet-fg "#e55dcb"))
  (custom-theme-set-faces
   'lrd-base
   `(default ((,class :background ,main-bg :foreground ,main-fg)))
   `(font-lock-builtin-face ((,class :foreground ,steelblue-fg)))
   `(font-lock-comment-face ((,class :foreground ,brown-fg)))
   `(font-lock-constant-face ((,class :foreground ,purple-fg)))
   `(font-lock-doc-face ((,class :inherit font-lock-string-face :italic t)))
   `(font-lock-function-name-face ((,class :foreground ,blue-fg)))
   `(font-lock-keyword-face ((,class :foreground ,teal-fg)))
   `(font-lock-preprocessor-face ((,class :inherit font-lock-builtin-face)))
   `(font-lock-string-face ((,class :foreground ,redviolet-fg)))
   `(font-lock-type-face ((,class :foreground ,green-fg)))
   `(font-lock-variable-name-face ((,class :foreground ,brick-fg)))
   `(font-lock-warning-face ((,class :foreground "orangered")))
   ))

(provide-theme 'lrd-base)

(provide 'lrd-base-theme)
