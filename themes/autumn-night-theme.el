;;; autum-theme.el --- Autumn theme ported from helix  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Larry Daffner

;; Author: Larry Daffner <vizzie@flamingpackets.net>
;; Created: 2025-12-07
;;
;; Commentary:
;; 
;; Code:

(deftheme autumn-night
  "Autumn Night theme ported from helix"
  :background-mode 'dark
  :kind 'color-scheme)

(defconst autumn-theme-palette
  '((my_black . "#111111")
    (my_brown . "#cfba8b")
    (my_gray0 . "#090909")
    (my_gray1 . "#0e0e0e")
    (my_gray2 . "#1a1a1a")
    (my_gray3 . "#404040")
    (my_gray4 . "#626c66")
    (my_gray5 . "#626c66")
    (my_gray6 . "#aaaaaa")
    (my_gray7 . "#c4c4c4")
    (my_gray8 . "#e8e8e8")
    (my_green . "#99be70")
    (my_red . "#F05E48")
    (my_turquoise1 . "#86c1b9")
    (my_turquoise2 . "#72a59e")
    (my_white1 . "#F3F2CC")
    (my_white2 . "#F3F2CC")
    (my_white3 . "#F3F2CC")
    (my_white4 . "#7e7d6a")
    (my_yellow1 . "#FAD566")
    (my_yellow2 . "#ffff9f")))

(defmacro autumn-get-color (color)
  (cdr (assoc color autumn-theme-palette)))

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'autumn-night
   `(cursor ((,class (:background ,(autumn-get-color my_white4)))))
   `(default ((,class (:background ,(autumn-get-color my_gray0) :foreground ,(autumn-get-color my_white1)))))
   `(fringe ((,class (:background ,(autumn-get-color my_gray0)))))
   `(menu ((,class (:foreground ,(autumn-get-color my_white1) :background ,(autumn-get-color my_gray2)))))
   `(region ((,class (:background ,(autumn-get-color my_gray3)))))

   `(mode-line-active ((,class (:background ,(autumn-get-color my_gray7) :background ,(autumn-get-color my_gray2)))))
   `(mode-line-inactive ((,class (:foreground ,(autumn-get-color my_gray5) :background ,(autumn-get-color my_gray2)))))
   
   `(hl-line ((,class (:background ,(autumn-get-color my_gray3)))))
   `(show-paren-match ((,class (:foreground ,(autumn-get-color my_white1) :background ,(autumn-get-color my_black) :bold t :underline t))))
   `(line-number ((,class (:foreground ,(autumn-get-color my_gray3) :background ,(autumn-get-color my_gray0)))))
   `(line-number-current-line ((,class (:foreground ,(autumn-get-color my_gray7) :background ,(autumn-get-color my_gray0)))))
   
   `(error ((,class (:foreground ,(autumn-get-color my_red)))))
   `(info ((,class (:foreground ,(autumn-get-color my_yellow2)))))
   `(warning ((,class (:foreground ,(autumn-get-color my_yellow2)))))
   
   `(eglot-inlay-hint-face ((,class (:foreground ,(autumn-get-color my_gray4) :background ,(autumn-get-color my_black)))))
   `(eglot-type-hint-face ((,class (:foreground ,(autumn-get-color my_gray4) :italic t))))
   `(eglot-parameter-hint-face ((,class (:foreground ,(autumn-get-color my_gray4)))))

   `(markdown-header-face ((,class (:foreground ,(autumn-get-color my_yellow1)))))
   `(markdown-link-face ((,class (:foreground ,(autumn-get-color my_turquoise2)))))
   `(markdown-list-face ((,class (:foreground ,(autumn-get-color my_white2)))))
   `(markdown-blockquote-face ((,class (:foreground ,(autumn-get-color my_brown)))))
   `(markdown-code-face ((,class (:foreground ,(autumn-get-color my_green)))))
   
   `(font-lock-builtin-face ((,class (:foreground ,(autumn-get-color my_white3)))))
   `(font-lock-comment-face ((,class (:foreground ,(autumn-get-color my_gray5)))))
   `(font-lock-property-use-face ((,class (:foreground ,(autumn-get-color my_turquoise1)))))
   `(font-lock-comment-face ((,class (:foreground ,(autumn-get-color my_gray5)))))
   `(font-lock-constant-face ((,class (:foreground ,(autumn-get-color my_white3)))))
   `(font-lock-escape-face ((,class (:foreground ,(autumn-get-color my_turquoise1)))))
   `(font-lock-function-name-face ((,class (:foreground ,(autumn-get-color my_yellow1)))))
   `(font-lock-keyword-face ((,class (:foreground ,(autumn-get-color my_red)))))
   `(font-lock-operator-face ((,class (:foreground ,(autumn-get-color my_white1)))))
   `(font-lock-string-face ((,class (:foreground ,(autumn-get-color my_green)))))
   `(font-lock-type-face ((,class (:foreground ,(autumn-get-color my_white3) :italic t))))
   `(font-lock-variable-name-face ((,class (:foreground ,(autumn-get-color my_white3)))))

   `(diff-added ((,class (:foreground ,(autumn-get-color my_green)))))
   `(diff-removed ((,class (:foreground ,(autumn-get-color my_red)))))
   `(diff-changed ((,class (:foreground ,(autumn-get-color my_gray5)))))
   ))

