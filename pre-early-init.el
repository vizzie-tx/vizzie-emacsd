;;; pre-early-init.el - First things forst -*- no-byte-compile:t; lexical-binding:t -*-

(defvar lrd/configs-directory
  (expand-file-name "config/" user-emacs-directory)
  "Directory for modular configuration files")

(defvar lrd/lisp-directory
  (expand-file-name "lisp/" user-emacs-directory))

(add-to-list 'load-path lrd/configs-directory)
(and (file-exists-p lrd/lisp-directory)
     (add-to-list 'load-path lrd/lisp-directory))

;; WHY? gtk-emacs decided that 10x20 is a good default size
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist '(height . 34))

(provide 'pre-early-init)
;; pre-early-init.el ends here
