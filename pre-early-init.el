;;; pre-early-init.el - First things forst -*- no-byte-compile:t; lexical-binding:t -*-

(defvar lrd/configs-directory
  (expand-file-name "config/" user-emacs-directory)
  "Directory for modular configuration files")

(add-to-list 'load-path lrd/configs-directory)

(provide 'pre-early-init)
;; pre-early-init.el ends here
