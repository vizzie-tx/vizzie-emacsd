;; functions to guess package name based on path
;; assumes that the treeis rooted at src or java
;; Used by YASnippet snippets to automatically guess package name
(defun lrd/pkg-from-path (dir)
  "Return a java package name from the source file path, or nil if none found"
  (let* ((projroot (and (project-current)
                        (project-root (project-current))))
         (srcdir (locate-dominating-file default-directory "src"))
         (toplevel (or projroot (and srcdir (expand-file-name srcdir) default-directory)))
         (relative-path (file-relative-name default-directory toplevel)))
    (dolist (prefix '("/" "src/" "main/" "java/")) (setq relative-path (string-remove-prefix prefix relative-path)))
    (string-replace "/" "." (directory-file-name relative-path))))
