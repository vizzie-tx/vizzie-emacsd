;; functions to guess package name based on path
;; assumes that the treeis rooted at src or java
;; Used by YASnippet snippets to automatically guess package name
(defun lrd/pkg-src-path (dir &optional desc)
  "Return components of directory path up to src or java, or nil if not found"
  (let ((parent (file-name-directory (directory-file-name dir)))
        (child (file-name-nondirectory (directory-file-name dir))))
    (cond ((equal child "") nil)
           ((or (equal child "src") (equal child "java")) desc)
           (t (lrd/pkg-src-path parent (cons child desc))))))

(defun lrd/pkg-from-path (dir)
  "Return a java package name from the source file path, or nil if none found"
  (message dir)
  (let ((ret (lrd/pkg-src-path dir)))
    (cond (ret (mapconcat 'identity ret "."))
          (t ""))))
