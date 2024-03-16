;;;; core-util.el --- Utility functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun util/recursive-add-to-load-path (dir)
  "Add DIR and all its sub-directories to `load-path'."
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir))
    (let ((name (expand-file-name f dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (util/recursive-add-to-load-path name)))))

(defun util/load-theme (theme)
  "Load the THEME."
  (load-theme theme))

(provide 'core-util)
;;; core-util.el ends here

