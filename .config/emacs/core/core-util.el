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

(defun util/load-custom-themes ()
  "Load themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(provide 'core-util)
;;; core-util.el ends here

