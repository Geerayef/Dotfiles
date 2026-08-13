;;; git.el --- Git utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun gracs/git-branch ()
  (magit-git-string "branch" "--show-current"))

(defun gracs/git-diff-stats ()
  (let* ((stats (magit-git-string "diff" "--numstat" "HEAD"))
         (lines (split-string stats "\n" t))
         (added 0) (deleted 0) (modified 0))
    (dolist (line lines)
      (when (string-match "\\([0-9]+\\)\t\\([0-9]+\\)" line)
        (setq added (+ added (string-to-number (match-string 1 line)))
              deleted (+ deleted (string-to-number (match-string 2 line)))
              modified (1+ modified))))
    (format "+%d ~%d -%d" added modified deleted)))

(provide 'util/git)
