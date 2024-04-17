;;;; fish.el --- Fish support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package fish-mode
  :ensure t
  :defer t
  :hook
  (fish-mode . (lambda () (add-hook 'before-save-hook . 'fish_indent-before-save))))

(provide 'fish)
;;; fish.el ends here
