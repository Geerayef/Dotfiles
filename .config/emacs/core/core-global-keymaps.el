;;;; core-global-keymaps.el --- Keymaps -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(global-set-key (kbd "C-v")
                (lambda ()
                  (interactive
                    (next-line (/ (window-height (selected-window)) 3)))))

(global-set-key (kbd "M-v")
                (lambda ()
                  (interactive
                    (previous-line (/ (window-height (selected-window)) 3)))))

(provide 'core-global-keymaps)
;;; core-global-keymaps.el ends here
