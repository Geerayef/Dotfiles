;;;; hydra.el --- Hydra -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (and (require 'hydra nil :noerror)
           (require 'dumb-jump nil :noerror))
  (defhydra dumb-jump-hydra (:color blue :columns 3)
            "Dumb Jump"
            ("j" dumb-jump-go "Go")
            ("o" dumb-jump-go-other-window "Other window")
            ("e" dumb-jump-go-prefer-external "Go external")
            ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
            ("i" dumb-jump-go-prompt "Prompt")
            ("l" dumb-jump-quick-look "Quick look")
            ("b" dumb-jump-back "Back"))
  (keymap-set dumb-jump-mode-map "C-M-y" #'dumb-jump-hydra/body))
;; use xref
(with-eval-after-load 'dumb-jump
                      (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'hydra)
;;; hydra.el ends here
