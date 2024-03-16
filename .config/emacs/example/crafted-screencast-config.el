;;;; crafted-screencast.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'package-selected-packages 'keycast)

(when (locate-library "keycast")
  (customize-set-variable 'keycast-mode-line-remove-tail-elements nil)
  (customize-set-variable 'keycast-mode-line-insert-after 'mode-line-misc-info)
  (keycast-mode-line-mode))

(provide 'crafted-screencast-config)
;;; crafted-screencast.el ends here
