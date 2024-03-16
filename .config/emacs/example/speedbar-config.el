;;;; speedbar-config.el --- Speedbar configuration -*- mode: emacs-lisp; mode: outline-minor; lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'speedbar)

;; Auto-update when the attached frame changes directory
(customize-set-variable 'speedbar-update-flag t)
;; Disable icon images, instead use text
(customize-set-variable 'speedbar-use-images nil)
;; Customize Speedbar Frame
(customize-set-variable 'speedbar-frame-parameters
                        '((name . "speedbar")
                          (title . "speedbar")
                          (minibuffer . nil)
                          (border-width . 2)
                          (menu-bar-lines . 0)
                          (tool-bar-lines . 0)
                          (unsplittable . t)
                          (left-fringe . 10)))

;;; Keybindings
(defun quick-buffers ()
  "Temporarily switch to quick-buffers expansion list.
Useful for quickly switching to an open buffer."
  (interactive)
  (speedbar-change-initial-expansion-list "quick buffers"))

;; map switch-to-quick-buffers in speedbar-mode
(keymap-set speedbar-mode-map "b" #'quick-buffers)

;;; File Extensions
(speedbar-add-supported-extension
 (list
;;;; General Lisp Languages
  ".cl"
  ".li?sp"
;;;; Lua/Fennel (Lisp that transpiles to lua)
  ".lua"
  ".fnl"
  ".fennel"
;;;; JVM languages (Java, Kotlin, Clojure)
  ".kt"
  ".mvn"
  ".gradle"
  ".properties"
  ".cljs?"
;;;; shellscript
  ".sh"
  ".bash"
  ".fish"
  ".zsh"
;;;; Web Languages and Markup/Styling
  ".php"
  ".ts"
  ".js"
  ".html?"
  ".css"
  ".less"
  ".scss"
  ".sass"
;;;; Makefile
  "makefile"
  "MAKEFILE"
  "Makefile"
;;;; Data formats
  ".json"
  ".yml"
  ".yaml"
  ".toml"
;;;; Notes and Markup
  ".md"
  ".markdown"
  ".org"
  ".txt"
  "README"))

(provide 'speedbar-config)
;;; speedbar-config.el ends here
