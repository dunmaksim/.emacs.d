;;; themes-and-fonts.el --- Summary
;;; Commentary:
;;; Themes and fonts

;;; Code:

(set-face-attribute 'default nil :height 110)
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

(use-package monokai-theme
  :init (load-theme 'monokai))

(use-package powerline)

(use-package airline-themes
  :init (load-theme 'airline-molokai))

;;; themes-and-fonts.el ends here
