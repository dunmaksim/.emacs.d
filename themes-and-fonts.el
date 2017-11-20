;;; Package --- Summary
;;; Commentary:
;;; Themes and fonts

;;; Code:

(set-face-attribute 'default nil :height 110)
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

(use-package monokai-theme
  :ensure t)

(use-package powerline)

(use-package airline-themes
  :config
  (load-theme 'airline-molokai))

;;; themes-and-fonts.el ends here
