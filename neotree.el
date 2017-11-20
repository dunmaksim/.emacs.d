;;; Neotree --- Summary
;;; Commentary:
;;; Настройки для NeoTree

;;; Code:
(use-package neotree
  :ensure t
  :pin melpa-stable
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 40)
  :config
  (neotree-dir "~/repo")
  :bind
  ("<f8>" . neotree-toggle)
  )
