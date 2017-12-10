;;; neotree.el --- Summary
;;; Commentary:
;;; Neotree settings

;;; Code:
(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open 0)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :init
  (global-set-key [f8] 'neotree-toggle)
  (neotree-dir "~/repo"))

;;; neotree.el ends here
