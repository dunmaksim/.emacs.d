;;; Neotree --- Summary
;;; Commentary:
;;; Neotree settings

;;; Code:
(use-package neotree
  :ensure t
  :init (setq neo-autorefresh nil) ;; Don't change working dir for neotree
  :config (neotree-dir "~/repo")
  :bind ("<f8>" . neotree-toggle))

;;; neotree.el ends here
