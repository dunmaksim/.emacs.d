;;; package --- Summary
;;; Commentary:
;;; Settings for Python Language Client

(use-package lsp-mode)

(use-package company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package lsp-python
  :mode
  "\\.py\\'"
  :config
  (add-hook 'python-mode-hook #'lsp-python-enable))

(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq python-indent-offset 4)
  (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

(use-package py-autopep8)

(use-package py-isort
  :config
  (setq py-isort-options '("-s1")))

;;; python.el ends here
