;;; python.el --- Summary
;;; Commentary:
;;; Settings for Python Language Client

;; (use-package lsp-mode)

;; (use-package company-lsp
;;   :config
;;   (push 'company-lsp company-backends))

;; (use-package lsp-python
;;   :mode
;;   "\\.py\\'"
;;   :config
;;   (add-hook 'python-mode-hook #'lsp-python-enable))

(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq python-indent-offset 4)
  (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

(use-package anaconda-mode)

(use-package company-anaconda
  :init
  (eval-after-load "company" '(add-to-list 'company-backends 'company-anaconda))
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package py-autopep8
  :init
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(use-package elpy
  :pin elpy
  :init
  (elpy-enable))

(use-package py-isort
  :init
  (add-hook 'before-save-hook 'py-isort-before-save)
  :config
  (setq py-isort-options '("-sl")))

(use-package virtualenvwrapper)

(defalias 'workon 'pyvenv-workon)

;;; python.el ends here
