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
  :interpreter ("python" . python-mode)
  :config
  (setq python-indent-offset 4)

  )

(use-package anaconda-mode)

(use-package company-anaconda
  :init
  (eval-after-load "company" '(add-to-list 'company-backends 'company-anaconda))
  :hook
  (python-mode-hook . anaconda-mode))

(use-package py-autopep8
  :hook
  (python-mode-hook . py-autopep8-enable-on-save))

(use-package elpy
  :init
  (elpy-enable))

(use-package py-isort
  :hook
  (add-hook 'before-save-hook 'py-isort-before-save)
  :config
  (setq py-isort-options '("-sl")))

(use-package virtualenvwrapper)

(defalias 'workon 'pyvenv-workon)

;;; python.el ends here
