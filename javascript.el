;;; Package -- Summary
;;; Commentary:
;;; Settings for JavaScript

;; (use-package lsp-javascript-typescript
;;   :config
;;   (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
;;   (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable) ;; for typescript support
;;   (add-hook 'js3-mode-hook #'lsp-javascript-typescript-enable) ;; for js3-mode support
;;   (add-hook 'rjsx-mode #'lsp-javascript-typescript-enable) ;; for rjsx-mode support
;;   )

(use-package js2-mode
  :mode "\\.js\\'")

(use-package tide
  :init (tide-hl-identifier-mode +1)
  :hook ((before-save-hook . tide-format-before-save)
	 (typescript-mode-hook . setup-tide-mode)))

(use-package company)
(use-package company-tern
  :init  (add-to-list 'company-backends 'company-tern)
  :hook  (js2-mode-hook . (lambda () (tern-mode)(company-mode)))
  :bind
  ("M-." . nil)
  ("M-m" . nil))

;;; javascript.el ends here
