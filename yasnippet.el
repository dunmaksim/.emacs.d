;;; yasnippet.el --- Summary
;;; Commentary:
;;; Settings for aysnippet

;;; Code:

(use-package yasnippet
  :after prog-mode
  :defer 10
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-load-directory (emacs-path "snippets"))
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

;;; yasnippet.el ends here
