;;; flycheck.el --- Summary:
;;; Commentary:
;;; Settings for Flycheck -- better syntax checker in the world

;;; Code:

(use-package flycheck
  :commands flycheck-mode
  :config
  (defalias 'show-error-at-point-soon
    'flycheck-show-error-at-point))


;;; flycheck.el ends here
