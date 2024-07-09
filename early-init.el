;;; early-init.el --- Файл ранних настроек
;;; Commentary:
;;; При использовании `straight.el' пакет `package.el' не нужен.

;;; Code:

(require 'custom)

(customize-set-variable 'package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
