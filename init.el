;;; init.el --- Dunaevsky Maxim GNU EMACS config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Настройки EMACS для работы техническим писателем.

;;; Code:


(defun emacs-version-not-less-than (major minor)
  "True when Emacs version is not less than MAJOR MINOR version."
  (or
   (> emacs-major-version major)
   (and (= emacs-major-version major)
        (>= emacs-minor-version minor))))

(defconst init-el-font-height 16 "Размер шрифта по умолчанию.")
(defvar init-el-theme 'misterioso "Тема по умолчанию")

(require 'custom)
(customize-set-variable
 'custom-file
 (expand-file-name
  (convert-standard-filename "custom.el")
  user-emacs-directory)
 "Файл для сохранения пользовательских настроек, сделанных в customize.")


(load "~/.emacs.d/base.el")
(load "~/.emacs.d/site.el")


;; Настройки, специфичные для графического режима
(defun setup-gui-settings (frame-name)
  "Настройки, необходимые при запуске EMACS в графической среде.

  FRAME-NAME — имя фрейма, который настраивается."
  (when (display-graphic-p frame-name)
    (global-font-lock-mode t)  ;; Отображать шрифты красиво, используя Font Face's

    (defvar availiable-fonts (font-family-list)) ;; Какие есть семейства шрифтов?
    (defvar default-font-family nil "Шрифт по умолчанию.")

    ;; Перебор шрифтов11
    (cond
     ((member "Fire Code Nerd" availiable-fonts)
      (setq default-font-family "Fira Code Nerd"))
     ((member "Fira Code" availiable-fonts)
      (setq default-font-family "Fira Code"))
     ((member "DejaVu Sans Mono Nerd" availiable-fonts)
      (setq default-font-family "DejaVu Sans Mono Nerd"))
     ((member "DejaVu Sans Mono" availiable-fonts)
      (setq default-font-family "DejaVu Sans Mono"))
     ((member "Source Code Pro" availiable-fonts)
      (setq default-font-family "Source Code Pro"))
     ((member "Consolas" availiable-fonts)
      (setq default-font-family "Consolas")))

    (when default-font-family
      ;; Это формат X Logical Font Description Conventions, XLFD
      ;; https://www.x.org/releases/X11R7.7/doc/xorg-docs/xlfd/xlfd.html
      (set-frame-font
       (format "-*-%s-normal-normal-normal-*-%d-*-*-*-m-0-iso10646-1"
               default-font-family
               init-el-font-height) nil t)
      (set-face-attribute 'default nil :family default-font-family))

    (set-face-attribute 'default nil :height (* init-el-font-height 10))))

;; Правильный способ определить, что EMACS запущен в графическом режиме. Подробнее здесь:
;; https://emacsredux.com/blog/2022/06/03/detecting-whether-emacs-is-running-in-terminal-or-gui-mode/
(add-to-list 'after-make-frame-functions #'setup-gui-settings)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setup-gui-settings (selected-frame))

(load-theme init-el-theme t)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init.el)
;;; init.el ends here
