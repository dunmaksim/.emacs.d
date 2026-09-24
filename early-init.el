;;; early-init.el --- Basic settings for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This file is not a part of GNU Emacs.

;;; Code:


;; Кастомные настройки загрузим сразу же, чтобы потом переопределить
(require 'custom)
(setopt custom-file
  (expand-file-name
    (convert-standard-filename "custom.el")
    user-emacs-directory)) ;; Файл для сохранения пользовательских настроек, сделанных в customize.
(when (file-exists-p custom-file)
  (load custom-file))

;; Меню не нужно
(when (fboundp 'menu-bar-mode) (setopt menu-bar-mode nil))
;; Полосы прокрутки не нужны
(when (fboundp 'scroll-bar-mode) (setopt scroll-bar-mode nil))
;; Панель инструментов не нужна
(when (fboundp 'tool-bar-mode) (setopt tool-bar-mode nil))

;; Настроим архивы:
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(setopt package-archive-priorities ;; Приоритеты архивов: чем выше, тем лучше.
  '(
     ("gnu" . 2)
     ("nongnu" . 1))
  package-native-compile t ;; Компиляция пакетов в нативный код при установке
  package-vc-register-as-project nil)


;; Всякое разное
(setopt
  gc-cons-threshold (* 800000 2) ;; Удвоим размер памяти для сборщика мусора)
  inhibit-splash-screen t        ;; Не показывать заставку
  inhibit-startup-buffer-menu t  ;; Выключить меню буферов при запуске
  inhibit-startup-screen t       ;; Не показывать приветственный экран
  load-prefer-newer t)           ;; Если есть файл elc, но el новее, загрузить el-файл.

(provide 'early-init)
;;; early-init.el ends here
