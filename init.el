;;; init.el --- Summary
;;; Commentary:
;;; Main EMACS settings file, load settings from parts.

;;; Code:

(require 'package)

;;; Sources for package installing. Stable is very stable, but not
;;; have somebody packages.

(setq package-archives nil)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize nil)

;;; Если пакет use-package не установлен, его нужно скачать и установить
(unless (package-installed-p 'use-package)
  (message "EMACS install use-package.el")
  (package-refresh-contents)
  (package-install 'use-package))

;;; Установили, загрузили, указали, что недостающие пакеты нужно
;;; автоматически загружать и устанавливать.
(require 'use-package)
(setq use-package-always-ensure t)

;;; Указываем откуда брать части настроек.
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory) user-emacs-directory)
        ((boundp 'user-init-directory) user-init-directory)
        (t "~/.emacs.d/")))

;;; Функция для загрузки настроек из указанного файла.
(defun load-user-file (file)
  "Check FILE existing and then load part of Emacs config.
Extension el is added automatically."
  (interactive "f")
  "Load a file in current user's configuration directory"
  (setq full-user-file-path (expand-file-name (format "%s.el" file) user-init-dir))
  (if (file-readable-p full-user-file-path)
      (load-file full-user-file-path)
    (message "File %s does not exists" full-user-file-path)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; ЗАГРУЗКА ОСНОВНЫХ МОДУЛЕЙ ПРОИСХОДИТ ЗДЕСЬ ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar user-files-list '(
			  neotree
			  keyboard
			  ibuffer
			  themes-and-fonts
			  company
			  python
			  javascript
			  json
			  markdown
			  orgmode
			  yasnippet
			  web
			  magit
			  ))

(dolist (user-file user-files-list)
  (load-user-file user-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; GLOBAL SETTINGS WITHOUT CATEGORY ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill *scratch* with fire!!!
(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))

;;; Scrolling
(setq scroll-step               1) ;; one line
(setq scroll-margin            10) ;; scroll buffer to 10 lines at going to last line
(setq scroll-conservatively 10000)
(setq-default save-place t)        ;; Save cursor position between sessions

;;; Disable overwrite mode-line
(define-key global-map [(insert)] nil)

;;; Format file before save
(defun format-current-buffer()
  (indent-region (point-min) (point-max)))
(defun untabify-current-buffer()
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max)))
  nil)
(add-to-list 'write-file-functions 'untabify-current-buffer)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(cua-mode 1)                   ;;; Windows-like Ctrl-C, Ctrl-V
(fset 'yes-or-no-p 'y-or-n-p)  ;;; Shortcuts for yea and no
(global-hl-line-mode 1)        ;;; Highlight current line always
(global-linum-mode 1)          ;;; Show line numbers globally
(menu-bar-mode -1)             ;;; Disable menu
(scroll-bar-mode -1)           ;;; Disable scrollbar
(tool-bar-mode -1)             ;;; Disable toolbar
(desktop-save-mode 1)          ;;; Auto save desktop
(linum-mode t)                 ;;; Always show line numbers
(electric-pair-mode t)         ;;; Auto close brackets
(setq ring-bell-function 'ignore) ;;; Turn off bell

;;; Turn off backups creation
(setq make-backup-files nil)
(setq auto-save-default nil)

;;; Rainbow delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package all-the-icons) ;;; Show icons for files in neotree, ibuffer and more

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOADING USER SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/customize.el")
(load-file "~/.emacs.d/customize.el")

;;; init.el ends here
