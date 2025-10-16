;;; init.el --- Dunaevsky Maxim GNU EMACS config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Настройки EMACS для работы техническим писателем.

;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p) ;; Использовать y и n вместо yes и no (сокращает объём вводимого текста для подтверждения команд)

(defconst init-el-font-height 15 "Размер шрифта по умолчанию.")

(defun init-el-set-font-height ()
  "Устанавливает указанный размер шрифта во фрейме."
  (set-face-attribute 'default nil :height (* init-el-font-height 10)))

(require 'custom)
(setopt custom-file
        (expand-file-name
         (convert-standard-filename "custom.el")
         user-emacs-directory)) ;; Файл для сохранения пользовательских настроек, сделанных в customize.

;; Загрузим настройки сразу, чтобы они не переопределяли параметры из `init.el'.
(when (file-exists-p custom-file)
  (load custom-file))

;;; Здесь находятся настройки базовой функциональности Emacs.
;;; Даже если будут какие-то проблемы со сторонними пакетами, этот код всё
;;; равно будет выполнен.
;;; По этой же причине здесь нет ничего, что могло бы сломаться.

;; Настройки, специфичные для графического режима
(defun setup-gui-settings (frame-name)
  "Настройки, необходимые при запуске EMACS в графической среде.

  FRAME-NAME — имя фрейма, который настраивается."
  (when (display-graphic-p frame-name)
    (defvar availiable-fonts (font-family-list)) ;; Какие есть семейства шрифтов?
    (defvar default-font-family nil "Шрифт по умолчанию.")

    ;; Перебор шрифтов
    (cond
     ((member "Fira Code Nerd" availiable-fonts)
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
               init-el-font-height)
       nil
       t)
      (set-face-attribute 'default nil :family default-font-family))
    (init-el-set-font-height)))

(global-font-lock-mode t)  ;; Отображать шрифты красиво, используя Font Face's

;; Правильный способ определить, что EMACS запущен в графическом режиме. Подробнее здесь:
;; https://emacsredux.com/blog/2022/06/03/detecting-whether-emacs-is-running-in-terminal-or-gui-mode/
(add-to-list 'after-make-frame-functions #'setup-gui-settings)
(add-hook 'after-init-hook (lambda ()(setup-gui-settings (selected-frame))))

(defconst init-el-autosave-dir
  (expand-file-name "saves" user-emacs-directory)
  "Каталог для файлов автосохранения.")
(unless (file-directory-p init-el-autosave-dir)
  (make-directory init-el-autosave-dir))

(defconst init-el-package-user-dir
  (expand-file-name "elpa" user-emacs-directory)
  "Пользовательский каталог с пакетами.")
(unless (file-directory-p init-el-package-user-dir)
  (make-directory init-el-package-user-dir))


;; Определение пути к каталогу с исходным кодом
(when (string-equal system-type "gnu/linux")
  (message "Используется ОС на базе GNU/Linux")
  (defvar init-el-emacs-source-path "Путь к каталогу с исходным кодом Emacs")
  (setq init-el-emacs-source-path
        (format "/usr/share/emacs/%d.%d/src/"
                emacs-major-version
                emacs-minor-version))
  (if (file-exists-p init-el-emacs-source-path)
      ;; Каталог существует
      (if (directory-empty-p init-el-emacs-source-path)
          ;; Каталог пуст
          (message (format "Каталог %s пуст." init-el-emacs-source-path))
        ;; Каталог не пуст
        (progn
          (setopt source-directory init-el-emacs-source-path)
          (message (format "Исходный код обнаружен в каталоге %s" init-el-emacs-source-path))))
    ;; Каталог не существует
    (message (format "Каталог %s не существует." init-el-emacs-source-path))))

(setopt completion-ignore-case t ;; Игнорировать регистр при автодополнении
        create-lockfiles nil ;; Не создавать lock-файлы
        cursor-type 'bar ;; Курсор в виде вертикальной черты
        default-input-method "russian-computer" ;; Метод ввода по умолчанию
        default-transient-input-method "russian-computer" ;; Временный метод ввода
        delete-by-moving-to-trash t ;; Удалять файлы в Корзину
        gc-cons-threshold (* 2 gc-cons-threshold) ;; Увеличить размер памяти для сборщика мусора
        inhibit-startup-screen t ;; Не показывать приветственный экран
        initial-scratch-message nil ;; Пустой буфер *scratch*
        load-prefer-newer t ;; Если есть файл elc, но el новее, загрузить el-файл.
        major-mode 'text-mode ;; Текстовый режим для новых буферов по умолчанию.
        read-answer-short t ;; Быстрый ввод ответов на вопросы (не аналог yes-or-no-p
        read-file-name-completion-ignore-case t ;; Игнорировать регистр при вводе имён файлов
        read-process-output-max (* 1024 1024) ;; Увеличим чанк чтения для LSP: по умолчанию 65535
        ring-bell-function 'ignore ;; Отключить звуковое сопровождение событий
        scroll-margin 4 ;; Отступ от верхней и нижней границ буфера
        show-trailing-whitespace t ;; Подсветка висячих пробелов
        standard-indent 4 ;; Отступ по умолчанию
        tab-always-indent 'complete ;; Если можно — выровнять текст, иначе — автодополнение.
        use-dialog-box nil ;; Диалоговые окна ОС не нужны
        use-short-answers t ;; Краткие ответы вместо длинных
        user-full-name "Dunaevsky Maxim" ;; Имя пользователя
        user-mail-address "dunmaksim@yandex.ru" ;; Адрес электронной почты
        vc-follow-symlinks t ;; Переходить по ссылкам без лишних вопросов
        visible-bell t) ;; Мигать буфером при переходе в него


(defun init-kill-scratch ()
  "Закрыть буфер *scratch* при запуске редактора или подключении клиента."
  (when (get-buffer "*scratch*")
    (kill-buffer "*scratch*")))

(add-hook 'after-init-hook 'init-kill-scratch)
(add-hook 'server-after-make-frame-hook 'init-kill-scratch)

(when (fboundp 'menu-bar-mode)
  (setopt menu-bar-mode nil)) ;; Выключить отображение меню

(when (fboundp 'scroll-bar-mode)
  (setopt scroll-bar-mode nil)) ;; Отключить полосы прокрутки

(when (fboundp 'tool-bar-mode)
  (setopt tool-bar-mode nil)) ;; Выключить отображение панели инструментов


(require 'keymap)

(keymap-global-unset "M-,")     ;; Такие маркеры не нужны
(keymap-global-unset "C-z")     ;; Такой Ctrl+Z нам не нужен
(keymap-global-unset "C-x C-z") ;; `suspend-emacs' тоже не нужен
(keymap-global-unset "C-x C-p") ;; `mark-page' не нужна, часто конфликтует с Projectile

;; Включим переключение буферов по Ctrl+PgUp и Ctrl+PgDn
(keymap-global-unset "C-<next>")  ;; Ни разу не видел, что это было нужно
(keymap-global-unset "C-<prior>") ;; Это сочетание тоже не нужно.
(keymap-global-set "C-<next>" 'next-buffer)
(keymap-global-set "C-<prior>" 'previous-buffer)

;; Закрыть буфер по нажатию [C-x k]
(keymap-global-set "C-x k" (lambda() (interactive) (kill-buffer (current-buffer))))

;; Вставка длинного тире по нажатию [M--]
(keymap-global-set "M--" (lambda() (interactive) (insert "—")))


;; 📦 PACKAGE
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(setopt package-archive-priorities
        '(("gnu" . 40)
          ("nongnu" . 30)
          ("melpa-stable" . 20)
          ("melpa" . 10)))

(defun init-el-check-archive-contents ()
  ;; Check package archive contents state and update them if needed
  (unless package-archive-contents
    (progn
      (message "Обновление списка архивов...")
      (package-refresh-contents))))

(init-el-check-archive-contents)


(unless (package-installed-p 'gnu-elpa-keyring-update)
  (progn
    (message "Обновление ключей для проверки цифровой подписи.")
    (package-install 'gnu-elpa-keyring-update)))

(unless (package-installed-p 'use-package)
  (progn
    (message "Пакет `use-package' не установлен.")
    (message "Установка `use-package'...")
    (package-install 'use-package t)))

(require 'use-package)

;; Настройки отладочного режима
(when init-file-debug
  (setopt debug-on-error t ;; Автоматически перейти в режим отладки при ошибках.
          use-package-compute-statistics t ;; Сбор статистики `use-package'
          use-package-expand-minimally t ;; Минимальное раскрытие кода.
          use-package-verbose t)) ;; Подробный режим работы `use-package'.


;; 📦 TREESIT
;; Встроенный пакет для работы с TreeSitter
(use-package treesit
  :init
  (progn
    ;; Создадим каталог для хранения so-файлов с грамматиками
    (defvar init-el-tree-sitter-dir (expand-file-name "tree-sitter" user-emacs-directory))
    (unless (file-directory-p init-el-tree-sitter-dir)
      (make-directory init-el-tree-sitter-dir)))
  :config
  (progn
    ;; Грамматики
    (add-to-list 'treesit-language-source-alist '(bash "https://github.com/tree-sitter/tree-sitter-bash.git" "v0.25.0"))
    (add-to-list 'treesit-language-source-alist '(css "https://github.com/tree-sitter/tree-sitter-css.git" "v0.23.2"))
    (add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile.git" "v0.2.0" "src/"))
    (add-to-list 'treesit-language-source-alist '(hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl.git" "v1.2.0" "src/"))
    (add-to-list 'treesit-language-source-alist '(html "https://github.com/tree-sitter/tree-sitter-html.git" "v0.23.2"))
    (add-to-list 'treesit-language-source-alist '(javascript "https://github.com/tree-sitter/tree-sitter-javascript.git" "v0.25.0" "src/"))
    (add-to-list 'treesit-language-source-alist '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc.git" "v0.23.2" "src/"))
    (add-to-list 'treesit-language-source-alist '(json "https://github.com/tree-sitter/tree-sitter-json.git" "v0.24.8"))
    (add-to-list 'treesit-language-source-alist '(make "https://github.com/tree-sitter-grammars/tree-sitter-make.git" "v1.1.1" "src/"))
    (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown.git" "v0.5.0" "tree-sitter-markdown/src/"))
    (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown.git" "v0.5.0" "tree-sitter-markdown-inline/src/"))
    (add-to-list 'treesit-language-source-alist '(python "https://github.com/tree-sitter/tree-sitter-python.git" "v0.23.6"))
    (add-to-list 'treesit-language-source-alist '(ruby "https://github.com/tree-sitter/tree-sitter-ruby.git" "v0.23.1"))
    (add-to-list 'treesit-language-source-alist '(rust "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.24.0"))
    (add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "tsx/src"))
    (add-to-list 'treesit-language-source-alist '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml.git" "v0.7.1" "src/"))
    ;; Сборка и установка грамматик
    (unless (file-exists-p (expand-file-name "libtree-sitter-bash.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'bash init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-css.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'css init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-dockerfile.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'dockerfile init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-javascript.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'javascript init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-hcl.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'hcl init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-html.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'html init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-jsdoc.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'jsdoc init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-json.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'json init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-python.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'python init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-ruby.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'ruby init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-rust.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'rust init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-typescript.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'typescript init-el-tree-sitter-dir))
    (unless (file-exists-p (expand-file-name "libtree-sitter-yaml.so" init-el-tree-sitter-dir))
      (treesit-install-language-grammar 'yaml init-el-tree-sitter-dir)))
  :custom
  (treesit-font-lock-level 4 "По умолчанию — 3. Увеличим немного."))


;; 📦 ABBREV-MODE
;; Встроенный пакет.
;; Использование аббревиатур -- фрагментов текста, которые при вводе
;; определённой последовательности символов заменяются на другую.
(use-package abbrev
  :hook
  ((asciidoc-mode
    markdown-mode
    rst-mode) . abbrev-mode))


;; 📦 ANSI-COLOR
(use-package ansi-color
  :custom
  (ansi-color-for-compilation-mode t "Расцветка буфера *compile*")
  :hook
  (compilation-filter . ansi-color-compilation-filter))


;; 📦 AUTOREVERT
;; Встроенный пакет.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
;; Автоматическое обновление буферов.
;; По умолчанию `global-auto-revert-mode' работает только с файловыми
;; буферами.
(use-package autorevert
  :custom
  (auto-revert-check-vc-info t "Автоматически обновлять статусную строку")
  (global-auto-revert-non-file-buffers t "Автообновление не только файловых буферов.")
  :config
  (global-auto-revert-mode t)
  :hook
  (dired-mode . auto-revert-mode))


;; 📦 BROWSE-URL
;; Встроенный пакет, отвечающий за открытие и просмотр URL.
(use-package browse-url
  :custom
  (setopt browse-url-generic-program "chromium" "Браузер по умолчанию."))


;; 📦 CALENDAR
;; Встроенный пакет
(use-package calendar
  :custom
  (calendar-week-start-day 1 "Начнём неделю с понедельника."))


;; 📦 CHECKDOC
;; Встроенный пакет для проверки строк документации.
(use-package checkdoc
  :custom
  (checkdoc-minor-mode-string " CheckDoc")
  :hook
  (emacs-lisp-mode . checkdoc-minor-mode))


;; 📦 COMPILE
(use-package compile
  :custom
  (compilation-scroll-output t))


;; 📦 CONF-MODE
;; Встроенный пакет.
;; Основной режим для редактирования конфигурационных файлов INI/CONF
(use-package conf-mode
  :mode
  ("\\.env\\'"
   "\\.flake8\\'"
   "\\.pylintrc\\'"
   "\\inventory\\'"))


;; 📦 CSS-MODE
;; Встроенный пакет для работы с CSS
(use-package css-mode
  :custom
  (css-indent-offset 2 "Отступ 2 пробела"))


;; 📦 DELSEL
;; Встроенный пакет.
;; Используется для управления удалением выделенного текста.
(use-package delsel
  :config
  (delete-selection-mode t)) ;; Удалять выделенный фрагмент при вводе текста


;; 📦 DESKTOP
;; Встроенный пакет.
;; Сохранение состояния Emacs между сессиями.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(use-package desktop
  :custom
  (desktop-dirname user-emacs-directory "Каталог для хранения файла .desktop.")
  (desktop-load-locked-desktop t "Загрузка файла .desktop даже если он заблокирован.")
  (desktop-restore-frames t "Восстанавливать фреймы.")
  (desktop-save t "Сохранять список открытых буферов, файлов и т. д. без лишних вопросов.")
  :config
  (desktop-save-mode t)
  (add-hook 'after-init-hook 'desktop-read)
  (add-hook 'server-after-make-frame-hook 'desktop-read)
  (add-hook 'server-done-hook 'desktop-save)
  (add-to-list 'after-delete-frame-functions 'desktop-save)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode))


;; 📦 DIRED
;; Встроенный пакет для работы с файлами и каталогами.
(use-package dired
  :custom
  (dired-free-space 'separate "Информация о занятом и свободном месте в отдельной строке")
  (dired-kill-when-opening-new-dired-buffer t "Удалять буфер при переходе в другой каталог")
  (dired-listing-switches "-l --human-readable --all --group-directories-first --dired")
  (dired-recursive-deletes 'always "Не задавать лишних вопросов при удалении не-пустых каталогов")
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))


;; 📦 DISPLAY-LINE-NUMBERS-MODE
;; Встроенный пакет для показа номеров строк
(use-package display-line-numbers
  :hook
  ((asciidoc-mode
    c-mode
    conf-mode
    css-ts-mode
    csv-mode
    dockerfile-ts-mode
    emacs-lisp-mode
    html-ts-mode
    js-ts-mode
    json-ts-mode
    latex-mode
    lisp-data-mode
    makefile-mode
    markdown-mode
    mhtml-mode
    nxml-mode
    po-mode
    python-ts-mode
    rst-mode
    ruby-ts-mode
    sh-mode
    terraform-mode
    tex-mode
    yaml-ts-mode) . display-line-numbers-mode))


;; 📦 DOCKERFILE-TS-MODE
;; Встроенный пакет на базе TreeSitter для работы с Dockerfile.
(use-package dockerfile-ts-mode
  :mode
  ("\\Containerfile\\'"
   "\\Dockerfile\\'"))


;; 📦 ELECTRIC-INDENT MODE
;; Встроенный пакет.
;; Автоматический отступ. В основном только мешает, лучше выключить.
(use-package electric
  :hook
  ((emacs-lisp-mode
    markdown-mode
    mhtml-mode
    nxml-mode
    python-ts-mode
    rst-mode
    ruby-ts-mode) . electric-indent-local-mode))


;; 📦 ELEC-PAIR MODE
;; Встроенный пакет.
;; Автоматически вставляет при вводе одной скобки или кавычки парную ей. Если
;; выделен регион, то в скобки обрамляется он.
(use-package elec-pair
  :config
  (add-to-list 'electric-pair-pairs '(?\( . ?\))) ;; ()
  (add-to-list 'electric-pair-pairs '(?\[ . ?\])) ;; []
  (add-to-list 'electric-pair-pairs '(?{ . ?}))   ;; {}
  (add-to-list 'electric-pair-pairs '(?« . ?»))   ;; «»
  (add-to-list 'electric-pair-pairs '(?‘ . ’?))   ;; ‘’
  (add-to-list 'electric-pair-pairs '(?‚ . ‘?))   ;; ‚‘
  (add-to-list 'electric-pair-pairs '(?“ . ”?))   ;; “”)
  :hook
  ((asciidoc-mode
    conf-mode
    css-ts-mode
    emacs-lisp-data-mode
    emacs-lisp-mode
    html-ts-mode
    js-ts-mode
    json-ts-mode
    lisp-data-mode
    markdown-mode
    mhtml-mode
    nxml-mode
    org-mode
    python-ts-mode
    ruby-mode
    terraform-mode
    tex-mode
    yaml-ts-mode) . electric-pair-local-mode))


;; 📦 EMACS-LISP-MODE
;; IT IS NOT A ELISP-MODE!
;; Встроенный пакет для EMACS Lisp
(use-package elisp-mode
  :mode
  ("\\.abbrev_defs\\'" . lisp-data-mode)
  ("\\.el\\'" . emacs-lisp-mode))


;; 📦 FACE-REMAP
;; Встроенный пакет.
;; Отображение шрифтов в графическом режиме.
(use-package face-remap
  :custom
  (text-scale-mode-step 1.1 "Шаг увеличения масштаба"))


;; 📦 FILES
;; Это встроенный пакет для управления файлами
(use-package files
  :custom
  (auto-save-file-name-transforms `((".*" , init-el-autosave-dir) t))
  (delete-old-versions t "Удалять старые резервные копии файлов без лишних вопросов")
  (enable-local-eval t "Разрешить вызов `eval' в `.dir-locals.el'")
  (enable-local-variables :all "Считать все переменные из файлов `.dir-locals.el' безопасными")
  (large-file-warning-threshold (* 100 1024 1024) "Предупреждение при открытии файлов больше 100 МБ (по умолчанию — 10 МБ)")
  (make-backup-files nil "Резервные копии не нужны, у нас есть undo-tree")
  (require-final-newline t "Требовать новую строку в конце файлов")
  (save-abbrevs 'silently "Сохранять аббревиатуры без лишних вопросов")
  :config
  (progn
    (add-to-list 'safe-local-variable-values '(buffer-env-script-name . ".venv/bin/activate"))
    (add-to-list 'safe-local-variable-values '(electric-pair-preserve-balance . t))
    (add-to-list 'safe-local-variable-values '(emacs-lisp-docstring-fill-column . 80))
    (add-to-list 'safe-local-variable-values '(fill-column . 120))
    (add-to-list 'safe-local-variable-values '(fill-column . 80))
    (add-to-list 'safe-local-variable-values '(frozen_string_literal . true))
    (add-to-list 'safe-local-variable-values '(lexical-binding . t))
    (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
    (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode))
    (add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
    (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
    (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
    (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
    (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))))


;; 📦 FILL-COLUMN
;; Встроенный пакет.
;; Отображение рекомендуемой границы символов.
(use-package display-fill-column-indicator
  :hook
  ((emacs-lisp-mode
    js-ts-mode
    python-ts-mode
    yaml-ts-mode) . display-fill-column-indicator-mode))


;; 📦 FLYMAKE
;; Встроенный пакет для работы со статическими анализаторами.
(use-package flymake
  :hook (emacs-mode . flymake-mode))


;; 📦 FLYSPELL-MODE
;; Встроенный пакет.
;; Проверка орфографии с помощью словарей.
;; Использовать пакет только в том случае, когда дело происходит в
;; Linux и Hunspell или Aspell доступны.
(when (string-equal system-type "gnu/linux")
  (defvar text-spell-program nil "Программа для проверки орфографии.")
  (cond
   ((or
     (file-exists-p "/usr/bin/hunspell")
     (file-symlink-p "/usr/bin/hunspell"))
    (setq text-spell-program "hunspell"))
   ((or
     (file-exists-p "/usr/bin/aspell")
     (file-symlink-p "/usr/bin/aspell"))
    (setq text-spell-program "aspell")))
  ;; Нужно использовать ispell-mode только в том случае, когда есть
  ;; чем проверять орфографию.
  (if text-spell-program
      ;; then
      (progn
        (message (format "Для проверки орфографии используется %s" text-spell-program))
        (use-package flyspell
          :custom
          (ispell-program-name text-spell-program)
          :hook
          ((text-mode . flyspell-mode)
           (emacs-lisp-mode . flyspell-prog-mode))))
    ;; else
    (message "Не найдено программ для проверки орфографии.")))


;; 📦 FRAME
;; Встроенный пакет.
;; Управление фреймами.
(use-package frame
  :custom
  (window-divider-default-places 't "Разделители окон со всех сторон (по умолчанию только справа)")
  (window-divider-default-right-width 3  "Ширина в пикселях для линии-разделителя окон")
  (frame-resize-pixelwise t "Размер фреймов считать по пикселям а не по символам")
  :bind
  (:map global-map
        ("C-x O" . previous-window-any-frame) ;; Перейти в предыдущее окно
        ;; Перейти в следующее окно
        ("C-x o" . next-window-any-frame)
        ("M-o" . next-window-any-frame)))


;; 📦 GOTO-ADDRESS-MODE
;; Встроенный пакет.
;; Подсвечивает ссылки и позволяет переходить по ним с помощью [C-c RET].
;; Возможны варианты (зависит от основного режима).
(use-package goto-addr
  :hook
  ((asciidoc-mode
    emacs-lisp-mode
    html-ts-mode
    markdown-mode
    rst-mode) . goto-address-mode))


;; 📦 GREP
;; Встроенный пакет для поиска с помощью `grep'.
(use-package grep
  :config
  (add-to-list 'grep-find-ignored-directories "node_modules")
  :bind
  (:map global-map ("<f6>" . find-grep)))


;; 📦 HTML-TS-MODE
;; Встроенный пакет для работы с HTML и SGML.
(use-package html-ts-mode
  :mode
  ("\\.hbs\\'"
   "\\.html\\'"
   "\\.jinja\\'"))


;; 📦 IBUFFER
;; Встроенный пакет для удобной работы с буферами.
;; По нажатию F2 выводит список открытых буферов.
(use-package ibuffer
  :custom
  (ibuffer-formats '((mark      ;; Отметка
                      modified  ;; Буфер изменён?
                      read-only ;; Только чтение?
                      locked    ;; Заблокирован?
                      " "
                      (name 35 45 :left :elide) ;; Имя буфера: от 30 до 40 знаков
                      " "
                      (mode 8 -1 :left)         ;; Активный режим: от 8 знаков по умолчанию, при необходимости увеличить
                      " "
                      filename-and-process)     ;; Имя файла и процесс
                     ;; Сокращённый формат
                     (mark      ;; Отметка?
                      " "
                      (name 35 -1) ;; Имя буфера: 32 знака, при необходимости — расширить на сколько нужно
                      " "
                      filename)))  ;; Имя файла
  (ibuffer-default-sorting-mode 'filename/process "Сортировать файлы по имени / процессу")
  (ibuffer-display-summary nil "Не показывать строку ИТОГО")
  (ibuffer-eliding-string "…" "Если строка не уместилась, показать этот символ")
  (ibuffer-expert 1 "Не запрашивать подтверждение для опасных операций")
  (ibuffer-shrink-to-minimum-size t "Минимальный размер буфера по умолчанию")
  (ibuffer-truncate-lines nil "Не обкусывать длинные строки")
  (ibuffer-use-other-window t "Открывать буфер *Ibuffer* в отдельном окне")
  :init
  (defalias 'list-buffers 'ibuffer "Замена стандартной функции на ibuffer.")
  :bind
  (:map global-map ("<f2>" . ibuffer)))


;; 📦 IBUF-EXT
;; Встроенный пакет с дополнительными настройками `ibuffer'.
(use-package ibuf-ext
  :custom
  (ibuffer-saved-filter-groups                    ;; Группы по умолчанию
   '(("default"
      ("Dired" (mode . dired-mode))
      ("Emacs Lisp"
       (or
        (mode . emacs-lisp-mode)
        (mode . lisp-data-mode)))
      ("Org" (mode . org-mode))
      ("Markdown" (mode . markdown-mode))
      ("AsciiDoc" (mode . asciidoc-mode))
      ("ReStructured Text" (mode . rst-mode))
      ("CONF / INI"
       (or
        (mode . conf-mode)
        (mode . editorconfig-conf-mode)))
      ("XML" (mode . nxml-mode))
      ("YAML" (mode . yaml-ts-mode))
      ("Makefile" (mode . makefile-mode))
      ("Python" (mode . python-ts-mode))
      ("Ruby" (mode . ruby-ts-mode))
      ("SSH keys" (or (name . "^\\*.pub$")))
      ("Shell-script" (mode . sh-mode))
      ("Terraform" (mode . terraform-mode))
      ("SQL" (mode . sql-mode))
      ("Web"
       (or
        (mode . html-ts-mode)
        (mode . js-ts-mode)))
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)
        (name . "^\\*magit")
        (name . "git-monitor")))
      ("Commands"
       (or
        (mode . compilation-mode)
        (mode . eshell-mode)
        (mode . shell-mode)
        (mode . term-mode)))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*\\(Customize\\|Help\\)")
        (name . "\\*\\(Echo\\|Minibuf\\)"))))))
  (ibuffer-hidden-filter-groups (list "*Internal*" )) ;; Не показывать эти буферы
  (ibuffer-show-empty-filter-groups nil) ;; Не показывать пустые группы
  :hook
  (ibuffer-mode . ibuffer-auto-mode)
  (ibuffer-mode . (lambda ()(ibuffer-switch-to-saved-filter-groups "default"))))


;; 📦 JS-MODE
;; Встроенный пакет для работы с JavaScript.
(use-package js
  :custom
  (js-chain-indent t "Выравнивание при цепочке вызовов через точку.")
  (js-indent-level 2 "Отступ в 2 пробела, а не 4 (по умолчанию).")
  (js-switch-indent-offset 2 "Отступ в 2 пробела для switch/case.")
  :mode
  ("\\.js\\'" . js-ts-mode)
  ("\\(\\.js[mx]\\|\\.har\\)\\'" . js-ts-mode))


;; 📦 JSON-TS-MODE
;; Встроенный пакет для работы с JSON через TreeSitter
(use-package json-ts-mode
  :mode "\\.json\\'")


;; 📦 MAKEFILE
;; Встроенный пакет для работы с `Makefile'.
(use-package make-mode)


;; 📦 MINIBUFFER
;; Встроенный пакет для управления поведением минибуфера.
(use-package minibuffer
  :custom
  (completions-detailed t "Подробные подсказки в минибуфере"))


;; 📦 NEW-COMMENT
;; Встроенный пакет для работы с комментариями.
(use-package newcomment
  :bind
  (:map global-map
        ("M-'" . comment-or-uncomment-region)))


;; 📦 PAREN
;; Встроенный пакет для управления парными скобками.
(use-package paren
  :config
  (show-paren-mode t)) ;; Подсвечивать парные скобки


;; 📦 PIXEL-SCROLL
;; Встроенный пакет, позволяет плавно прокручивать текст
(when (package-installed-p 'pixel-scroll)
  (use-package pixel-scroll
    :config
    (progn
      (pixel-scroll-mode t)
      (pixel-scroll-precision-mode))))


;; 📦 PROG-MODE
;; Встроенный пакет, предоставляющий предка для всех программистских режимов.
(use-package prog-mode
  :hook
  (emacs-lisp-mode . prettify-symbols-mode))


;; 📦 PROJECT
;; Встроенный пакет для работы с проектами
(use-package project)


;; 📦 REPEAT-MODE
;; Встроенный пакет для повторения типовых действий
(use-package repeat
  :config
  (repeat-mode t)
  :hook
  (text-mode . repeat-mode))


;; 📦 REPLACE
;; Встроенный пакет.
;; Функции поиска и замены текста.
(use-package replace
  :bind
  (:map global-map
        ("<f3>" . replace-string)
        ("<f4>" . replace-regexp)))


;; 📦 RUBY-TS-MODE
;; Встроенный пакет для работы с Ruby.
(use-package ruby-ts-mode
  :mode
  ("\\.rb\\'"
   "Vagrantfile\\'"))


;; 📦 RUST-MODE
;; https://github.com/rust-lang/rust-mode
;; Поддержка языка Rust: https://rust-lang.org/
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :custom
  (rust-format-on-save t "Автоматическое форматирование буфера при сохранении.")
  :config
  (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil))))


;; 📦 SAVEPLACE
;; Встроенный пакет.
;; Запоминание позиции курсора в посещённых файлах.
(use-package saveplace
  :custom
  (save-place-forget-unreadable-files t "Не запоминать положение в нечитаемых файлах.")
  :config
  (save-place-mode t))


;; 📦 RST-MODE
;; Встроенный пакет для редактирования reStructutedText
;; https://www.writethedocs.org/guide/writing/reStructuredText/
(use-package rst
  :custom
  (rst-default-indent 3)
  (rst-indent-comment 3)
  (rst-indent-field 3)
  (rst-indent-literal-minimized 3)
  (rst-indent-width 3)
  (rst-preferred-adornments '((?# over-and-under 1)
                              (?* over-and-under 1)
                              (?= simple 0)
                              (?- simple 0)
                              (?^ simple 0)
                              (?\" simple 0)))
  (rst-toc-indent 3))


;; 📦 SAVEHIST
;; Встроенный пакет для запоминания истории команд
(use-package savehist
  :hook
  (kill-emacs . savehist-save)
  :config
  (add-to-list 'delete-frame-functions 'savehist-save)
  (savehist-mode t))


;; 📦 SHELL-SCRIPT-MODE
;; Встроенный пакет для работы со скриптами Shell.
(use-package sh-script
  :mode
  ("\\.bash_aliases\\'" . bash-ts-mode)
  ("\\.bashrc\\'" . bash-ts-mode)
  ("\\.envrc\\'" . sh-mode)
  ("\\.profile\\'" . sh-mode)
  ("\\.sh\\'" . sh-mode))


;; 📦 SHELL-MODE
;; Встроенный пакет.
;; Оболочка командной строки внутри Emacs
(use-package shell
  :custom
  (shell-kill-buffer-on-exit t "Закрыть буфер, если работа завершена."))


;; 📦 SIMPLE
;; Встроенный пакет.
;; Разные настройки управления элементарным редактированием текста.
(use-package simple
  :custom
  (backward-delete-char-untabify-method 'hungry "Удалять все символы выравнивания при нажатии [Backspace]")
  (blink-matching-paren t "Мигать, когда скобки парные")
  (indent-tabs-mode nil "Отключить `indent-tabs-mode'.")
  (kill-do-not-save-duplicates t "Не добавлять строку в kill-ring, если там уже есть такая же")
  (overwrite-mode nil "Выключить режим перезаписи.")
  (size-indication-mode nil "Выключить показ размера буфера в mode-line")
  (suggest-key-bindings t "Показывать подсказку клавиатурной комбинации для команды")
  :config
  (keymap-global-unset "<insert>") ;; Режим перезаписи не нужен
  :bind
  (:map global-map
        ("C-z" . undo)) ;; Отмена на Ctrl+Z
  :hook
  (compilation-mode . visual-line-mode)
  (messages-buffer-mode . visual-line-mode)
  (text-mode . visual-line-mode))


;; 📦 TEX
;; Встроенный пакет для работы с TeX и LaTeX.
(use-package tex-mode
  :mode
  ("\\.tex\\'" . tex-mode))


;; 📦 TOOLBAR
;; Встроенный пакет, недоступный в Emacs NOX
(when (fboundp 'tool-bar-mode)
  (use-package tool-bar
    :custom
    (tool-bar-mode nil "Выключить панель инструментов.")))


;; 📦 TOOLTIP
;; Встроенный пакет.
;; Вывод подсказок в графической среде.
(when (fboundp 'tooltip-mode)
  (use-package tooltip
    :config
    (tooltip-mode nil))) ;; Отключить использование GUI для вывода подсказок


;; 📦 UNIQUIFY
;; Встроенный пакет.
;; Используется для поддержания уникальности названий буферов, путей и т. д.
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward "Показывать каталог перед именем файла, если буферы одинаковые (по умолчанию имя<каталог>)")
  (uniquify-separator "/" "Разделять буферы с похожими именами, используя /"))


;; 📦 WHITESPACE MODE
;; Встроенный пакет.
;; Отображение невидимых символов.
(use-package whitespace
  :custom
  (whitespace-display-mappings ;; Отображение нечитаемых символов
   '((space-mark   ?\    [?\xB7]     [?.])        ;; Пробел
     (space-mark   ?\xA0 [?\xA4]     [?_])        ;; Неразрывный пробел
     (newline-mark ?\n   [?¶ ?\n]    [?$ ?\n])    ;; Конец строки
     (tab-mark     ?\t   [?\xBB ?\t] [?\\ ?\t]))) ;; TAB
  (whitespace-line-column 1000 "По умолчанию подсвечиваются длинные строки. Не надо этого делать.")
  :hook
  ((asciidoc-mode
    conf-mode
    css-ts-mode
    dockerfile-ts-mode
    emacs-lisp-mode
    html-ts-mode
    js-ts-mode
    json-ts-mode
    latex-mode
    lisp-data-mode
    makefile-gmake-mode
    makefile-mode
    markdown-mode
    nxml-mode
    org-mode
    po-mode
    python-ts-mode
    rst-mode
    ruby-ts-mode
    sh-mode
    snippet-mode ;; Yasnippet
    sql-mode
    terraform-mode
    tex-mode
    yaml-ts-mode) . whitespace-mode))


;; 📦 WINDMOVE
;; Встроенный пакет для быстрого переключения окон.
;; Перемещение между окнами Emacs.
(use-package windmove
  :config
  (windmove-default-keybindings 'ctrl)
  (windmove-swap-states-default-keybindings 'meta)
  (windmove-mode t))


;; 📦 WINNER-MODE
;; Встроенный пакет для управления состояниями окон.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Convenience.html
;; Для управления конфигурациями окон используются последовательности
;; [C-c <left>] и [C-c <right>]
(use-package winner
  :config
  (winner-mode t))


;; 📦 WINDOW
;; Встроенный пакет, отвечает за управление размерами окон
(use-package window
  :custom
  (window-resize-pixelwise t "Делить окна по пикселям, а не по символам.")
  :bind
  (:map global-map
        ("C-S-<iso-lefttab>" . next-buffer) ;; [Ctrl+Tab]       Вернуться в предыдущий буфер
        ("C-<tab>" . previous-buffer)))     ;; [Ctrl+Shift+Tab] Следующий буфер


;; 📦 XML
;; Встроенный пакет для работы с диалектами XML
(use-package xml
  :custom
  (nxml-attribute-indent 4 "Выравнивание атрибутов")
  (nxml-auto-insert-xml-declaration-flag nil "Не вставлять декларацию")
  (nxml-bind-meta-tab-to-complete-flag t "Использовать TAB для завершения ввода")
  (nxml-child-indent 4 "Выравнивание дочерних элементов")
  (nxml-slash-auto-complete-flag t "Закрывать теги по вводу /")
  :mode
  ("\\.pom\\'"
   "\\.xml\\'"))


;; 📦 YAML-TS-MODE
;; Встроенный пакет для работы с YAML через TreeSitter
(use-package yaml-ts-mode
  :mode
  ("\\.ansible\\-lint\\'"
   "\\.clang\\-tidy\\'"
   "\\.ya?ml\\'"
   "\\.yamllint\\'"
   "\\.yfm\\'"))


;;;;;; Здесь заканчиваются настройки встроенных пакетов и начинаются
;;;;;; настройки пакетов, полученных от чертей из интернета.


(defvar init-el-my-packages
  '(apheleia
    auctex
    adjust-parens
    all
    ansible
    avy
    buffer-env
    cape
    colorful-mode
    corfu
    counsel
    csv-mode
    cursor-undo
    denote
    doom-modeline
    edit-indirect
    editorconfig
    ef-themes
    eglot
    eldoc
    elpy
    flycheck
    flycheck-eglot
    format-all
    hl-todo
    hyperbole
    indent-bars
    ivy
    ivy-hydra
    jinx
    lin
    magit
    diff-hl
    markdown-mode
    modus-themes
    multiple-cursors
    nerd-icons
    nerd-icons-corfu
    nerd-icons-completion
    nerd-icons-dired
    nerd-icons-ibuffer
    org
    package-lint
    plantuml-mode
    po-mode
    projectile
    pulsar
    python
    rainbow-delimiters
    russian-techwriter
    rust-mode
    standard-themes
    swiper
    symbols-outline
    which-key
    yasnippet
    yasnippet-snippets))

(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (progn
      (init-el-check-archive-contents)
      (package-install pkg t))))


;; 📦 APHELEIA
;; https://github.com/radian-software/apheleia
;; Автоформат буфера перед сохранением.
(use-package apheleia
  :custom
  (apheleia-mode-lighter " ɑ" "Вместо длинного Apheleia")
  :hook
  ((emacs-lisp-mode
    python-ts-mode
    ruby-ts-mode) . apheleia-mode))


;; 📦 AUCTEX
;; IDE для работы с TeX
;; https://www.gnu.org/software/auctex/index.html
(use-package auctex)


;; 📦 ADJUST-PARENS
;; https://elpa.gnu.org/packages/adjust-parens.html
;; Пакет для автоматического управления скобочками и уровнями отступов.
(use-package adjust-parens
  :hook (emacs-lisp-mode . adjust-parens-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("<tab>" . lisp-indent-adjust-parens)
        ("<backtab>" . lisp-dedent-adjust-parens)))


;; 📦 ASCIIDOC-MODE
(use-package asciidoc-mode
  :load-path "~/repo/asciidoc-mode/"
  :mode "\\.adoc\\'")


;; 📦 ALL
;; https://elpa.gnu.org/packages/all.html
;; Это аналог `occur', только все найденные строки помещаются в отдельный буфер,
;; где их можно отредактировать, не прыгая по всему буферу. После изменений
;; достаточно нажать C-c C-c, и изменения отразятся в основном буфере
(use-package all)


;; 📦 ANSIBLE
;; https://gitlab.com/emacs-ansible/emacs-ansible
;; Дополнительные возможности при работе с YAML-файлами Ansible
(use-package ansible
  :defer t)


;; 📦 AVY
;; https://github.com/abo-abo/avy
;; Множество функций для быстрого перехода к нужной строке, слову, символу и
;; так далее.
(use-package avy
  :bind
  (:map global-map
        ("M-g f" . avy-goto-line)
        ("M-g w" . avy-goto-word)
        ("C-'" . avy-goto-char)))


;; 📦 BUFFER-ENV
;; https://github.com/astoff/buffer-env
;; Переменные окружения для отдельного буфера. Почти ENVRC, только от GNU
(use-package buffer-env
  :config
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  (add-hook 'comint-mode-hook #'buffer-env-update))


;; 📦 CAPE
;; https://github.com/minad/cape
;; Бэкенды автодополнения для CORFU
(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))


;; 📦 COLORFUL-MODE
;; https://github.com/DevelopmentCool2449/colorful-mode
;; Отображение цветов прямо в буфере. Наследник `raibow-mode.el'.
(use-package colorful-mode
  :hook
  ((css-ts-mode
    emacs-lisp-mode
    html-ts-mode
    json-ts-mode
    yaml-ts-mode) . colorful-mode))


;; 📦 CORFU
;; corfu.el - COmpletion in Region FUnction
;; Минималистичный аналог Company
(use-package corfu
  :hook
  ((css-ts-mode
    dockerfile-ts-mode
    emacs-lisp-mode
    html-ts-mode
    latex-mode
    lisp-data-mode
    nxml-mode
    org-mode
    python-ts-mode
    ruby-ts-mode
    tex-mode) . corfu-mode))


;; 📦 COUNSEL
;; https://elpa.gnu.org/packages/counsel.html
;; Автодополнение на основе Ivy
(use-package counsel
  :bind
  (:map global-map
        ("C-c c" . counsel-compile)
        ("C-c g" . counsel-git)
        ("C-h f" . counsel-describe-function)
        ("C-h l" . counsel-find-library)
        ("C-h v" . counsel-describe-variable)
        ("C-x 8 RET" . counsel-unicode-char)
        ("C-x C-f" . counsel-find-file)
        ("M-x" . counsel-M-x)
        ("M-y" . counsel-yank-pop)))


;; 📦 CSV-MODE
;; https://elpa.gnu.org/packages/csv-mode.html
;; Поддержка CSV
(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode))


;; 📦 CURSOR-UNDO
;; https://elpa.gnu.org/packages/cursor-undo.html
;; Отмена работает в том числе на перемещение курсора.
(use-package cursor-undo
  :config (cursor-undo t))


;; 📦 DENOTE
;; https://protesilaos.com/emacs/denote
;; Режим для управления заметками
(use-package denote
  :custom
  (denote-directory (expand-file-name "~/Notes/") "Каталог для хранения заметок."))


;; 📦 DOOM-MODELINE
;; https://github.com/seagle0128/doom-modeline
;; Красивая строка статуса
(use-package doom-modeline
  :custom
  (doom-modeline-total-line-number t "Общее количество строк")
  (doom-modline-vcs-max-length 20 "Видим имена длинных веток")
  (doom-modeline-irc nil "Не показывать статус IRC")
  (doom-modeline-battery nil "У меня нет батареи, показывать нечего")
  :config (doom-modeline-mode t))


;; 📦 EDIT-INDIRECT
;; https://github.com/Fanael/edit-indirect
;; Позволяет редактировать выделенный регион в отдельном буфере.
;; Это может быть полезно в том случае, когда, например, нужно
;; отредактировать код программы, вставленный как пример в документацию.
;; Самая полезная команда — edit-indirect-region:
;; 1. Выделяем область.
;; 2. Нажимаем [C-c '].
;; 3. Редактируем.
;; 4. Нажимаем [C-c], чтобы вернуться в основной буфер и подтвердить изменения,
;; либо [C-c C-k], чтобы отменить правки.
(use-package edit-indirect
  :bind (:map global-map
              ("C-c '" . edit-indirect-region)))


;; 📦 EDITORCONFIG
;; Поддержка https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :config
  (editorconfig-mode t))


;; 📦 EF-THEMES
;; https://github.com/protesilaos/ef-themes.git
(use-package ef-themes)


;; 📦 EGLOT
;; Пакет для поддержки LSP.
;; https://elpa.gnu.org/packages/eglot.html
;;
;; ПОДГОТОВКА К РАБОТЕ
;; Установка серверов:
;; - Ansible:    sudo npm -g install @ansible/ansible-language-server
;; - Dockerfile: sudo npm -g install dockerfile-language-server-nodejs
;; - HTML:       sudo npm -g install vscode-langservers-extracted
;; - Markdown:   sudo snap install marksman
;; - Python:     pip3 install jedi-language-server
;; - ReST        pip3 install esbonio
;;               Создать в корне проекта файл .dir-locals.el и задать значение
;;               переменной `eglot-workspace-configuration'.
;; - YAML:       sudo npm -g install yaml-language-server
(use-package eglot
  :defer t
  :custom
  (eglot-events-buffer-config '(
                                :size 0 ;; Выключить ведение буфера событий
                                :format 'lisp ;; Формат Lisp для логов
                                )
                              "Настройки буфера событий Eglot")
  :config
  (progn
    (add-to-list 'eglot-server-programs '(ansible-mode . ("ansible-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs '(dockerfile-ts-mode . ("docker-langserver" "--stdio")))
    (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
    (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
    (add-to-list 'eglot-server-programs '(ruby-ts-mode . ("bundle" "exec" "rubocop" "--lsp")))
    (add-to-list 'eglot-server-programs '(yaml-ts-mode . ("yaml-language-server" "--stdio"))))
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . eldoc)
        ("C-c C-r" . eglot-rename)
        ("C-c C-f" . eglot-format-buffer))
  :hook
  ((ansible-mode
    dockerfile-ts-mode
    markdown-mode
    python-ts-mode
    ruby-ts-mode
    rust-mode
    yaml-ts-mode
    ) . eglot-ensure))


;; 📦 ELDOC-MODE
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Programming-Language-Doc.html
;; Отображение подсказок при работе с Emacs Lisp
(use-package eldoc
  :config
  (global-eldoc-mode nil)
  :custom
  (eldoc-minor-mode-string "" "Не надо показывать ничего в строке статуса.")
  :hook (emacs-lisp-mode . eldoc-mode))


;; 📦 ELPY
;; https://elpy.readthedocs.io/en/latest/index.html
;; Emacs Lisp Python IDE
(use-package elpy
  :config (elpy-enable))


;; 📦 FLYCHECK
;; https://www.flycheck.org/
;; Проверка синтаксиса на лету с помощью статических анализаторов
(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save new-line))
  (flycheck-highlighting-mode 'lines "Стиль отображения проблемных мест — вся строка")
  (flycheck-indication-mode 'left-fringe "Место размещения маркера ошибки — левая граница")
  (flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc" "Файл настроек Markdownlint")
  (flycheck-sphinx-warn-on-missing-references t "Предупреждать о некорректных ссылках в Sphinx")
  (flycheck-textlint-config ".textlintrc.yaml" "Файл настроек Textlint")
  :hook
  ((asciidoc-mode
    conf-mode
    css-ts-mode
    dockerfile-ts-mode
    emacs-lisp-mode
    html-ts-mode
    js-ts-mode
    json-ts-mode
    latex-mode
    lisp-data-mode
    makefile-mode
    markdown-mode
    nxml-mode
    python-ts-mode
    rst-mode
    ruby-ts-mode
    sh-mode
    sql-mode
    terraform-mode
    yaml-ts-mode
    ) . flycheck-mode))


;; 📦 FLYCHECK-EGLOT
;; https://github.com/flycheck/flycheck-eglot
;; Интеграция Flycheck с Eglot
(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode t))


;; 📦 FORMAT-ALL
;; https://github.com/lassik/emacs-format-all-the-code
;; Форматирование кода с помощью разных внешних средств.
(use-package format-all
  :defer t
  :bind
  (:map global-map
        ([f12] . format-all-buffer)))


;; 📦 HL-TODO
;; https://github.com/tarsius/hl-todo
;; Подсветка TODO, FIXME и т. п.
(use-package hl-todo
  :config (global-hl-todo-mode t))


;; 📦 HYPERBOLE
;; https://www.gnu.org/software/hyperbole/
;; Распознаёт текст в буферах и автоматически превращает в кнопки и ссылки.
(use-package hyperbole
  :custom
  (hyperbole-mode-lighter nil "Убрать индикатор из статусной строки")
  :hook
  ((emacs-lisp-mode
    markdown-mode
    rst-mode
    text-mode) . hyperbole-mode))


;; 📦 INDENT-BARS
;; https://github.com/jdtsmith/indent-bars
;; Красивая подсветка отступов
(use-package indent-bars
  :hook
  ((emacs-lisp-mode
    js-ts-mode
    makefile-mode
    markdown-mode
    python-ts-mode
    rst-mode
    ruby-ts-mode
    yaml-ts-mode
    ) . indent-bars-mode))


;; 📦 IVY
;; https://elpa.gnu.org/packages/ivy.html
;; https://elpa.gnu.org/packages/doc/ivy.html
;; Функции фильтрации и выбора элементов. Как Helm, но теперь в GNU ELPA.
;; При переименовании файлов рекомендуется использовать `ivy-immediate-done',
;; это последовательность [C-M-j].
(use-package ivy
  :demand t
  :config
  (ivy-mode t)
  :bind
  (:map global-map
        ("C-x b" . ivy-switch-buffer)
        ("C-c v" . ivy-push-view)
        ("C-c V" . ivy-pop-view)))


;; 📦 IVY-HYDRA
;; https://elpa.gnu.org/packages/ivy-hydra.html
;; Дополнительные сочетания клавиш для IVY.
(use-package ivy-hydra
  :demand t
  :after ivy
  :requires ivy)


;; 📦 JINX
;; https://github.com/minad/jinx
;; Проверка орфографии не только для слова под курсором, как во `flyspell',
;; а вообще во всём буфере.
;; В Debian требует для работы пакеты `libenchant2-dev' и `pkgconf'.
(use-package jinx
  :custom
  (jinx-languages "ru_RU en_US")
  :hook ((emacs-lisp-mode
          text-mode) . jinx-mode)
  :bind
  (:map global-map
        ("M-$" . jinx-correct)
        ("C-M-$" . jinx-languages)
        ("M-n" . jinx-next)
        ("M-p" . jinx-previous)))


;; 📦 LIN
;; https://github.com/protesilaos/lin
;; Почти то же самое, что и `hl-line-mode', только лучше.
;; TODO: в чём именно?
(use-package lin
  :config
  (lin-global-mode t))


;; 📦 MAGIT
;; https://magit.vc/
;; Magic + Git + Diff-HL.
;; Лучшее средство для работы с Git.
(use-package magit
  :custom
  (magit-define-global-key-bindings 'default "Включить глобальные сочетания Magit.")
  (magit-show-long-lines-warning nil "Выключить предупреждения про длинные строки.")
  :hook
  (magit-mode . magit-auto-revert-mode)
  (after-save . magit-after-save-refresh-status)
  (after-save . magit-after-save-refresh-buffers))


;; 📦 DIFF-HL
;; https://github.com/dgutov/diff-hl
;; Показывает небольшие маркеры рядом с незафиксированными изменениями.
;; Дополняет функциональность git-gutter, который показывает изменения только в
;; обычных буферах. Этот пакет умеет работать с dired и другими режимами.
(use-package diff-hl
  :hook
  ((asciidoc-mode
    emacs-lisp-mode
    makefile-mode
    markdown-mode
    python-ts-mode
    rst-mode
    yaml-ts-mode). diff-hl-mode)
  ((dired-mode . diff-hl-dired-mode)))


;; 📦 MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
;; Режим для работы с файлами в формате Markdown
(use-package markdown-mode
  :defer t
  :custom
  (markdown-fontify-code-blocks-natively t "Подсвечивать синтаксис в примерах кода")
  (markdown-header-scaling-values '(1.0 1.0 1.0 1.0 1.0 1.0) "Все заголовки одной высоты")
  (markdown-list-indent-width 4 "Размер отступа для выравнивания вложенных списков")
  :config (setq-local word-wrap t)
  :bind (:map markdown-mode-map
              ("M-." . markdown-follow-thing-at-point)))


;; 📦 MODUS-THEMES
;; https://www.gnu.org/software/emacs/manual/html_node/modus-themes/index.html
(use-package modus-themes)


;; 📦 MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
;; Позволяет использовать мультикурсорность.
(use-package multiple-cursors
  :custom (mc/always-run-for-all t "Не задавать лишних вопросов.")
  :init
  (keymap-global-unset "M-<down-mouse-1>")
  :bind
  (:map global-map
        ("C-S-c C-S-c" . mc/edit-lines)
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c C-<" . mc/mark-all-like-this)
        ("M-<mouse-1>" . mc/add-cursor-on-click)))


;; 📦 NERD-ICONS
;; https://github.com/rainstormstudio/nerd-icons.el
;; Для установки самих шрифтов следует использовать команду `nerd-icons-install-fonts'.
;; В Debian Linux шрифты будут загружены в каталог `~/.local/share/fonts'. Рекомендуется
;; скопировать их в `/usr/local/share/fonts/'.
(use-package nerd-icons
  :custom
  (nerd-icons-color-icons t "Использовать цветные иконки."))


;; 📦 NERD-ICONS-CORFU
;; https://github.com/LuigiPiucco/nerd-icons-corfu
;; Иконки в CORFU
(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; 📦 NERD-ICONS-COMPLETION
;; https://github.com/rainstormstudio/nerd-icons-completion
;; Иконки в автозавершении ввода.
(use-package nerd-icons-completion
  :config (nerd-icons-completion-mode t))


;; 📦 NERD-ICONS-DIRED
;; https://github.com/rainstormstudio/nerd-icons-dired
;; Иконки в `dired'.
(use-package nerd-icons-dired
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))


;; 📦 NERD-ICONS-IBUFFER
;; https://github.com/seagle0128/nerd-icons-ibuffer
;; Отображение иконок в ibuffer
(use-package nerd-icons-ibuffer
  :after (ibuffer nerd-icons)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; 📦 ORG-MODE
;; https://orgmode.org/
;; Органайзер, заметки и так далее
(use-package org
  :defer t
  :config
  (setq-local
   truncate-lines nil ;; Не обрезать строки
   word-wrap t))      ;; Перенос длинных строк


;; 📦 PACKAGE-LINT
;; https://github.com/purcell/package-lint
;; Проверка кода пакетов Emacs.
(use-package package-lint
  :defer t)


;; 📦 PLANTUML-MODE
;; https://github.com/skuro/plantuml-mode
;; Пакет для работы с PlantUML
(use-package plantuml-mode)


;; 📦 PO-MODE
;; https://www.gnu.org/software/gettext/manual/html_node/Installation.html
;; Работа с файлами локализации.
;; Необходимо установить в систему эти пакеты:
;; * gettext
;; * gettext-el: если po-mode из архивов не работает
(use-package po-mode
  :pin "melpa")


;; 📦 PROJECTILE
;; https://docs.projectile.mx/projectile/installation.html
;; Управление проектами. Чтобы каталог считался проектом, он должен быть
;; под контролем любой системы версионирования, либо содержать специальные
;; файлы. В крайнем случае сгодится пустой файл .projectile
;; Подробнее здесь: https://docs.projectile.mx/projectile/projects.html
(use-package projectile
  :bind-keymap
  ("C-x p" . projectile-command-map)
  ("C-c p" . projectile-command-map)
  :bind
  (:map global-map
        ("<f6>" . projectile-test-project)
        ("<f9>" . projectile-compile-project))
  :init
  (progn
    (add-to-list 'safe-local-variable-values '(projectile-project-compilation-cmd . "make dirhtml"))
    (add-to-list 'safe-local-variable-values '(projectile-project-compilation-cmd . "make docker-clear && make docker-dirhtml"))
    (add-to-list 'safe-local-variable-values '(projectile-project-compilation-cmd . "make docker-dirhtml"))
    (add-to-list 'safe-local-variable-values '(projectile-project-test-cmd . "pre-commit run --all")))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-mode t))


;; 📦 PULSAR
;; Вспыхивание строки, к которой переместился курсор
;; https://github.com/protesilaos/pulsar
;; Этот пакет требует Emacs версии 27.1 или новее
(use-package pulsar
  :custom
  (pulsar-pulse t)
  (ring-bell-function 'pulsar-pulse-line "Вместо звонка подсветить строку")
  :config
  (progn
    (add-hook 'after-init-hook 'pulsar-global-mode)
    (add-hook 'next-error-hook 'pulsar-pulse-line)
    (add-to-list 'pulsar-pulse-functions 'flycheck-next-error)
    (add-to-list 'pulsar-pulse-functions 'flyspell-goto-next-error)
    (add-to-list 'pulsar-pulse-functions 'recenter-top-bottom)))


;; 📦 PYTHON-TS-MODE
;; Встроенный пакет для работы с Python через TreeSitter
(use-package python
  :custom
  (py-pylint-command-args "--max-line-length 120" "Дополнительные параметры, передаваемые pylint")
  (python-indent-guess-indent-offset-verbose nil "Выключить уведомления")
  (python-indent-offset 4 "Отступ по умолчанию — 4 пробела"))


;; 📦 RAINBOW-DELIMITERS-MODE
;; https://github.com/Fanael/rainbow-delimiters
;; Подсветка парных скобок одним и тем же цветом
(use-package rainbow-delimiters
  :hook
  ((asciidoc-mode
    conf-mode
    css-ts-mode
    emacs-lisp-mode
    js-ts-mode
    lisp-data-mode
    makefile-gmake-mode
    makefile-mode
    markdown-mode
    nxml-mode
    org-mode
    python-ts-mode
    rst-mode
    sh-mode
    sql-mode
    terraform-mode
    yaml-ts-mode
    ) . rainbow-delimiters-mode))


;; 📦 RUSSIAN-TECHWRITER
;; Метод ввода для технических писателей.
;; В отличие от russian-computer, позволяет использовать лигатуры.
;; https://github.com/dunmaksim/emacs-russian-techwriter-input-method
(use-package russian-techwriter
  :custom
  (default-qinput-method "russian-techwriter" "Метод ввода по умолчанию.")
  (default-transient-input-method "russian-techwriter" "Временный метод ввода"))


;; 📦 STANDARD THEMES
;; https://github.com/protesilaos/standard-themes
;; Улучшенные темы на основе стандартных
(use-package standard-themes
  :custom
  (standard-themes-bold-constructs t)
  (standard-themes-italic-constructs t))


;; 📦 SWIPER
;; https://elpa.gnu.org/packages/swiper.html
;; Умный поиск и отличная (в некоторых случаях) замена `isearch-forward' и
;; `isearch-backward'.
(use-package swiper
  :bind
  (:map global-map
        ("C-s" . swiper-isearch)
        ("C-r" . swiper-isearch-backward)))


;; SYMBOLS-OUTLINE
;; https://github.com/liushihao456/symbols-outline.el
;; Показывает переменные, функции, заголовки и другие части файла
;; в отдельном окне и позволяет быстро перемещаться между ними.
;; Для корректной работы нужна утилита ctags. В Debian Linux это пакет
;; universal-ctags
(use-package symbols-outline
  :custom
  (symbols-outline-window-width 40 "Ширина окна")
  :bind (:map global-map
              ("C-c i" . symbols-outline-show))
  :hook
  ((asciidoc-mode
    emacs-lisp-mode
    markdown-mode
    python-ts-mode
    rst-mode) . symbols-outline-follow-mode))


;; 📦 WHICH-KEY MODE
;; https://elpa.gnu.org/packages/which-key.html
;; Показывает подсказки к сочетаниям клавиш.
(use-package which-key
  :custom
  (which-key-compute-remaps t "Выводить актуальные сочетания клавиш, а не «как должно быть»")
  (which-key-dont-use-unicode nil "Используем Unicode")
  (which-key-idle-delay 2 "Задержка появления подсказки")
  (which-key-idle-secondary-delay 0.05 "Ещё одна задержка появления подсказки")
  (which-key-lighter nil "Справимся и так, не надо ничего показывать в строке статуса.")
  (which-key-separator " → " "Разделитель сочетаний и команд")
  (which-key-show-major-mode t "То же самое что и [C-h m], но в формате which-key")
  :config
  (which-key-mode t))


;; 📦 YASNIPPET
;; https://elpa.gnu.org/packages/yasnippet.html
;; Библиотека для управления сниппетами. Требуется для расширения функций Eglot.
(use-package yasnippet
  :init
  (progn
    ;; Создать каталог для хранения сниппетов, иначе будет ошибка
    (defvar init-el-yasnippet-snippets-dir (expand-file-name "snippets" user-emacs-directory))
    (unless (file-directory-p init-el-yasnippet-snippets-dir)
      (make-directory init-el-yasnippet-snippets-dir)))
  :config (yas-global-mode t))


;; 📦 YASNIPPET-SNIPPETS
;; https://github.com/AndreaCrotti/yasnippet-snippets
;; Набор сниппетов для `yasnippet'
(use-package yasnippet-snippets)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(load-theme 'ef-autumn t)

(provide 'init.el)
;;; init.el ends here
