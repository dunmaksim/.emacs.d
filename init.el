;;; init.el --- Dunaevsky Maxim GNU EMACS config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Настройки EMACS для работы техническим писателем.

;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p) ;; Использовать y и n вместо yes и no (сокращает объём вводимого текста для подтверждения команд)

(defconst init-el-font-height 18 "Размер шрифта по умолчанию.")

(defun init-el-set-font-height ()
  "Установка размера шрифта.
Размер шрифта устанавливается в pt в 10 раз больше чем указано в FONT-HEIGHT."
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

(defun init-el-set-font (font-family)
  "Эта функция устанавливает семейство шрифтов FONT-FAMILY как предпочтительное."
  ;; Это формат X Logical Font Description Conventions, XLFD
  ;; https://www.x.org/releases/X11R7.7/doc/xorg-docs/xlfd/xlfd.html
  (set-frame-font (format "-*-%s-normal-normal-normal-*-%d-*-*-*-m-0-iso10646-1"
                          font-family
                          init-el-font-height)
                  nil ;; Не сохранять установленный ранее размер
                  t   ;; Применить ко всем фреймам
                  t)  ;; Игнорировать настройки, сделанные через `customize'
  (set-face-attribute
   'default ;; Font Face по умолчанию
   nil      ;; Применить ко всем фреймам
   ;; Атрибуты шрифта
   :height (* init-el-font-height 10)
   :family font-family))


;; Настройки, специфичные для графического режима
(defun setup-gui-settings (&optional frame-name)
  "Настройки, необходимые при запуске EMACS в графической среде.
FRAME-NAME — название настраиваемого фрейма."
  (when (display-graphic-p frame-name) ;; Фрейм графический
    ;; Получаем список шрифтов
    (let ((font-families (font-family-list))
          (lilex "Lilex")
          (sauce-code-pro "SauceCodePro NFP")
          (fira-code-nerd-font-mono "FiraCode Nerd Font Mono")
          (fira-code "Fira Code")
          (dejavu-sans-mono-nerd "DejaVu Sans Mono Nerd")
          (dejavu-sans-mono "DejaVu Sans Mono")
          (source-code-pro "Source Code Pro")
          (consolas "Consolas"))
      ;; Мои любимые шрифты, от самого любимого к менее любимому
      (let ((preferred-font-family (cond ((member lilex font-families) lilex)
                                         ((member sauce-code-pro font-families) sauce-code-pro)
                                         ((member fira-code-nerd-font-mono font-families) fira-code-nerd-font-mono)
                                         ((member fira-code font-families) fira-code)
                                         ((member dejavu-sans-mono-nerd font-families) dejavu-sans-mono-nerd)
                                         ((member dejavu-sans-mono font-families) dejavu-sans-mono)
                                         ((member source-code-pro font-families) source-code-pro)
                                         ((member consolas font-families) consolas)
                                         (t nil))))
        (when preferred-font-family
          (progn
            (message (format "Шрифт по умолчанию: %s" preferred-font-family))
            (init-el-set-font preferred-font-family)))))))

;; Правильный способ определить, что EMACS запущен в графическом режиме. Подробнее здесь:
;; https://emacsredux.com/blog/2022/06/03/detecting-whether-emacs-is-running-in-terminal-or-gui-mode/
;; Настройка шрифтов для обычного режима
(add-hook 'after-init-hook (lambda ()(setup-gui-settings (selected-frame))))
;; Настройка шрифтов при работе в режиме сервера
(add-hook 'server-after-make-frame-hook (lambda ()(setup-gui-settings (selected-frame))))
;; Настройка шрифтов в новых фреймах в любом режиме
(add-to-list 'after-make-frame-functions 'setup-gui-settings)


(global-font-lock-mode t)  ;; Отображать шрифты красиво, используя Font Face's


;; Определение пути к каталогу с исходным кодом
;; Исходный код нужен для тех случаев, когда хочется посмотреть код пакета
;; или ядра Emacs.
(when (string-equal system-type "gnu/linux")
  (message "Используется ОС на базе GNU/Linux")
  ;; Поищем исходный код в /usr/share/emacs/X.Y/src/, где X и Y мажорная и
  ;; минорная версия Emacs соответственно.
  (let ((emacs-source-path (format "/usr/share/emacs/%d.%d/src/"
                                   emacs-major-version
                                   emacs-minor-version)))
    ;; Проверим, существует ли каталог
    (if (file-exists-p emacs-source-path)
        ;; Проверяем, пуст ли каталог
        (if (directory-empty-p emacs-source-path)
            ;; Каталог пуст
            (message (format "Каталог %s пуст." emacs-source-path))
          ;; Каталог не пуст
          (progn
            (setopt source-directory emacs-source-path)
            (message (format "Исходный код обнаружен в каталоге %s" emacs-source-path))))
      ;; Каталог не существует
      (message (format "Каталог %s не существует." emacs-source-path)))))


(setopt
 case-fold-search t ;; Игнорировать регистр при поиске.
 completion-ignore-case t ;; Игнорировать регистр при автодополнении
 create-lockfiles nil ;; Не создавать lock-файлы
 cursor-in-non-selected-windows nil ;; Отключить курсор в неактивных окнах
 cursor-type 'bar ;; Курсор в виде вертикальной черты
 default-directory "~/repo/" ;; Директория по умолчанию
 default-input-method "russian-computer" ;; Метод ввода по умолчанию
 default-transient-input-method "russian-computer" ;; Временный метод ввода
 delete-by-moving-to-trash t ;; Удалять файлы в Корзину
 gc-cons-threshold (* 2 gc-cons-threshold) ;; Увеличить размер памяти для сборщика мусора
 highlight-nonselected-windows nil ;; Не подсвечивать неактивные окна
 inhibit-compacting-font-caches t ;; Не сжимать шрифты в памяти
 inhibit-startup-screen t ;; Не показывать приветственный экран
 initial-scratch-message nil ;; Пустой буфер *scratch*
 kill-buffer-delete-auto-save-files t ;; Удалять файлы автосохранения при закрытии буфера
 load-prefer-newer t ;; Если есть файл elc, но el новее, загрузить el-файл.
 long-line-threshold (* long-line-threshold 2) ;; Вдвое увеличим порог
 major-mode 'text-mode ;; Текстовый режим для новых буферов по умолчанию.
 read-answer-short t ;; Быстрый ввод ответов на вопросы (не аналог yes-or-no-p
 read-buffer-completion-ignore-case t ;; Игнорировать регистр при вводе названия буфера
 read-extended-command-predicate #'command-completion-default-include-p ;; Скрыть команды, которые нельзя выполнить в буфере
 read-file-name-completion-ignore-case t ;; Игнорировать регистр при вводе имён файлов
 read-process-output-max (* read-process-output-max 2) ;; Увеличим чанк чтения для LSP в 2 раза
 redisplay-skip-fontification-on-input t ;; Не обновлять буфер, если происходит ввод
 ring-bell-function 'ignore ;; Отключить звуковое сопровождение событий
 sentence-end-double-space nil ;; Устаревшее требование
 show-trailing-whitespace t ;; Подсветка висячих пробелов
 standard-indent 4 ;; Отступ по умолчанию
 tab-always-indent 'complete ;; Если можно — выровнять текст, иначе — автодополнение.
 use-dialog-box nil ;; Диалоговые окна ОС не нужны
 use-file-dialog nil ;; Файловые диалоги тоже не нужны
 use-short-answers t ;; Краткие ответы вместо длинных
 user-full-name "Dunaevsky Maxim" ;; Имя пользователя
 user-mail-address "dunmaksim@yandex.ru" ;; Адрес электронной почты
 vc-follow-symlinks t ;; Переходить по ссылкам без лишних вопросов
 visible-bell t) ;; Мигать буфером при переходе в него


;; Буфер *scratch* не нужен, если вы не программист Emacs Lisp
(defun init-kill-scratch ()
  "Закрыть буфер *scratch* при запуске редактора или подключении клиента."
  (when (get-buffer "*scratch*")
    (kill-buffer "*scratch*")))
(add-hook 'after-init-hook 'init-kill-scratch)
(add-hook 'server-after-make-frame-hook 'init-kill-scratch)

;; Меню не нужно
(when (fboundp 'menu-bar-mode)
  (setopt menu-bar-mode nil))

;; Полосы прокрутки не нужны
(when (fboundp 'scroll-bar-mode)
  (setopt scroll-bar-mode nil))

;; Панель инструментов не нужна
(when (fboundp 'tool-bar-mode)
  (setopt tool-bar-mode nil))


;; Изменим некоторые привязки клавиш по умолчанию
(require 'keymap)
(keymap-global-unset "M-,")     ;; Такие маркеры не нужны
(keymap-global-unset "C-z")     ;; Такой Ctrl+Z нам не нужен
(keymap-global-unset "C-x C-z") ;; `suspend-emacs' тоже не нужен
(keymap-global-unset "C-x C-p") ;; `mark-page' не нужна, часто конфликтует с Projectile

;; Включим переключение буферов по Ctrl+PgUp и Ctrl+PgDn
(keymap-global-unset "C-<next>")
(keymap-global-unset "C-<prior>")
(keymap-global-set "C-<next>" 'next-buffer) ;;
(keymap-global-set "C-<prior>" 'previous-buffer)

;; Закрыть буфер по нажатию [C-x k]
(defun init-el-kill-current-buffer ()
  "Закрыть активный буфер."
  (interactive)
  (kill-buffer (current-buffer)))
(keymap-global-set "C-x k" 'init-el-kill-current-buffer)

;; Вставка длинного тире по нажатию [M--]
(keymap-global-set "M--" (lambda() (interactive) (insert "—")))


;; 📦 PACKAGE
;; Настроим архивы:
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; Приоритеты архивов: чем выше, тем лучше.
(setopt package-archive-priorities
        '(("gnu" . 2)
          ("nongnu" . 1)))

;; Не надо регистрировать как проекты пакеты, установленные с помощью
;; `package-vc-install'.
(setopt package-vc-register-as-project nil)

(defun init-el-check-archive-contents ()
  "Проверим наличие списка пакетов в архивах.
Если списка нет, то создадим его."
  (unless package-archive-contents
    (package-refresh-contents)))

(init-el-check-archive-contents)


;; Проверим наличие пакета `gnu-elpa-keyring-update'.
;; В некоторых случаях без него Emacs не может проверить цифровые
;; подписи пакетов.
(unless (package-installed-p 'gnu-elpa-keyring-update)
  (progn
    (message "Обновление ключей для проверки цифровой подписи.")
    (package-install 'gnu-elpa-keyring-update t)))

;; Проверяем наличие пакета `use-package'.
;; В новых версиях Emacs он встроенный, но в старых его может не быть.
(unless (package-installed-p 'use-package)
  (package-install 'use-package t))

;; Если `use-package' встроенный, обновим из архива GNU ELPA.
(unless (alist-get 'use-package package-alist)
  (package-upgrade 'use-package))

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
  ;; Проверим существование подкаталога tree-sitter. При необходимости создадим.
  (let ((ts-lib-dir (expand-file-name "tree-sitter" user-emacs-directory)))
    (unless (file-directory-p ts-lib-dir)
      (make-directory ts-lib-dir)))
  :config
  ;; Грамматики
  (add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile.git" "v0.2.0" "src/"))
  (add-to-list 'treesit-language-source-alist '(javascript "https://github.com/tree-sitter/tree-sitter-javascript.git" "v0.23.1" "src/"))
  (add-to-list 'treesit-language-source-alist '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc.git" "v0.23.1" "src/"))
  (add-to-list 'treesit-language-source-alist '(json "https://github.com/tree-sitter/tree-sitter-json.git" "v0.24.8"))
  (add-to-list 'treesit-language-source-alist '(make "https://github.com/tree-sitter-grammars/tree-sitter-make.git" "v1.1.1" "src/"))
  (add-to-list 'treesit-language-source-alist '(ruby "https://github.com/tree-sitter/tree-sitter-ruby.git" "v0.23.1"))
  ;; Нужна более новая версия TreeSitter в самом Emacs
  ;; (add-to-list 'treesit-language-source-alist '(rust "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.24.0"))
  (add-to-list 'treesit-language-source-alist '(typst "https://github.com/uben0/tree-sitter-typst.git"))
  (add-to-list 'treesit-language-source-alist '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml.git" "v0.7.2" "src/"))
  ;; Сборка и установка грамматик
  (dolist (source treesit-language-source-alist)
    (unless (treesit-ready-p (car source))
      (treesit-install-language-grammar (car source))))
  :bind
  (:map global-map
        ("<f5>" . treesit-explore-mode)))


;; 📦 ABBREV-MODE
;; Встроенный пакет.
;; Использование аббревиатур -- фрагментов текста, которые при вводе
;; определённой последовательности символов заменяются на другую.
(use-package abbrev
  :hook
  (asciidoc-mode . abbrev-mode)
  (markdown-mode . abbrev-mode)
  (rst-mode . abbrev-mode))


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
  (auto-revert-check-vc-info t "Автоматически обновлять статусную строку при использовании VCS")
  (global-auto-revert-non-file-buffers t "Автообновление не только файловых буферов."))


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


;; 📦 COMP-RUN
;; Встроенный пакет для управления настройками компиляции
(use-package comp-run
  :custom
  (native-comp-async-report-warnings-errors 'silent "Проблемы нативной компиляции — не мои проблемы, не надо их показывать."))


;; 📦 COMPILE
(use-package compile
  :custom
  (compilation-read-command nil "Будем запрашивать команду только один раз.")
  (compilation-scroll-output 'first-error "Остановимся на первой ошибке."))


;; 📦 CONF-MODE
;; Встроенный пакет.
;; Основной режим для редактирования конфигурационных файлов INI/CONF
(use-package conf-mode
  :mode
  ("\\.env\\'"
   "\\.flake8\\'"
   "\\.pylintrc\\'"
   "\\inventory\\'"))


;; 📦 CUSTOM
;; Встроенный пакет для управления настройками кастомизации
(use-package custom
  :custom
  (custom-buffer-done-kill t "Закрывать буферы customize при выходе из них")
  (custom-safe-themes t "Все темы считаем безопасными"))


;; 📦 DELSEL
;; Встроенный пакет.
;; Используется для управления удалением выделенного текста.
(use-package delsel
  :config
  (delete-selection-mode t)) ;; Удалять выделенный фрагмент при вводе текста


;; 📦 DESKTOP
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
  (add-to-list 'delete-frame-functions 'desktop-save)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'image-mode)
  :hook
  (after-init . desktop-read)
  (kill-emacs . (lambda () (desktop-save user-emacs-directory t)))
  (server-done . (lambda () (desktop-save user-emacs-directory t))))


;; 📦 DIRED
;; Встроенный пакет для работы с файлами и каталогами.
(use-package dired
  :custom
  (dired-auto-revert-buffer t "Обновлять содержимое при повторном переходе в каталог")
  (dired-free-space nil "Информация о занятом и свободном месте в отдельной строке")
  ;; Без этой настройки при каждой смене каталога Dired будет создавать новый буфер
  (dired-kill-when-opening-new-dired-buffer t "Удалять буфер при переходе в другой каталог")
  ;; Дополнительные параметры вызова команды ls
  (dired-listing-switches "-l --human-readable --all --group-directories-first --dired")
  (dired-recursive-copies 'always "Всегда копировать каталоги рекурсивно.")
  (dired-recursive-deletes 'always "Не задавать лишних вопросов при удалении не-пустых каталогов")
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))


;; 📦 DISPLAY-LINE-NUMBERS-MODE
;; Встроенный пакет для показа номеров строк
(use-package display-line-numbers
  :hook
  ((c-mode
    conf-mode
    conf-toml-mode
    css-mode
    csv-mode
    dockerfile-ts-mode
    emacs-lisp-mode
    html-mode
    js-ts-mode
    json-ts-mode
    latex-mode
    lisp-data-mode
    makefile-mode
    markdown-mode
    mhtml-mode
    nxml-mode
    po-mode
    python-mode
    rst-mode
    ruby-mode
    ruby-ts-mode
    rust-mode
    sed-mode
    sh-mode
    tex-mode
    xml-mode
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
    python-mode
    rst-mode
    ruby-mode
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
  ((conf-mode
    css-mode
    emacs-lisp-data-mode
    emacs-lisp-mode
    html-mode
    js-ts-mode
    json-ts-mode
    lisp-data-mode
    markdown-mode
    mhtml-mode
    nxml-mode
    org-mode
    python-mode
    ruby-mode
    ruby-ts-mode
    rust-mode
    sed-mode
    tex-mode
    text-mode
    conf-toml-mode
    yaml-ts-mode) . electric-pair-local-mode))


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
  (confirm-kill-processes nil "Прерывать процессы без лишних вопросов.")
  (delete-old-versions t "Удалять старые резервные копии файлов без лишних вопросов")
  (enable-local-eval t "Разрешить вызов `eval' в `.dir-locals.el'")
  (enable-local-variables :all "Считать все переменные из файлов `.dir-locals.el' безопасными")
  (large-file-warning-threshold (* 100 1024 1024) "Предупреждение при открытии файлов больше 100 МБ (по умолчанию — 10 МБ)")
  (make-backup-files nil "Резервные копии не нужны, у нас есть VCS")
  (require-final-newline t "Требовать новую строку в конце файлов")
  (save-abbrevs 'silently "Сохранять аббревиатуры без лишних вопросов")
  :config
  (add-to-list 'safe-local-variable-values '(buffer-env-script-name . ".venv/bin/activate"))
  (add-to-list 'safe-local-variable-values '(electric-pair-preserve-balance . t))
  (add-to-list 'safe-local-variable-values '(emacs-lisp-docstring-fill-column . 80))
  (add-to-list 'safe-local-variable-values '(fill-column . 120))
  (add-to-list 'safe-local-variable-values '(fill-column . 80))
  (add-to-list 'safe-local-variable-values '(frozen_string_literal . true))
  (add-to-list 'safe-local-variable-values '(lexical-binding . t))
  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode))
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))


;; 📦 FILL-COLUMN
;; Встроенный пакет.
;; Отображение рекомендуемой границы символов.
(use-package display-fill-column-indicator
  :hook
  ((emacs-lisp-mode
    js-ts-mode
    yaml-ts-mode) . display-fill-column-indicator-mode))


;; 📦 FLYMAKE
;; Встроенный пакет для работы со статическими анализаторами.
(use-package flymake
  :pin "gnu"
  :ensure t
  :init
  (unless (alist-get 'flymake package-alist)
    (package-upgrade 'flymake))
  :bind (:map emacs-lisp-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :hook ((emacs-lisp-mode) . flymake-mode))


;; 📦 FLYSPELL-MODE
;; Встроенный пакет.
;; Проверка орфографии с помощью словарей.
;; Использовать пакет только в том случае, когда дело происходит в
;; Linux и Hunspell, Aspell и Nuspell доступны.
(when (string-equal system-type "gnu/linux")
  (use-package flyspell
    :custom
    ;; Выбираем желаемую утилиту для проверки орфографии
    (ispell-program-name (cond ((file-executable-p "/usr/bin/hunspell") "hunspell")
                               ((file-executable-p "/usr/bin/aspell") "aspell")
                               ((file-executable-p "/usr/bin/nuspell") "nuspell")
                               ;; Ничего не установлено?
                               (t nil)))
    :hook
    ((text-mode . flyspell-mode)
     (emacs-lisp-mode . flyspell-prog-mode))))


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
        ("M-O" . previous-window-any-frame)
        ("M-o" . next-window-any-frame)))


;; 📦 GOTO-ADDRESS-MODE
;; Встроенный пакет.
;; Подсвечивает ссылки и позволяет переходить по ним с помощью [C-c RET].
;; Возможны варианты (зависит от основного режима).
(use-package goto-addr
  :hook
  (asciidoc-mode . goto-address-mode)
  (emacs-lisp-mode . goto-address-mode)
  (html-mode . goto-address-mode)
  (markdown-mode . goto-address-mode)
  (rst-mode . goto-address-mode))


;; 📦 GREP
;; Встроенный пакет для поиска с помощью `grep'.
(use-package grep
  :config
  (add-to-list 'grep-find-ignored-directories "node_modules")
  :bind
  (:map global-map ("<f6>" . find-grep)))


;; 📦 HL-LINE-MODE
;; Подсветка активной строки.
(use-package hl-line
  :config
  (global-hl-line-mode t))


;; 📦 HTML-MODE
;; Встроенный пакет для работы с HTML и SGML.
(use-package html-mode
  :mode
  ("\\.hbs\\'"
   "\\.html\\'"
   "\\.jinja\\'"))


;; 📦 IBUFFER
;; Встроенный пакет для удобной работы с буферами.
;; По нажатию F2 выводит список открытых буферов.
(require 'ibuffer)
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
  (ibuffer-default-sorting-mode 'major-mode "Сортировать файлы по основному режиму")
  (ibuffer-display-summary nil "Не показывать строку ИТОГО")
  (ibuffer-eliding-string "…" "Если строка не уместилась, показать этот знак")
  (ibuffer-expert t "Не запрашивать подтверждение для опасных операций")
  (ibuffer-shrink-to-minimum-size t "Минимальный размер буфера по умолчанию")
  (ibuffer-truncate-lines nil "Не обкусывать длинные строки")
  (ibuffer-use-other-window t "Открывать буфер *Ibuffer* в отдельном окне")
  :init
  (defalias 'list-buffers 'ibuffer "Замена стандартной функции на ibuffer.")
  :bind
  (:map global-map ("<f2>" . ibuffer)))


;; 📦 IBUF-EXT
;; Встроенный пакет с дополнительными настройками `ibuffer'.
(require 'ibuf-ext)
(use-package ibuf-ext
  :custom
  (ibuffer-saved-filter-groups ;; Группы по умолчанию
   '(("default"
      ("Dired" (mode . dired-mode))
      ("Emacs Lisp" (or (mode . emacs-lisp-mode)
                        (mode . lisp-data-mode)))
      ("Org" (mode . org-mode))
      ("AsciiDoc" (mode . asciidoc-mode))
      ("Markdown" (mode . markdown-mode))
      ("ReStructured Text" (mode . rst-mode))
      ("CONF / INI" (or (mode . conf-mode)
                        (mode . editorconfig-conf-mode)))
      ("XML" (or (mode . nxml-mode)
                 (mode . xml-mode)))
      ("YAML" (mode . yaml-ts-mode))
      ("Makefile" (mode . makefile-mode))
      ("Python" (mode . python-mode))
      ("Ruby" (or (mode . ruby-mode)
                  (mode . ruby-ts-mode)))
      ("SSH keys" (name . "\\.pub\\'"))
      ("Shell-script" (mode . sh-mode))
      ("SQL" (mode . sql-mode))
      ("Web" (or (mode . html-mode)
                 (mode . json-mode)
                 (mode . json-ts-mode)
                 (mode . js-mode)
                 (mode . js-ts-mode)))
      ("Magit" (or (mode . magit-diff-mode)
                   (mode . magit-log-mode)
                   (mode . magit-status-mode)
                   (name . "\\*magit\\*")
                   (name . "git-monitor")))
      ("Commands" (or (mode . compilation-mode)
                      (mode . eshell-mode)
                      (mode . shell-mode)
                      (mode . term-mode)))
      ("Emacs" (or (name . "\\*scratch\\*")
                   (name . "\\*Messages\\*")
                   (name . "\\*Customize\\*")
                   (name . "\\*Help\\*")
                   (name . "\\*Echo\\*")
                   (name . "\\*Minibuf\\*"))))))
  (ibuffer-hidden-filter-groups (list "*Internal*" )) ;; Не показывать эти буферы
  (ibuffer-show-empty-filter-groups nil) ;; Не показывать пустые группы
  :hook
  (ibuffer-mode . ibuffer-auto-mode)
  (ibuffer-mode . (lambda ()(ibuffer-switch-to-saved-filter-groups "default"))))


;; 📦 IMENU
(use-package imenu
  :custom
  (imenu-auto-rescan t))


;; 📦 JS-MODE
;; Встроенный пакет для работы с JavaScript.
(use-package js
  :custom
  (js-chain-indent t "Выравнивание при цепочке вызовов через точку.")
  (js-indent-level 2 "Отступ в 2 пробела, а не 4 (по умолчанию).")
  (js-switch-indent-offset 2 "Отступ в 2 пробела для switch/case.")
  :mode
  ("\\.js\\'" . js-ts-mode)
  ("\\.jsm\\'" . js-ts-mode)
  ("\\.jsx\\'" . js-ts-mode)
  ("\\.har\\'" . js-ts-mode))


;; 📦 JSONRPC
;; https://elpa.gnu.org/packages/jsonrpc.html
;; Пакет для работы с JSON. Зависимость Eglot.
(use-package jsonrpc
  :pin "gnu"
  :init
  (unless (alist-get 'jsonrpc package-alist)
    (package-upgrade 'jsonrpc)))


;; 📦 JSON-TS-MODE
;; Встроенный пакет для работы с JSON через TreeSitter
(use-package json-ts-mode
  :mode "\\.json\\'")


;; 📦 MAKEFILE
;; Встроенный пакет для работы с `Makefile'.
(use-package make-mode
  :mode ("Makefile\\'" . makefile-mode))


;; ;; 📦 MINIBUFFER
;; ;; Встроенный пакет для управления поведением минибуфера.
;; (use-package minibuffer
;;   :custom
;;   (setq completions-detailed t "Подробные подсказки в минибуфере"))


;; 📦 nXML
;; Встроенный пакет для работы с диалектами XML
(use-package nxml-mode
  :custom
  (nxml-attribute-indent 4 "Выравнивание атрибутов")
  (nxml-auto-insert-xml-declaration-flag nil "Не вставлять декларацию")
  (nxml-bind-meta-tab-to-complete-flag t "Использовать TAB для завершения ввода")
  (nxml-child-indent 4 "Выравнивание дочерних элементов")
  (nxml-slash-auto-complete-flag t "Закрывать теги по вводу /")
  :mode
  ("\\.pom\\'" . nxml-mode)
  ("\\.xml\\'" . nxml-mode))


;; 📦 PAREN
;; Подсветка парных скобок.
(use-package paren
  :config
  (show-paren-mode t)) ;; Подсвечивать парные скобки


;; 📦 OUTLINE
;; Управление видимостью блоков кода
(use-package outline
  :hook
  (asciidoc-mode . outline-minor-mode)
  (emacs-lisp-mode . outline-minor-mode)
  (markdown-mode . outline-minor-mode)
  (rst-mode . outline-minor-mode))


;; 📦 PEG
;; Встроенный пакет, который мы просто обновим из GNU ELPA
(use-package peg
  :pin "gnu"
  :ensure t
  :init (unless (alist-get 'peg package-alist)
          (package-upgrade 'peg)))


;; 📦 PIXEL-SCROLL
;; Встроенный пакет, позволяет плавно прокручивать текст
(when (package-installed-p 'pixel-scroll)
  (use-package pixel-scroll
    :config
    (pixel-scroll-mode t)
    (pixel-scroll-precision-mode t)))


;; 📦 PROG-MODE
;; Встроенный пакет, предоставляющий предка для всех программистских режимов.
(use-package prog-mode
  :hook
  (emacs-lisp-mode . prettify-symbols-mode)) ;; Будем показывать глифы вместо некоторых конструкций


;; 📦 PROJECT
;; Управление проектами на самом базовом уровне.
;; [C-x p p] — переключение.
;; [C-x p D] — Dired.
;; [C-x p d] — выбрать каталог.
;; [C-x p f] — выбрать файл.
;; [C-x p k] — закрыть открытые файлы проекта.
;; [C-x p C-b] — показать список буферов проекта.
;; [C-x p b] — переключение буферов в рамках проекта.
;; [C-x p c] — компиляция проекта.
;; [C-x p s] — Shell в текущем проекте.
;; [C-x p e] — EShell в текущем проекте.
(use-package project
  :pin "gnu"
  :ensure t
  :init
  (unless (alist-get 'project package-alist)
    (package-upgrade 'project)))


;; 📦 PYTHON-MODE
;; Встроенный пакет для работы с Python через TreeSitter
(use-package python
  :pin "gnu"
  :init
  (unless (alist-get 'python package-alist)
    (package-upgrade 'python))
  :custom
  (py-pylint-command-args "--max-line-length 120" "Дополнительные параметры, передаваемые pylint")
  (python-indent-guess-indent-offset-verbose nil "Выключить уведомления")
  (python-indent-offset 4 "Отступ по умолчанию — 4 пробела"))


;; 📦 RECENTF-MODE
;; Встроенный пакет, позволяет просматривать и быстро переходить к последним
;; открытым файлам
(use-package recentf
  :custom
  (recentf-max-saved-items 100 "Помнить последние 100 файлов")
  (recentf-save-file (locate-user-emacs-file "recentf") "Хранить список в файле .emacs.d/recentf")
  :config
  (recentf-mode t))


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


;; 📦 RUBY-TS-MODE
;; Встроенный пакет для работы с Ruby.
(use-package ruby-ts-mode
  :mode
  ("\\.rb\\'" . ruby-ts-mode)
  ("\\Vagrantfile\\'" . ruby-ts-mode))


;; 📦 SAVEPLACE
;; Запоминание позиции курсора в посещённых файлах.
(use-package saveplace
  :custom
  (save-place-forget-unreadable-files t "Не запоминать положение в нечитаемых файлах.")
  :config
  (save-place-mode t))


;; 📦 SAVEHIST
;; Встроенный пакет для запоминания истории команд
(require 'savehist)
(use-package savehist
  :hook
  (server-done . savehist-save)
  (kill-emacs . savehist-save)
  :config
  (add-to-list 'delete-frame-functions 'savehist-save)
  (add-to-list 'savehist-additional-variables 'compile-history)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring)
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'shell-command-history)
  (savehist-mode t))


;; 📦 SHELL-SCRIPT-MODE
;; Встроенный пакет для работы со скриптами Shell.
;; Можно было бы использовать `bash-ts-mode', но нужна более новая версия
;; TreeSitter
(use-package sh-script
  :mode
  ("\\.bash_aliases\\'" . sh-mode)
  ("\\.bashrc\\'" . sh-mode)
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
  :init
  ;; Создадим каталог для файлов автосохранения
  (let ((saves-dir (expand-file-name "saves" user-emacs-directory)))
    (unless (file-directory-p saves-dir)
      (make-directory saves-dir)))
  :custom
  (backward-delete-char-untabify-method 'hungry "Удалять все символы выравнивания при нажатии [Backspace]")
  (blink-matching-paren t "Мигать, когда скобки парные")
  (indent-tabs-mode nil "Отключить `indent-tabs-mode'.")
  (kill-do-not-save-duplicates t "Не добавлять строку в kill-ring, если там уже есть такая же")
  (kill-whole-line t "Удалять логическую строку, а не видимую.")
  (next-line-add-newlines nil "Не добавлять пустую строку при прокрутке")
  (save-interprogram-paste-before-kill t "Сохранять данные в kill ring перед попаданием нового фрагмента")
  (size-indication-mode nil "Не показывать размера буфера в mode-line")
  (suggest-key-bindings t "Показывать подсказку клавиатурной комбинации для команды")
  :config
  (auto-save-mode t)
  (column-number-mode t) ;; Номер колонки в mode-line
  (keymap-global-unset "<insert>" t) ;; Режим перезаписи не нужен
  (line-number-mode t) ;; Номер строки в mode-line
  (put 'overwrite-mode 'disabled t) ;; Выключить `overwrite-mode'.
  :bind
  (:map global-map
        ("C-z" . undo)) ;; Отмена на Ctrl+Z
  :hook
  ((compilation-mode . visual-line-mode)
   (markdown-mode . visual-line-mode)
   (messages-buffer-mode . visual-line-mode)
   (text-mode . visual-line-mode)))


;; 📦 SORT
;; Встроенный пакет для сортировки всякого разного
(use-package sort
  :bind
  (:map global-map
        ("<f7>" . sort-lines)))


;; 📦 SVG
;; Встроенный пакет, который мы просто обновим из GNU ELPA
(use-package svg
  :pin "gnu"
  :ensure t
  :init (unless (alist-get 'svg package-alist)
          (package-upgrade 'svg)))


;; 📦 TEXINFO
;; Встроенный пакет для работы с файлами Texinfo
(use-package texinfo
  :mode
  ("\\.texi\\'" . texinfo-mode))


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


;; 📦 TRACK-CHANGES
;; Встроенный пакет, обновлять который будем из архива
(use-package track-changes
  :pin "gnu"
  :init
  (unless (alist-get 'track-changes package-alist)
    (package-upgrade 'track-changes)))


;; 📦 TRAMP
;; Встроенный пакет, который мы просто обновим из GNU ELPA
(use-package tramp
  :pin "gnu"
  :ensure t
  :init (unless (alist-get 'tramp package-alist)
          (package-upgrade 'tramp)))


;; 📦 TRANSIENT
;; Встроенный пакет, который нужно обновить, чтобы нормально работал MAGIT.
(use-package transient
  :pin "gnu"
  :init
  (unless (alist-get 'transient package-alist)
    (package-upgrade 'transient)))


;; 📦 UNIQUIFY
;; Встроенный пакет.
;; Используется для поддержания уникальности названий буферов, путей и т. д.
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward "Показывать каталог перед именем файла, если буферы одинаковые (по умолчанию имя<каталог>)")
  (uniquify-separator "/" "Разделять буферы с похожими именами, используя /"))


;; 📦 VERILOG-MODE
;; Просто обновим его из GNU ELPA
(use-package verilog-mode
  :pin "gnu"
  :init
  (unless (alist-get 'verilog-mode package-alist)
    (package-upgrade 'verilog-mode)))


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
  (whitespace-line-column nil "Используем значение fill-column")
  :hook
  ((asciidoc-mode
    conf-mode
    css-mode
    dockerfile-ts-mode
    emacs-lisp-mode
    html-mode
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
    python-mode
    rst-mode
    ruby-mode
    ruby-ts-mode
    rust-mode
    rust-ts-mode
    sass-mode
    sed-mode
    sh-mode
    snippet-mode ;; Yasnippet
    sql-mode
    tex-mode
    texinfo-mode
    yaml-ts-mode) . whitespace-mode))


;; 📦 WINDOW
;; Встроенный пакет, отвечает за управление размерами окон
(use-package window
  :custom
  (window-resize-pixelwise t "Делить окна по пикселям, а не по символам.")
  :bind
  (:map global-map
        ("C-S-<iso-lefttab>" . next-buffer) ;; [Ctrl+Tab]       Вернуться в предыдущий буфер
        ("C-<tab>" . previous-buffer)))     ;; [Ctrl+Shift+Tab] Следующий буфер


;; 📦 WINDOW-TOOL-BAR
;; Просто обновим из GNU ELPA
(use-package window-tool-bar
  :pin "gnu"
  :init
  (unless (alist-get 'window-tool-bar package-alist)
    (package-upgrade 'window-tool-bar)))



;; 📦 XREF
;; Встроенный пакет, который просто обновим из GNU ELPA
(use-package xref
  :pin "gnu"
  :ensure t
  :init (unless (alist-get 'xref package-alist)
          (package-upgrade 'xref)))


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

;; 📦 APHELEIA
;; https://github.com/radian-software/apheleia
;; Автоформат буфера перед сохранением.
(use-package apheleia
  :vc
  (:url "https://github.com/radian-software/apheleia.git"
        :rev "v4.4.2")
  :custom
  (apheleia-mode-lighter " ɑ" "Вместо длинного Apheleia")
  :bind (:map global-map
              ("<f12>" . apheleia-format-buffer))
  :hook
  (emacs-lisp-mode . apheleia-mode)
  (python-mode . apheleia-mode)
  (python-ts-mode . apheleia-mode)
  (ruby-mode . apheleia-mode)
  (ruby-ts-mode . apheleia-mode))


;; 📦 AUCTEX
;; IDE для работы с TeX
;; https://www.gnu.org/software/auctex/index.html
(use-package auctex
  :pin "gnu"
  :ensure t)


;; 📦 ALL
;; https://elpa.gnu.org/packages/all.html
;; Это аналог `occur', только все найденные строки помещаются в отдельный буфер,
;; где их можно отредактировать, не прыгая по всему буферу. После изменений
;; достаточно нажать C-c C-c, и изменения отразятся в основном буфере
(use-package all
  :ensure t
  :pin "gnu")


;; 📦 ASCIIDOC
(let ((asciidoc-repo-dir (format "/home/%s/repo/asciidoc-mode/" user-login-name)))
  (when (file-exists-p asciidoc-repo-dir)
    (add-to-list 'load-path asciidoc-repo-dir)
    (require 'asciidoc-mode)))


;; 📦 AVY
;; https://github.com/abo-abo/avy
;; Множество функций для быстрого перехода к нужной строке, слову, символу и
;; так далее.
(use-package avy
  :pin "gnu"
  :ensure t
  :bind
  (:map global-map
        ("M-g f" . avy-goto-line)
        ("M-g w" . avy-goto-word-0)
        ("C-'" . avy-goto-char)))


;; 📦 BIND-KEY
;; Часть `use-package', но мы будем обновлять его из архивов.
(use-package bind-key
  :pin "gnu"
  :ensure t
  :init (unless (alist-get 'bind-key package-alist)
          (package-upgrade 'bind-key)))


;; 📦 BREADCRUMB
;; https://elpa.gnu.org/packages/breadcrumb.html
;; Вывод пути к файлу в верхней части окна
(use-package breadcrumb
  :pin "gnu"
  :ensure t
  :config
  (when (fboundp 'breadcrumb-mode)
    (breadcrumb-mode t)))


;; 📦 BUFFER-ENV
;; https://github.com/astoff/buffer-env
;; Переменные окружения для отдельного буфера. Почти ENVRC, только от GNU
(use-package buffer-env
  :pin "gnu"
  :ensure t
  :config
  (when (fboundp 'buffer-env-update)
    (add-hook 'hack-local-variables-hook #'buffer-env-update)
    (add-hook 'comint-mode-hook #'buffer-env-update)))


;; 📦 COLORFUL-MODE
;; https://github.com/DevelopmentCool2449/colorful-mode
;; Отображение цветов прямо в буфере. Наследник `raibow-mode.el'.
(use-package colorful-mode
  :pin "gnu"
  :ensure t
  :hook
  ((css-mode
    emacs-lisp-mode
    html-mode
    json-ts-mode
    yaml-ts-mode) . colorful-mode))


;; 📦 CORFU
;; https://elpa.gnu.org/packages/corfu.html
;; Расширение для автодополнения в буфере.
(use-package corfu
  :pin "gnu"
  :ensure t
  :config
  (when (fboundp 'global-corfu-mode)
    (global-corfu-mode t)))


;; 📦 COUNSEL
;; https://elpa.gnu.org/packages/counsel.html
;; Замена встроенных команд на их более удобные аналоги.
(use-package counsel
  :pin "gnu"
  :ensure t
  :bind
  (:map global-map
        ("C-c c" . #'counsel-compile)
        ("C-c g" . #'counsel-git)
        ("C-c j" . #'counsel-file-jump)
        ("C-h S" . #'counsel-info-lookup-symbol)
        ("C-h a" . #'counsel-apropos)
        ("C-h b" . #'counsel-descbinds)
        ("C-h f" . #'counsel-describe-function)
        ("C-h l" . #'counsel-find-library)
        ("C-h o" . #'counsel-describe-symbol)
        ("C-h v" . #'counsel-describe-variable)
        ("C-x 8 RET" . #'counsel-unicode-char)
        ("C-x C-f" . #'counsel-find-file)
        ("C-x r b" . #'counsel-bookmark)
        ("M-g i" . #'counsel-imenu)
        ("M-x" . #'counsel-M-x)
        ("M-y" . #'counsel-yank-pop))
  :custom
  (add-to-list 'savehist-additional-variables 'counsel-unicode-char-history))


;; 📦 CSV-MODE
;; https://elpa.gnu.org/packages/csv-mode.html
;; Поддержка CSV
(use-package csv-mode
  :pin "gnu"
  :ensure t
  :mode ("\\.csv\\'" . csv-mode)
  :hook
  (csv-mode . csv-guess-set-separator)) ;; Попытаемся угадать `csv-separator' на основе содержимого файла.


;; 📦 DENOTE
;; https://protesilaos.com/emacs/denote
;; Режим для управления заметками
(use-package denote
  :pin "gnu"
  :ensure t
  :custom
  (denote-directory (expand-file-name "~/Notes/") "Каталог для хранения заметок."))


;; 📦 DIFF-HL
;; https://github.com/dgutov/diff-hl
;; Показывает небольшие маркеры рядом с незафиксированными изменениями.
;; Дополняет функциональность git-gutter, который показывает изменения только в
;; обычных буферах. Этот пакет умеет работать с dired и другими режимами.
(use-package diff-hl
  :pin "gnu"
  :ensure t
  :config
  (progn
    (when (fboundp 'global-diff-hl-mode)
      (global-diff-hl-mode t))
    (when (fboundp 'diff-hl-dired-mode)
      (add-hook 'dired-mode-hook 'diff-hl-dired-mode))))


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
  :pin "nongnu"
  :ensure t
  :bind (:map global-map
              ("C-c '" . edit-indirect-region)))


;; 📦 EDITORCONFIG
;; Поддержка https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :pin "gnu"
  :ensure t
  :init (unless (alist-get 'editorconfig package-alist)
          (package-upgrade 'editorconfig))
  :config
  (editorconfig-mode t))


;; 📦 EF-THEMES
;; https://github.com/protesilaos/ef-themes.git
(use-package ef-themes
  :pin "gnu"
  :ensure t)


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
;; - ReST:       pip3 install esbonio
;;               Создать в корне проекта файл .dir-locals.el и задать значение
;;               переменной `eglot-workspace-configuration'.
;; - Ruby:       sudo gem install ruby-lsp
;; - YAML:       sudo npm -g install yaml-language-server
(use-package eglot
  :pin "gnu"
  :ensure t
  :init
  (unless (alist-get 'eglot package-alist)
    (package-upgrade 'eglot))
  :defer t
  :custom
  (eglot-autoshutdown t "Автоматически выключить сервер при закрытии последнего буфера")
  (eglot-events-buffer-config '(
                                :size 0 ;; Выключить ведение буфера событий
                                :format 'lisp ;; Формат Lisp для логов
                                )
                              "Настройки буфера событий Eglot")
  :config
  (add-to-list 'eglot-server-programs '(ansible-mode . ("ansible-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(dockerfile-ts-mode . ("docker-langserver" "--stdio")))
  ;; (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("jedi-language-server")))
  (add-to-list 'eglot-server-programs '(ruby-mode . ("bundle" "exec" "ruby-lsp")))
  (add-to-list 'eglot-server-programs '(ruby-ts-mode . ("bundle" "exec" "ruby-lsp")))
  (add-to-list 'eglot-server-programs '(yaml-ts-mode . ("yaml-language-server" "--stdio")))
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . eldoc)
        ("C-c C-r" . eglot-rename)
        ("C-c C-f" . eglot-format-buffer))
  :hook
  (ansible-mode . eglot-ensure)
  (dockerfile-ts-mode . eglot-ensure)
  ;; (markdown-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (ruby-mode . eglot-ensure)
  (ruby-ts-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure))


;; 📦 ELDOC-MODE
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Programming-Language-Doc.html
;; Отображение подсказок при работе с Emacs Lisp
(use-package eldoc
  :pin "gnu"
  :ensure t
  :init (unless (alist-get 'eldoc package-alist)
          (package-upgrade 'eldoc))
  :config
  (global-eldoc-mode nil)
  :custom
  (eldoc-minor-mode-string nil "Не надо показывать ничего в строке статуса.")
  :hook
  ((emacs-lisp-mode
    lisp-interaction-mode) . eldoc-mode))


;; 📦 FLYCHECK
;; https://www.flycheck.org/
;; Проверка синтаксиса на лету с помощью статических анализаторов
(use-package flycheck
  :pin "nongnu"
  :ensure t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save new-line))
  (flycheck-highlighting-mode 'lines "Стиль отображения проблемных мест — вся строка")
  (flycheck-indication-mode 'left-fringe "Место размещения маркера ошибки — левая граница")
  (flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc" "Файл настроек Markdownlint")
  (flycheck-sphinx-warn-on-missing-references t "Предупреждать о некорректных ссылках в Sphinx")
  (flycheck-textlint-config ".textlintrc.yaml" "Файл настроек Textlint")
  :hook
  ((conf-mode
    css-mode
    dockerfile-ts-mode
    emacs-lisp-mode
    html-mode
    js-ts-mode
    json-ts-mode
    latex-mode
    lisp-data-mode
    makefile-mode
    nxml-mode
    python-mode
    rst-mode
    ruby-mode
    ruby-ts-mode
    rust-mode
    sh-mode
    sql-mode
    yaml-ts-mode
    ) . flycheck-mode))


;; 📦 INDENT-BARS
;; https://github.com/jdtsmith/indent-bars
;; Красивая подсветка отступов
(use-package indent-bars
  :pin "gnu"
  :ensure t
  :hook
  (css-mode . indent-bars-mode)
  (css-ts-mode . indent-bars-mode)
  (emacs-lisp-mode . indent-bars-mode)
  (javascript-mode . indent-bars-mode)
  (js-ts-mode . indent-bars-mode)
  (makefile-mode . indent-bars-mode)
  (markdown-mode . indent-bars-mode)
  (python-mode . indent-bars-mode)
  (rst-mode . indent-bars-mode)
  (ruby-mode . indent-bars-mode)
  (ruby-ts-mode . indent-bars-mode)
  (rust-mode . indent-bars-mode)
  (rust-ts-mode . indent-bars-mode)
  (sass-mode . indent-bars-mode)
  (yaml-ts-mode . indent-bars-mode))


;; 📦 IVY
;; https://elpa.gnu.org/packages/ivy.html
;; https://elpa.gnu.org/packages/doc/ivy.html
;; Функции фильтрации и выбора элементов. Как Helm, но теперь в GNU ELPA.
;; При переименовании файлов рекомендуется использовать `ivy-immediate-done',
;; это последовательность [C-M-j].
(use-package ivy
  :pin "gnu"
  :ensure t
  :demand t
  :config
  (ivy-mode t)
  :bind
  (:map global-map
        ("C-x b" . ivy-switch-buffer)))


;; 📦 JINJA2-MODE
;; https://elpa.nongnu.org/nongnu/jinja2-mode.html
;; Режим для работы с шаблонами Jinja2
(use-package jinja2-mode
  :pin "nongnu"
  :ensure t
  :mode ("\\.j2" . jinja2-mode))


;; 📦 JINX
;; https://github.com/minad/jinx
;; Проверка орфографии не только для слова под курсором, как во `flyspell',
;; а вообще во всём буфере.
;; В Debian требует для работы пакеты `libenchant2-dev' и `pkgconf'.
(use-package jinx
  :pin "gnu"
  :ensure t
  :custom
  (jinx-languages "ru_RU en_US")
  :hook
  (asciidoc-mode . jinx-mode)
  (emacs-lisp-mode . jinx-mode)
  (markdown-mode . jinx-mode)
  (rst-mode . jinx-mode)
  :bind
  (:map global-map
        ("M-$" . jinx-correct)
        ("C-M-$" . jinx-languages)
        ("M-n" . jinx-next)
        ("M-p" . jinx-previous)))


;; 📦 LIN
;; https://github.com/protesilaos/lin
;; Более умная подсветка активной строки, чем `hl-line-mode'.
(message "Загрузка lin-mode")
(use-package lin
  :pin "gnu"
  :ensure t
  :hook
  (asciidoc-mode . lin-mode)
  (emacs-lisp-mode . lin-mode)
  (ibuffer-mode . lin-mode)
  (markdown-mode . lin-mode)
  (rst-mode . lin-mode)
  (ruby-mode . lin-mode)
  (ruby-ts-mode . lin-mode)
  (rust-mode . lin-mode)
  (yaml-ts-mode . lin-mode))


;; 📦 MAGIT
;; https://magit.vc/
;; Magic + Git + Diff-HL.
;; Лучшее средство для работы с Git.
(use-package magit
  :pin "nongnu"
  :ensure t
  :custom
  (magit-define-global-key-bindings 'default "Включить глобальные сочетания Magit.")
  :hook
  (after-save . magit-after-save-refresh-buffers)
  (after-save . magit-after-save-refresh-status)
  :config
  (when (fboundp 'magit-auto-revert-mode)
    (magit-auto-revert-mode t)))


;; 📦 MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
;; Режим для работы с файлами в формате Markdown
(use-package markdown-mode
  :pin "nongnu"
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t "Подсвечивать синтаксис в примерах кода")
  (markdown-header-scaling-values '(1.0 1.0 1.0 1.0 1.0 1.0) "Все заголовки одной высоты")
  (markdown-list-indent-width 4 "Размер отступа для выравнивания вложенных списков")
  :config (setq-local word-wrap t)
  :bind (:map markdown-mode-map
              ("M-." . markdown-follow-thing-at-point)))


;; 📦 MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
;; Позволяет использовать мультикурсорность.
(use-package multiple-cursors
  :pin "nongnu"
  :ensure t
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


;; 📦 ORG-MODE
;; https://orgmode.org/
;; Органайзер, заметки и так далее
(use-package org
  :pin "gnu"
  :init (unless (alist-get 'org package-alist)
          (package-upgrade 'org))
  :custom
  (org-agenda-files '("~/Notes/"))
  :defer t
  :config
  (setq-local
   truncate-lines nil ;; Не обрезать строки
   word-wrap t))      ;; Перенос длинных строк


;; 📦 PO-MODE
;; https://elpa.gnu.org/packages/po-mode.html
;; Пакет для работы с файлами переводов в формате `.po'.
(use-package po-mode
  :pin "gnu"
  :ensure t
  :mode ("\\.po\\'" . po-mode))


;; 📦 PULSAR
;; Вспыхивание строки, к которой переместился курсор
;; https://github.com/protesilaos/pulsar
;; Этот пакет требует Emacs 27.1 или новее
(use-package pulsar
  :pin "gnu"
  :ensure t
  :custom
  (pulsar-pulse t)
  (ring-bell-function 'pulsar-pulse-line "Вместо звонка подсветить строку")
  :hook
  (after-init . pulsar-global-mode)
  (next-error . pulsar-pulse-line)
  :config
  (add-to-list 'pulsar-pulse-functions 'flycheck-next-error)
  (add-to-list 'pulsar-pulse-functions 'flyspell-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'recenter-top-bottom))


;; 📦 RAINBOW-DELIMITERS
;; https://github.com/Fanael/rainbow-delimiters
;; Подсветка парных скобок одним цветом
(use-package rainbow-delimiters
  :pin "nongnu"
  :ensure t
  :hook
  (asciidoc-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-data-mode . rainbow-delimiters-mode)
  (markdown-mode . rainbow-delimiters-mode))


;; 📦 RUSSIAN-TECHWRITER
;; Метод ввода для технических писателей.
;; В отличие от russian-computer, позволяет использовать лигатуры.
;; https://github.com/dunmaksim/emacs-russian-techwriter-input-method
(use-package russian-techwriter
  :vc (:url "https://github.com/dunmaksim/emacs-russian-techwriter-input-method.git"
            :rev :newest)
  :custom
  (default-input-method "russian-techwriter" "Метод ввода по умолчанию.")
  (default-transient-input-method "russian-techwriter" "Временный метод ввода"))


;; 📦 RUST-MODE
;; https://github.com/rust-lang/rust-mode
;; Поддержка языка Rust
(use-package rust-mode
  :pin "nongnu"                         ; Comment
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))


;; 📦 SASS-MODE
;; https://github.com/nex3/haml/tree/master
;; Таблицы стилей, созданные с помощью SASS
(use-package sass-mode
  :pin "nongnu"
  :ensure t
  :mode ("\\.sass\\'" . sass-mode))


;; 📦 SED-MODE
;; Режим для работы с файлами команд `sed'.
(use-package sed-mode                   ;
  :pin "gnu"
  :ensure t
  :mode ("\\.sed\\'" . sed-mode))


;; 📦 SWIPER
;; https://elpa.gnu.org/packages/swiper.html
;; Умный поиск и отличная (в некоторых случаях) замена `isearch-forward' и
;; `isearch-backward'.
(use-package swiper
  :pin "gnu"
  :ensure t
  :bind
  (:map global-map
        ("C-s" . 'swiper-isearch)
        ("C-r" . 'swiper-isearch-backward))
  :config
  (add-to-list 'savehist-additional-variables 'swiper-history))


;; 📦 TYPST-TS-MODE
;; https://codeberg.org/meow_king/typst-ts-mode/
;; Поддержка формата Typst с помощью TreeSitter
(use-package typst-ts-mode
  :pin "nongnu"
  :ensure t
  :mode
  ("\\.typ\\'" . typst-ts-mode))


;; VUNDO
;; https://github.com/casouri/vundo
;; Визуализация отмен.
;; C-z -- запуск.
;; f -- вперёд
;; b -- назад
;; n -- другая ветка
;; p -- предыдущая ветка
;; a -- назад к ближайшей развилке
;; w -- вперёд к ближайшей развилке
;; e -- в конец ветки
;; l -- к последнему сохранённому узлу
;; r -- к следующему сохранённому узлу
;; m -- выбрать узел для просмотра diff
;; u -- снять выделение
;; d -- показать diff между текущим и отмеченным (или родительским) узлом
;; q -- закрыть
;; C-c C-s -- сохранить изменения
(use-package vundo
  :pin "gnu"
  :ensure t
  :init
  (keymap-global-unset "C-z")
  :bind (:map global-map
              ("C-z" . vundo)))


;; 📦 WHICH-KEY MODE
;; https://elpa.gnu.org/packages/which-key.html
;; Показывает подсказки к сочетаниям клавиш.
(use-package which-key
  :pin "gnu"
  :ensure t
  :init
  (unless (alist-get 'which-key package-alist)
    (package-upgrade 'which-key))
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

(load-theme 'ef-maris-dark t)

(provide 'init.el)
;;; init.el ends here
