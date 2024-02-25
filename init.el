;;; init.el --- Dunaevsky Maxim GNU EMACS config -*- lexical-binding:t -*-
;;; Commentary:
;;; Настройки EMACS для работы техническим писателем.

;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p) ;; Использовать y и n вместо yes и no (сокращает объём вводимого текста для подтверждения команд)

(defconst init-emacs-config-dir (file-name-directory user-init-file) "Корневой каталог для размещения настроек.") ;; ~/.emacs.d/
(defconst init-emacs-autosave-dir (concat init-emacs-config-dir "saves") "Каталог для файлов автосохранения.") ;; ~/.emacs.d/saves/
(defconst init-emacs-package-user-dir (expand-file-name "elpa" user-emacs-directory) "Пользовательский каталог с пакетами.") ;; ~/.emacs.d/elpa/
(defconst init-emacs-version-greater-than-26-1
  (or
   (> emacs-major-version 26)
   (and (= emacs-major-version 26)
        (>= emacs-minor-version 1))) "Версия Emacs ≥ 26.1.")
(defconst init-emacs-version-greater-than-27-1
  (or
   (> emacs-major-version 27)
   (and (= emacs-major-version 27)
        (>= emacs-minor-version 1))) "Версия Emacs ≥ 27.1.")

;; Если нужного каталога не существует, его следует создать
(dolist
    (emacs-directory
     (list
      init-emacs-config-dir
      init-emacs-autosave-dir
      init-emacs-package-user-dir))
  (unless (file-directory-p emacs-directory)
    (make-directory emacs-directory)
    (message (format "Создана директория %s" emacs-directory))))

(defconst emacs-default-font-height 16 "Размер шрифта по умолчанию.")

;; Если используется старая версия EMACS, нужно указать параметры протокола TLS.
;; В противном случае будут проблемы при загрузке архива пакетов.
(when (< emacs-major-version 27)
  (require 'gnutls)
  (setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; -> PACKAGE
;; Встроенный пакет для управления пакетами.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(setq
 package-archive-priorities
 '(
   ;; Порядок использования архивов.
   ;; Чем выше приоритет, тем важнее архив
   ("gnu" . 50)
   ("nongnu" . 40)
   ("melpa-stable" . 30)
   ("melpa" . 20))
 package-native-compile t                      ;; Компиляция пакетов во время установки, а не при первом запуске
 package-user-dir init-emacs-package-user-dir) ;; Хранить все пакеты в каталоге ~/.emacs.d/elpa/

(add-to-list 'package-pinned-packages '("use-package" . "gnu"))

;; Проверка наличия индекса пакетов
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

(require 'use-package)

;; Настройки отладочного режима
(when init-file-debug
  (setq use-package-verbose t
        use-package-compute-statistics t
        use-package-expand-minimally t
        debug-on-error t))


;; -> Настройки, специфичные для графического режима
(defun setup-gui-settings (frame-name)
  "Настройки, необходимые при запуске EMACS в графической среде.

  FRAME-NAME — имя фрейма, который настраивается."
  (when (display-graphic-p frame-name)
    (global-font-lock-mode t)  ;; Отображать шрифты красиво, используя Font Face's

    (defvar availiable-fonts (font-family-list)) ;; Какие есть семейства шрифтов?
    (defvar default-font-family nil "Шрифт по умолчанию.")

    ;; Перебор шрифтов
    (cond
     ((member "Fire Code Nerd" availiable-fonts)(setq default-font-family "Fira Code Nerd"))
     ((member "Fira Code" availiable-fonts) (setq default-font-family "Fira Code"))
     ((member "DejaVu Sans Mono Nerd" availiable-fonts) (setq default-font-family "DejaVu Sans Mono Nerd"))
     ((member "DejaVu Sans Mono" availiable-fonts) (setq default-font-family "DejaVu Sans Mono"))
     ((member "Source Code Pro" availiable-fonts) (setq default-font-family "Source Code Pro"))
     ((member "Consolas" availiable-fonts) (setq default-font-family "Consolas")))

    (when default-font-family
      ;; Это формат  X Logical Font Description Conventions, XLFD
      ;; https://www.x.org/releases/X11R7.7/doc/xorg-docs/xlfd/xlfd.html
      (set-frame-font (format "-*-%s-normal-normal-normal-*-%d-*-*-*-m-0-iso10646-1" default-font-family emacs-default-font-height) nil t)
      (set-face-attribute 'default nil :family default-font-family))

    (set-face-attribute 'default nil :height (* emacs-default-font-height 10))))

;; Правильный способ определить, что EMACS запущен в графическом режиме. Подробнее здесь:
;; https://emacsredux.com/blog/2022/06/03/detecting-whether-emacs-is-running-in-terminal-or-gui-mode/
(add-to-list 'after-make-frame-functions #'setup-gui-settings)


;; -> ABBREV-MODE
;; Использование аббревиатур -- фрагментов текста, которые при вводе определённой
;; последовательности символов заменяются на другую, например:
;; tf → Terraform
;; yc → Yandex Cloud
;; Это встроенный пакет
(use-package abbrev
  :ensure nil
  :defer t
  :diminish nil
  :custom (abbrev-mode t "Включить поддержку аббревиатур глобально"))


;; -> ACE-WINDOW
;; https://github.com/abo-abo/ace-window
;; Быстрое переключение между окнами по Alt+O
;; От этого пакета зависит `treemacs'.
(use-package ace-window
  :pin "gnu"
  :ensure t
  :bind ("M-o" . ace-window))


;; -> ADOC-MODE
;; https://github.com/bbatsov/adoc-mode
;; Работа с AsciiDoc
(use-package adoc-mode
  :pin "melpa"
  :ensure t
  :defer t
  :custom
  (adoc-fontify-code-blocks-natively 10000)
  :mode "\\.adoc\\'")


;; -> AGGRESSIVE-INDENT
;; Принудительное выравнивание кода
(use-package aggressive-indent
  :pin "gnu"
  :ensure t
  :defer t
  :hook
  ((
    emacs-lisp-mode
    js2-mode
    json-mode
    latex-mode
    lisp-data-mode
    nxml-mode
    sh-mode
    sql-mode
    ) . aggressive-indent-mode))


;; -> ANSIBLE
;; https://github.com/k1LoW/emacs-ansible
;; Дополнительные возможности при работе с YAML-файлами Ansible
(use-package ansible
  :pin "melpa-stable"
  :ensure t
  :defer t)


;; -> AUTO-REVERT
;; Встроенный пакет
;; Автоматически обновляет содержимое буфера при изменениях в файловой системе
(use-package auto-revert
  :hook
  ((
    dired-mode
    ibuffer-mode
    treemacs-mode
    ) . auto-revert-mode))


;; -> BIND-KEY
;; https://github.com/jwiegley/use-package
;; Позволяет настраивать привязки клавиш более простым и наглядным способом чем
;; тот, что предоставляет Emacs
(use-package bind-key
  :ensure t
  :pin "gnu")


;; -> BREADCRUMB
;; https://elpa.gnu.org/packages/breadcrumb.html
;; Хлебные крошки в верхней части буфера показывают путь к файлу или разделу
(use-package breadcrumb
  :ensure t
  :pin "gnu"
  :config
  (breadcrumb-mode 1))


;; -> BUFFER-ENV
;; https://github.com/astoff/buffer-env
;; Настройка окружения отдельно для каждого буфера.
;; Настройки загружаются из файла `.env' в каталоге проекта или `.dir-locals.el'.
;; Во первом случае в файле должна быть указана команда для активации окружения, например:
;; source .venv/bin/activate
;; Во втором достаточно задать значение переменной `buffer-env-script-name'.
(use-package buffer-env
  :ensure t
  :pin "gnu"
  :defer
  :hook
  ((hack-local-variables
    comint-mode) . buffer-env-update))


;; -> CHECKDOC
;; Встроенный пакет для проверки строк документации.
(use-package checkdoc
  :hook (emacs-lisp-mode . checkdoc-minor-mode))


;; -> COMPANY-MODE
;; https://company-mode.github.io/
;; Автодополнение
(use-package company
  :pin "gnu"
  :ensure t
  :defer t
  :custom
  (company-idle-delay 0.5 "Задержка вывода подсказки — полсекунды")
  (company-minimum-prefix-length 2 "Минимум 2 знака, чтобы company начала работать")
  (company-show-quick-access t "Показывать номера возле потенциальных кандидатов")
  (company-tooltip-align-annotations t "Выровнять текст подсказки по правому краю")
  (company-tooltip-limit 15 "Ограничение на число подсказок")
  :hook
  ((
    css-mode
    dockerfile-mode
    emacs-lisp-mode
    js2-mode
    latex-mode
    lisp-data-mode
    minibufer-mode
    nxml-mode
    org-mode
    rst-mode
    ruby-mode
    ) . company-mode)
  :bind
  (:map company-active-map
        ("TAB" . company-complete-common-or-cycle)
        ("M-/" . company-complete)
        ("M-." . company-show-location)))


;; -> CONF-MODE
;; Встроенный пакет. Основной режим для редактирования конфигурационных файлов INI/CONF
(use-package conf-mode
  :ensure nil
  :defer t
  :mode
  ("\\.editorconfig\\'"
   "\\.env\\'"
   "\\.flake8\\'"
   "\\.ini\\'"
   "\\.pylintrc\\'"))


;; -> CSS-MODE
;; Встроенный режим
(use-package css-mode
  :ensure nil
  :defer t
  :custom
  (css-indent-offset 2)
  :mode "\\.css\\'")


;; -> CUS-EDIT
;; Управление custom-файлами
;; Это встроенный пакет.
(use-package cus-edit
  :ensure nil
  :custom
  (custom-file
   (expand-file-name
    (convert-standard-filename "custom.el")
    init-emacs-config-dir)
   "Файл для сохранения пользовательских настроек, сделанных в customize."))


;; -> DASHBOARD
;; https://github.com/emacs-dashboard/emacs-dashboard
;; Отображает дашборд при запуске EMACS
(use-package dashboard
  :pin "melpa-stable"
  :ensure t
  :custom
  (dashboard-display-icons-p t "Включить отображение иконок")
  (dashboard-icon-type 'nerd-icons "Использовать иконки из пакета `nerd-icons'")
  (dashboard-items             ;; Элементы дашборда
   '(
     (recents . 15)           ;; Последние открытые файлы
     (bookmarks . 10)         ;; Последние закладки
     (projects . 10)          ;; Последние проекты
     (agenda . 10)            ;; Агенда
     (registers . 0)))        ;; Регистры
  (dashboard-set-footer nil "Скрыть \"весёлые\" надписи в нижней части дашборда")
  (dashboard-set-file-icons t "Показывать иконки рядом с элементами списков")
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda ()(get-buffer "*dashboard*")))) ;; Теперь Dashboard будет буфером по умолчанию после запуска


;; -> DESKTOP-SAVE-MODE
;; Встроенный пакет, позволяет сохранять состояние EMACS между сессиями
(use-package desktop
  :ensure nil
  :custom
  (desktop-load-locked-desktop t "Загрузка файла .desktop даже если он заблокирован")
  (desktop-modes-not-to-save '(dired-mode Info-mode info-lookup-mode)) ; А вот эти не сохранять
  (desktop-save t "Сохранять список открытых буферов, файлов и т. д. без лишних вопросов")
  :config
  (desktop-save-mode 1)
  (add-hook 'server-after-make-frame-hook #'desktop-read))


;; -> DIFF-HL
;; https://github.com/dgutov/diff-hl
;; Показывает небольшие маркеры рядом с незафиксированными изменениями. Дополняет функциональность git-gutter,
;; которые показывает изменения только в обычных буферах. Этот пакет умеет работать с dired и другими режимами.
(use-package diff-hl
  :pin "gnu"
  :ensure t
  :commands (diff-hl-mode diff-hl-dired-mode)
  :config (global-diff-hl-mode 1))


;; -> DIMINISH
;; https://github.com/myrjola/diminish.el
;; Позволяет изменить или вовсе скрыть название дополнительного режима с панели статуса.
;; Поддерживается `use-package' с помощью ключевого слова `diminish'.
(use-package diminish
  :pin "gnu"
  :ensure t)


;; -> DIRED
;; Встроенный пакет для работы с файлами и каталогами.
;; Клавиши:
;; [+] - создание каталога.
;; [C-x C-f] - создание файла с последующим открытием буфера.
(use-package dired
  :custom
  (dired-listing-switches "-lah --group-directories-first"))


;; -> DISPLAY-LINE-NUMBERS-MODE
;; Встроенный пакет
;; Показывает номера строк
(use-package display-line-numbers
  :hook
  ((
    adoc-mode
    conf-mode
    dockerfile-mode
    emacs-lisp-mode
    json-mode
    latex-mode
    lisp-data-mode
    makefile-mode
    markdown-mode
    nxml-mode
    python-mode
    rst-mode
    ruby-mode
    sh-mode
    shell-script-mode
    terraform-mode
    web-mode
    yaml-mode
    ) . display-line-numbers-mode))


;; -> DOCKERFILE-MODE
;; https://github.com/spotify/dockerfile-mode
;; Работа с файлами `Dockerfile'.
(use-package dockerfile-mode
  :ensure t
  :defer t
  :pin "nongnu")


;; -> DOOM-MODELINE
;; https://github.com/seagle0128/doom-modeline
;; Красивая статусная строка
(use-package doom-modeline
  :ensure t
  :pin "melpa-stable"
  :hook (after-init . doom-modeline-mode)
  :requires (nerd-icons)
  :custom
  (doom-modeline-buffer-encoding t "Отображение кодировки.")
  (doom-modeline-buffer-name t "Отображение названия буфера.")
  (doom-modeline-buffer-state-icon t "Отображение иконки со статусом буфера.")
  (doom-modeline-env-version t "Отображение версии окружения.")
  (doom-modeline-highlight-modified-buffer-name t "Подсветка названия измененного буфера.")
  (doom-modeline-icon t "Отображение иконок.")
  (doom-modeline-indent-info t "Отображение информации об отступах.")
  (doom-modeline-lsp t "Отображение статуса LSP-сервера.")
  (doom-modeline-lsp-icon t "Отображение иконки со статусом LSP-сервера.")
  (doom-modeline-major-mode-icon t "Отображение иконки основного режима.")
  (doom-modeline-major-mode-color-icon t "Отображение иконки основного режима.")
  (doom-modeline-minor-modes t "Отображение списка дополнительных режимов.")
  (doom-modeline-project-detection 'auto "Автоматическое определение проектов.")
  (doom-modeline-total-line-number t "Отображение общего количества строк.")
  (doom-modeline-vcs-max-length 40 "Максимальная длина названия ветки VCS.")
  (doom-modeline-workspace-name t "Отображение названия рабочего пространства."))


;; -> DOOM-THEMES
;; https://github.com/doomemacs/themes
;; Темы из DOOM Emacs
(use-package doom-themes
  :pin "melpa-stable"
  :ensure t
  :custom
  (doom-themes-enable-bold t "Включить поддержку полужирного начертания.")
  (doom-themes-enable-italic t "Включить поддержку наклонного начертания.")
  :config
  (doom-themes-visual-bell-config)
  ;; (doom-themes-treemacs-config)
  (load-theme 'doom-molokai t))


;; -> EDIT-INDIRECT
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
  :defer t
  :bind
  ("C-c '" . edit-indirect-region))


;; -> EDITORCONFIG
;; Поддержка https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :pin "nongnu"
  :ensure t
  :defer t
  :after (ws-butler)
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode "Очистка лишних пробелов методом `ws-butler'.")
  :config (editorconfig-mode 1))


;; -> ELEC-PAIR MODE
;; Встроенный пакет.
;; Автоматически вставляет при вводе одной скобки или кавычки парную ей. Если
;; выделен регион, то в скобки обрамляется он.
(use-package elec-pair
  :ensure nil
  :config
  (add-to-list 'electric-pair-pairs '(?« . ?»))
  (add-to-list 'electric-pair-pairs '(?{ . ?}))
  (add-to-list 'electric-pair-pairs '(?‘ . ’?))
  (add-to-list 'electric-pair-pairs '(?“ . ”?))
  (add-to-list 'electric-pair-pairs '(?‚ . ‘?))
  (electric-pair-mode t)) ;; Глобальный режим


;; -> ELECTRIC-INDENT MODE
;; Встроенный пакет.
;; Автоматический отступ. В основном только мешает, лучше выключить.
(use-package electric
  :ensure nil
  :config (electric-indent-mode -1)
  :custom (electric-indent-inhibit t "Не выравнивать предыдущую строку по нажатию Enter.")
  :hook (emacs-lisp-mode . electric-indent-local-mode))


;; -> EGLOT
;; Пакет для поддержки LSP.
;; https://elpa.gnu.org/packages/eglot.html
;;
;; ПОДГОТОВКА К РАБОТЕ
;; Установка серверов:
;; - Ansible:
;;   sudo npm install -g @ansible/ansible-language-server
;; - HTML:
;;   npm install -g vscode-langservers-extracted
;; - Markdown:
;;   sudo snap install marksman
(use-package eglot
  :pin "gnu"
  :ensure t
  :defer t
  :config
  (add-to-list 'eglot-server-programs
               '(ansible-mode . ("ansible-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("marksman")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("jedi-language-server")))
  (add-to-list 'eglot-server-programs
               '(ruby-mode . ("bundle" "exec" "rubocop" "--lsp")))
  :hook
  ((
    ansible-mode
    markdown-mode
    python-mode
    ruby-mode
    ) . eglot-ensure))


;; -> ELPY
;; Python IDE
;; https://elpy.readthedocs.io/en/latest/index.html
;; Краткая справка по использованию:
;; Проверка состояния: `elpy-config'.
;; Активация окружения: `pyenv-activate', указать путь к каталогу с окружением.
(use-package elpy
  :pin "melpa-stable"
  :ensure t
  :defer t
  :after (python-mode)
  :config
  (elpy-enable)
  (defalias 'workon 'pyvenv-workon)
  :hook python-mode)


;; -> EMACS-LISP-MODE
;; IT IS NOT A ELISP-MODE!
;; Встроенный пакет для EMACS Lisp
(use-package elisp-mode
  :ensure nil
  :config
  (setq-local
   tab-width 2)
  :mode
  ("\\abbrev_defs\\'" . lisp-data-mode)
  ("\\.el\\'" . emacs-lisp-mode))


;; -> EMACS
;; Настройки, предоставляемые базовой функциональностью Emacs
;; Можно считать встроенным пакетом
(use-package emacs
  :init
  (setq-default indent-tabs-mode nil) ;; Отключить `indent-tabs-mode'.
  :custom
  (auto-revert-check-vc-info t "Автоматически обновлять статусную строку")
  (backward-delete-char-untabify-method 'hungry "Удалять все символы выравнивания при нажатии [Backspace]")
  (blink-matching-paren t "Мигать, когда скобки парные")
  (calendar-week-start-day 1 "Начнём неделю с понедельника")
  (create-lockfiles nil "Не надо создавать lock-файлы")
  (cursor-type 'bar "Курсор в виде вертикальной черты")
  (custom-safe-themes t "Считать все темы безопасными")
  (delete-by-moving-to-trash t "При удалении файла помещать его в Корзину")
  (enable-local-variables :all "Считать все переменные из файлов .dir-locals.el безопасными")
  (gc-cons-threshold (* 50 1000 1000) "Увеличим лимит для сборщика мусора с 800 000 до 50 000 000")
  (inhibit-splash-screen t "Не надо показывать загрузочный экран")
  (inhibit-startup-message t "Не надо показывать приветственное сообщение")
  (initial-scratch-message nil "В новых буферах не нужно ничего писать")
  (load-prefer-newer t "Если есть файл elc, но el новее, загрузить el-файл")
  (locale-coding-system 'utf-8 "UTF-8 по умолчанию")
  (menu-bar-mode nil "Отключить показ главного меню")
  (overwrite-mode-binary nil "Выключить режим перезаписи текста под курсором для бинарных файлов")
  (overwrite-mode-textual nil "Выключить режим перезаписи текста под курсором для текстовых файлов")
  (ring-bell-function #'ignore "Заблокировать пищание")
  (safe-local-variable-values '((buffer-env-script-name ".venv/bin/activate")) "Безопасные переменные")
  (save-place-file (expand-file-name ".emacs-places" init-emacs-config-dir) "Хранить данные о позициях в открытых файлах в .emacs-places")
  (save-place-forget-unreadable-files t "Если файл нельзя открыть, то и помнить о нём ничего не надо")
  (scroll-bar-mode nil "Не показывать полосы прокрутки")
  (scroll-conservatively 100000 "TODO: проверить, что это такое")
  (scroll-margin 5 "При прокрутке помещать курсор на 5 строк выше / ниже верхней / нижней границы окна")
  (scroll-preserve-screen-position 1 "TODO: проверить, что это такое")
  (show-trailing-whitespace t "Показывать висячие пробелы")
  (source-directory "/usr/share/emacs/28.2/src/" "Путь к исходному коду EMACS")
  (suggest-key-bindings t "Показывать подсказку клавиатурной комбинации для команды")
  (tab-always-indent 'complete "Если можно — выровнять текст, иначе — автодополнение")
  (tool-bar-mode nil "Отключить показ панели инструментов")
  (tooltip-mode nil "Отключить показ подсказок с помощью GUI")
  (truncate-lines 1 "Обрезать длинные строки")
  (uniquify-buffer-name-style 'forward "Показывать директорию перед именем файла, если буферы одинаковые (по умолчанию имя<директория>)")
  (uniquify-separator "/" "Разделять буферы с похожими именами, используя /")
  (use-dialog-box nil "Диалоговые окна не нужны, будем использовать текстовый интерфейс")
  (user-full-name "Dunaevsky Maxim" "Имя пользователя")
  (visible-bell t "Эффект мигания при переходе в буфер")
  (window-divider-default-places 't "Разделители окон со всех сторон (по умолчанию только справа)")
  (window-divider-default-right-width 3  "Ширина в пикселях для линии-разделителя окон")

  :config
  (column-number-mode 1) ;; Показывать номер колонки в статусной строке
  (delete-selection-mode t) ;; Удалять выделенный фрагмент при вводе текста
  (global-auto-revert-mode 1) ;; Автоматически перезагружать буфер при изменении файла на дискею
  (global-hl-line-mode 1) ;; Подсветка активной строки
  (global-unset-key (kbd "<insert>")) ;; Режим перезаписи не нужен
  (global-unset-key (kbd "M-,")) ;; Такие маркеры не нужны
  (global-visual-line-mode 1) ;; Деление логических строк на видимые
  (hl-line-mode 1) ;; Подсвечивать текущую строку
  (line-number-mode t) ;; Показывать номер строки в статусной строке
  (menu-bar-mode 0) ;; Отключить показ меню
  (overwrite-mode 0) ;; Отключить режим перезаписи
  (prefer-coding-system 'utf-8)        ;; При попытке определить кодировку файла начинать перебор с UTF-8)
  (save-place-mode 1) ;; Помнить позицию курсора в открытых файлах
  (savehist-mode t) ;; Запоминать историю введенных в минибуфере команд
  (scroll-bar-mode 0) ;; Не показывать полосы прокрутки
  (set-default-coding-systems 'utf-8)  ;; Кодировка по умолчанию
  (set-keyboard-coding-system 'utf-8)  ;; Кодировка символов при вводе текста в терминале
  (set-language-environment 'utf-8)    ;; Кодировка языка по умолчанию
  (set-selection-coding-system 'utf-8) ;; Кодировка символов для передачи скопированных в буфер данных другим приложениям X11
  (set-terminal-coding-system 'utf-8) ;; Кодировка символов для вывода команд, запущенных в терминале
  (show-paren-mode 1) ;; Подсвечивать парные скобки
  (size-indication-mode 1) ;; Отображать размер буфера в строке статуса
  (when (fboundp 'too-bar-mode)
    (tool-bar-mode 0)) ;; Отключить отображение панели инструментов
  (tooltip-mode -1) ;; Отключить показ подсказок с помощью GUI
  (window-divider-mode t) ;; Отображать разделитель между окнами
  :bind
  (:map
   global-map
   ("<escape>" . keyboard-quit)           ;; ESC работает как и Ctrl+g, т. е. прерывает ввод команды
   ("C-x O" . previous-multiframe-window) ;; Перейти в предыдущее окно)
   ("C-x k" .
    (lambda ()
      (interactive)
      (kill-buffer (current-buffer))))    ;; Закрыть активный буфер без лишних вопросов
   ("C-x o" . next-multiframe-window)     ;; Перейти в следующее окно
   ("C-z" . undo)                         ;; Отмена
   ("M-'" . comment-or-uncomment-region)  ;; Закомментировать/раскомментировать область)
   ("M--" .
    (lambda ()
      (interactive)
      (insert "—"))) ;; Вставка длинного тире нажатием Alt+-
   ("S-<SPC>" . just-one-space)           ;; Заменить пробелы и TAB'ы до и после курсора на один пробел
   ([f3] . replace-string)                ;; Замена строки
   ([f9] . sort-lines)))                  ;; Отсортировать выделенные строки


;; -> FACE-REMAP
;; Встроенный пакет, отвечающий за отображение шрифтов
(use-package face-remap
  :ensure nil
  :custom
  (text-scale-mode-step 1.1 "Шаг увеличения масштаба"))


;; -> FILES
;; Это встроенный пакет для управления файлами
(use-package files
  :ensure nil
  :custom
  (auto-save-file-name-transforms `((".*" , init-emacs-autosave-dir) t))
  (delete-old-versions t "Удалять старые резервные копии файлов без лишних вопросов")
  (large-file-warning-threshold (* 100 1024 1024) "Предупреждение при открытии файлов больше 100 МБ (по умолчанию — 10 МБ)")
  (make-backup-files nil "Резервные копии не нужны, у нас есть undo-tree")
  (save-abbrevs 'silently "Сохранять аббревиатуры без лишних вопросов"))


;; -> FLYCHECK
;; https://flycheck.org
;; Проверка синтаксиса на лету с помощью статических анализаторов
(use-package flycheck
  :pin "melpa-stable"
  :ensure t
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save new-line))
  (flycheck-highlighting-mode 'lines "Стиль отображения проблемных мест — вся строка")
  (flycheck-indication-mode 'left-fringe "Место размещения маркера ошибки — левая граница")
  (flycheck-locate-config-file-functions '(
                                           flycheck-locate-config-file-by-path
                                           flycheck-locate-config-file-ancestor-directories
                                           flycheck-locate-config-file-home))
  (flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc" "Файл настроек Markdownlint")
  (flycheck-textlint-config ".textlintrc.yaml" "Файл настроек Textlint")
  :hook
  ((
    adoc-mode
    conf-mode
    css-mode
    dockerfile-mode
    emacs-lisp-mode
    js2-mode
    json-mode
    latex-mode
    lisp-data-mode
    makefile-mode
    markdown-mode
    nxml-mode
    python-mode
    rst-mode
    ruby-mode
    sh-mode
    sql-mode
    terraform-mode
    web-mode
    yaml-mode
    ) . flycheck-mode))


;; -> FLYCHECK-EGLOT
;; https://github.com/flycheck/flycheck-eglot
;; Интеграция Flycheck + Eglot
(use-package flycheck-eglot
  :ensure t
  :defer t
  :after (eglot flycheck))



;; -> FLYMAKE
;; Более свежая версия встроенного пакета из репозитория gnu
;; Используется для проверки `init.el'.
;; https://elpa.gnu.org/packages/flymake.html
(use-package flymake
  :pin "gnu"
  :ensure t
  :hook
  ((
    emacs-lisp-mode
    lisp-data-mode
    rst-mode
    ) . flymake-mode))


;; -> FLYSPELL-MODE
;; Проверка орфографии с помощью словарей
;; Использовать пакет только в том случае, когда дело происходит в Linux и
;; Aspell доступен
(use-package flyspell
  :when (and
         (string-equal system-type "gnu/linux") ;; Aspell для Linux, в Windows без проверки орфографии
         (file-exists-p "/usr/bin/aspell"))    ;; Надо убедиться, что программа установлена в ОС
  :custom
  (ispell-program-name "/usr/bin/aspell")
  :hook
  ((
    adoc-mode
    markdown-mode
    rst-mode
    ) . flyspell-mode)
  ((
    emacs-lisp-mode
    ) . flyspell-prog-mode))


;; -> FORMAT-ALL
;; https://github.com/lassik/emacs-format-all-the-code
;; Форматирование кода по нажатию [F12]
(use-package format-all
  :pin "melpa-stable"
  :ensure t
  :defer t
  :bind (([f12] . format-all-buffer)))


;; -> GOTO-ADDRESS-MODE
;; Встроенный пакет
;; Подсвечивает ссылки и позволяет переходить по ним
(use-package goto-addr
  :ensure t
  :hook
  ((
    adoc-mode
    emacs-lisp-mode
    markdown-mode
    rst-mode
    text-mode
    web-mode
    ) . goto-address-mode))


;; -> HELM
;; https://emacs-helm.github.io/
;; Подсказки и автодополнение ввода
;; [C-o] — переключение между источниками подсказок (история и полный список команд)
(use-package helm
  :pin "nongnu"
  :ensure t
  :diminish nil
  :config
  (helm-mode 1)
  :bind
  (:map global-map
        ("M-x" . helm-M-x)))

;; -> HELM-COMPANY
;;
;; Расширение для `company-mode'.
(use-package helm-company
  :pin "melpa-stable"
  :ensure t
  :defer t
  :after (company helm)
  :bind
  (:map company-mode-map ("C-:" . helm-company))
  (:map company-active-map ("C-:" . helm-company)))


;; -> HL-TODO
;; https://github.com/tarsius/hl-todo
;; Подсветка TODO, FIXME и т. п.
(use-package hl-todo
  :pin "melpa-stable"
  :ensure t
  :config (global-hl-todo-mode t))


;; -> IBUFFER
;; Встроенный пакет для удобной работы с буферами.
;; По нажатию F2 выводит список открытых буферов.
;; Взято из конфига автора пакета
;; https://github.com/jwiegley/dot-emacs/blob/master/init.org
(use-package ibuffer
  :custom
  (ibuffer-formats ;; Форматирование вывода
   '(
     (;; Полный формат
      mark      ;; Отметка
      modified  ;; Буфер изменён?
      read-only ;; Только чтение?
      locked    ;; Заблокирован?
      " "
      (name 30 40 :left :elide) ;; Имя буфера: от 30 до 40 знаков
      " "
      (mode 8 -1 :left)         ;; Активный режим: от 8 знаков по умолчанию, при необходимости увеличить
      " "
      filename-and-process)     ;; Имя файла и процесс
     ( ;; Сокращённый формат
      mark      ;; Отметка?
      " "
      (name 32 -1) ;; Имя буфера: 32 знака, при неоходимости — расширить на сколько нужно
      " "
      filename)))  ;; Имя файла)
  (ibuffer-expert 1 "Не запрашивать подтверждение для опасных операций")
  (ibuffer-hidden-filter-groups (list "*Internal*" ) "Не показывать эти буферы")
  (ibuffer-saved-filter-groups                    ;; Группы по умолчанию
   '(
     ("default"
      ("Dired" (mode . dired-mode))
      ("EMACS Lisp"
       (or
        (mode . emacs-lisp-mode)
        (mode . lisp-data-mode)))
      ("Org" (mode . org-mode))
      ("Markdown" (mode . markdown-mode))
      ("AsciiDoc" (mode . adoc-mode))
      ("ReStructured Text" (mode . rst-mode))
      ("CONF / INI"
       (or
        (mode . conf-mode)
        (name . "\\.conf\\'")
        (name . "\\.editorconfig\\'")
        (name . "\\.ini\\'")))
      ("XML"
       (or
        (mode . nxml-mode)
        (mode . xml-mode)))
      ("YAML" (mode . yaml-mode))
      ("Makefile"
       (or
        (mode . makefile-mode)
        (name  . "^Makefile$")))
      ("Python"
       (or
        (mode . anaconda-mode)
        (mode . python-mode)))
      ("SSH keys" (or (name . "^\\*.pub$")))
      ("Shell-script"
       (or
        (mode . shell-script-mode)
        (mode . sh-mode)))
      ("Terraform"
       (or
        (mode . terraform-mode)
        (name . "^\\*.tf$")))
      ("SQL" (mode . sql-mode))
      ("Web"
       (or
        (mode . javascript-mode)
        (mode . js-mode)
        (mode . js2-mode)
        (mode . web-mode)))
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
  (ibuffer-show-empty-filter-groups nil "Не показывать пустые группы")
  (ibuffer-sorting-mode 'filename/process "Сортировать файлы по имени / процессу")
  (ibuffer-truncate-lines nil "Не обкусывать длинные строки")
  (ibuffer-use-other-window t "Открывать ibuffer в отдельном окне")
  :commands ibuffer
  :init
  (defalias 'list-buffers 'ibuffer)
  (add-hook 'ibuffer-mode-hook #'(lambda ()
                                   (ibuffer-switch-to-saved-filter-groups "default")))
  :bind
  ([f2] . ibuffer))


;; -> JS2-MODE
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :pin "gnu"
  :ensure t
  :defer t
  :mode "\\.js\\'")


;; -> JSON-MODE
;; Поддержка JSON
(use-package json-mode
  :pin "melpa-stable"
  :ensure t
  :defer t)


;; -> MAGIT
;; https://magit.vc/
;; Magic + Git + Git-gutter. Лучшее средство для управления Git.
(use-package magit
  :pin "nongnu"
  :ensure t
  :defer t
  :custom
  (magit-define-global-key-bindings t "Включить глобальные сочетания Magit."))


;; -> MAKEFILE
;; Встроенный пакет для работы с Makefile
(use-package make-mode
  :ensure nil
  :defer t
  ;; :config (setq-local indent-tabs-mode t)
  )


;; -> MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
;; Режим для работы с файлами в формате Markdown
(when init-emacs-version-greater-than-27-1
  (use-package markdown-mode
    :pin "nongnu"
    :ensure t
    :defer t
    :custom
    (markdown-fontify-code-blocks-natively t "Подсвечивать синтаксис в примерах кода")
    (markdown-header-scaling-values '(1.0 1.0 1.0 1.0 1.0 1.0) "Все заголовки одной высоты")
    (markdown-list-indent-width 4 "Размер отступа для выравнивания вложенных списков")
    :config (setq-local word-wrap t)
    :bind (
           :map markdown-mode-map
           ("M-." . markdown-follow-thing-at-point))))


;; -> MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
;; Позволяет использовать мультикурсорность.
(use-package multiple-cursors
  :pin "nongnu"
  :ensure t
  :bind
  (:map global-map
        ("C-S-c C-S-c" . mc/edit-lines)
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c C-<" . mc/mark-all-like-this))
  :config
  (add-to-list 'after-make-frame-functions
               (lambda ()
                 (when (display-graphic-p)
                   ;; Если режим графический, то курсоры можно расставлять с помощью Alt+Click
                   (progn
                     (global-unset-key (kbd "M-<down-mouse-1>"))
                     (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
                     )))))


;; -> NERD-ICONS
;; https://github.com/rainstormstudio/nerd-icons.el
;; Требуется для корректной работы `doom-modeline'. Начиная с версии 4.0.0
;; пакет `all-the-icons' не поддерживается.
;; Для установки самих шрифтов следует использовать команду `nerd-icons-install-fonts'.
;; В Debian Linux шрифты будут загружены в каталог `~/.local/share/fonts'. Рекомендуется
;; скопировать их в `/usr/local/share/fonts/'.
(use-package nerd-icons
  :ensure t)


;; -> NERD-ICONS-DIRED
;; https://github.com/rainstormstudio/nerd-icons-dired
;; Иконки в `dired'.
(use-package nerd-icons-dired
  :ensure t
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))


;; -> NERD-ICONS-IBUFFER
;;
;; Отображение иконок в ibuffer
(use-package nerd-icons-ibuffer
  :pin "melpa-stable"
  :ensure t
  :after (ibuffer nerd-icons)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; -> MODUS-THEMES
;; Темы от проекта gnu
;; https://www.gnu.org/software/emacs/manual/html_mono/modus-themes.html
;; https://git.sr.ht/~protesilaos/modus-themes
;; (use-package modus-themes
;;   :pin "gnu"
;;   :ensure t
;;   :config (load-theme 'modus-vivendi-tritanopia))


;; -> NXML-MODE
;; Встроенный пакет, почти как `xml-mode', только лучше и новее
(use-package nxml-mode
  :defer t
  :ensure nil
  :custom
  (nxml-attribute-indent 4 "Выравнивание атрибутов")
  (nxml-auto-insert-xml-declaration-flag nil "Не вставлять декларацию")
  (nxml-bind-meta-tab-to-complete-flag t "Использовать TAB для завершения ввода")
  (nxml-child-indent 4 "Выравнивание дочерних элементов")
  (nxml-slash-auto-complete-flag t "Закрывать теги по вводу /")
  :mode
  ("\\.pom\\'"
   "\\.xml\\'"))


;; -> ORG-MODE
;; https://orgmode.org/
;; Органайзер, и не только
(use-package org
  :pin "gnu"
  :ensure t
  :defer t
  :config
  (setq-local
   truncate-lines nil ;; Не обрезать строки
   word-wrap t))      ;; Перенос длинных строк


;; -> PACKAGE-LINT
;; https://github.com/purcell/package-lint
;; Проверка пакетов Emacs
(use-package package-lint
  :pin "melpa-stable"
  :ensure t
  :defer t)


;; -> PROJECTILE
;; https://docs.projectile.mx/projectile/installation.html
;; Управление проектами. Чтобы каталог считался проектом, он должен быть
;; под контролем любой системы версионирования, либо содержать специальные
;; файлы. В крайнем случае сгодится пустой файл .projectile
;; Подробнее здесь: https://docs.projectile.mx/projectile/projects.html
(use-package projectile
  :pin "nongnu"
  :ensure t
  :defer t
  :bind (
         :map projectile-mode-map
         ("M-p" . projectile-command-map))
  :config
  (projectile-register-project-type 'sphinx
                                    '("Makefile" "source" "source/conf.py")
                                    :project-file "source/conf.py"
                                    :src-dir "source/"
                                    :install "pip3 install -r requirements.txt -U"
                                    :compile "make dirhtml"
                                    :run "python3 -m http.server -d build/dirhtml -b 127.0.0.1 8080")
  (projectile-register-project-type 'hugo

                                    '("hugo.toml")
                                    :project-file "hugo.toml"
                                    :src-dir "content"
                                    :compile "hugo"
                                    :run "hugo server")
  (projectile-mode 1))


;; -> PULSAR
;; Вспыхивание строки, к которой переместился курсор
;; https://git.sr.ht/~protesilaos/pulsar
(when init-emacs-version-greater-than-27-1
  ;; Этот пакет требует Emacs версии 27.1 или новее
  (use-package pulsar
    :pin "gnu"
    :ensure t
    :custom (pulsar-pulse t)
    :hook
    (next-error . pulsar-pulse-line)
    (after-init . pulsar-global-mode)
    :config
    (add-to-list 'pulsar-pulse-functions 'ace-window)
    (add-to-list 'pulsar-pulse-functions 'flycheck-next-error)
    (add-to-list 'pulsar-pulse-functions 'flyspell-goto-next-error)
    (add-to-list 'pulsar-pulse-functions 'next-multiframe-window)
    (add-to-list 'pulsar-pulse-functions 'recenter-top-bottom)))


;; -> PYTHON-MODE
;; Встроенный пакет для работы с Python
(use-package python-mode
  :pin "melpa-stable"
  :ensure t
  :custom
  (py-pylint-command-args "--max-line-length 120" "Дополнительные параметры, передаваемые pylint"))


;; -> RAINBOW-DELIMITERS-MODE
;; https://github.com/Fanael/rainbow-delimiters
;; Подсветка парных скобок одним и тем же цветом
(use-package rainbow-delimiters
  :pin "nongnu"
  :ensure t
  :diminish nil
  :hook
  ((
    adoc-mode
    conf-mode
    css-mode
    emacs-lisp-mode
    js2-mode
    json-mode
    lisp-data-mode
    makefile-gmake-mode
    makefile-mode
    markdown-mode
    nxml-mode
    org-mode
    python-mode
    rst-mode
    sh-mode
    sql-mode
    terraform-mode
    web-mode
    yaml-mode
    ) . rainbow-delimiters-mode))


;; => RAINBOW-MODE
;; https://elpa.gnu.org/packages/rainbow-mode.html
;; Подсветка строк с цветами нужным цветом, например #153415, #223956
(use-package rainbow-mode
  :pin "gnu"
  :ensure t
  :diminish nil
  :hook
  ((
    css-mode
    emacs-lisp-mode
    web-mode
    ) . rainbow-mode))


;; -> RUSSIAN-TECHWRITER
;; Метод ввода для технических писателей
;; https://github.com/dunmaksim/emacs-russian-techwriter-input-method
(use-package russian-techwriter
  :pin "melpa-stable"
  :ensure t
  :init (setq-default default-input-method 'russian-techwriter))


;; -> REVERSE-IM
;; https://github.com/a13/reverse-im.el
;; Чтобы сочетания клавиш работали в любой раскладке.
(use-package reverse-im
  :pin "melpa-stable"
  :ensure t
  :after (russian-techwriter)
  :custom
  (reverse-im-input-methods
   '(
     "russian-computer"
     "russian-techwriter"))
  :config (reverse-im-mode t))


;; -> RST-MODE
;; Основной режим для редактирования reStructutedText
;; Встроенный пакет.
;; Больше здесь: https://www.writethedocs.org/guide/writing/reStructuredText/
(use-package rst
  :ensure t
  :defer t
  :custom
  (rst-default-indent 3)
  (rst-indent-comment 3)
  (rst-indent-field 3)
  (rst-indent-literal-minimized 3)
  (rst-indent-width 3)
  (rst-toc-indent 3)
  :mode
  (("\\.rest\\'" . rst-mode)
   ("\\.rst\\'" . rst-mode)
   ("\\.txt\\'" . rst-mode)))


;; -> RUBY-MODE
;; Встроенный пакет
(use-package ruby-mode
  :ensure nil
  :defer t
  :init
  (defvar ruby-indent-offset 2 "Ширина TAB'а в `ruby-mode'.")
  :mode
  ("\\Vagrantfile\\'"
   "\\.rb\\'"))


;; -> SHELL-SCRIPT-MODE
;; Встроенный пакет
(use-package sh-script
  :ensure nil
  :defer t
  :mode
  ("\\.bashrc\\'" . shell-script-mode)
  ("\\.profile\\'" . shell-script-mode)
  ("\\.sh\\'" . shell-script-mode))


;; -> SPHINX-MODE
;; https://github.com/Fuco1/sphinx-mode
;; Дополнительные функции для `rst-mode', если работаем со Sphinx.
(use-package sphinx-mode
  :pin "melpa-stable"
  :ensure t
  :defer t
  :hook (rst-mode-hook . sphinx-mode))


;; -> TERRAFORM-MODE
;; https://github.com/emacsorphanage/terraform-mode
;; Работа с файлами конфигурации Terraform
(use-package terraform-mode
  :pin "melpa-stable"
  :ensure t
  :defer t
  :mode "\\.terraformrc\\'")


;; -> TREEMACS — awesome file manager (instead NeoTree)
;; https://github.com/Alexander-Miller/treemacs
;; Дерево файлов и каталогов
(use-package treemacs
  :pin "melpa-stable"
  :ensure t
  :defer t
  :custom
  (treemacs-eldoc-display 'simple)
  (treemacs-follow-after-init t)
  (treemacs-indentation 2)
  (treemacs-position 'left)
  (treemacs-width 35 "Ширина окна Treemacs")
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)
        ("C-x t 1" . treemacs-delete-orher-windows)
        ("C-x t t" . treemacs)
        ("C-x t d" . treemacs-select-directory)
        ("C-x t B" . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :config
  (progn
    (treemacs-follow-mode 1)     ;; При смене буфера TreeMacs сменит позицию в дереве
    (treemacs-git-mode 'simple)  ;; Простой режим
    (treemacs-filewatch-mode 1)) ;; Отслеживание изменений в ФС на лету
  (define-key treemacs-mode-map (kbd "f") 'find-grep))


;; -> TREEMACS-MAGIT
(use-package treemacs-magit
  :pin "melpa-stable"
  :ensure t
  :defer t
  :after (treemacs magit))


;; -> TREEMACS-PROJECTILE
(use-package treemacs-projectile
  :pin "melpa-stable"
  :ensure t
  :defer t
  :after (treemacs projectile))


;; -> UNDO-TREE
;; https://gitlab.com/tsc25/undo-tree
;; Не только предоставляет привычное поведение при отмене команд, но и даёт мощные возможности по
;; ведению дерева правок.
(use-package undo-tree
  :pin "gnu"
  :ensure t
  :defer t
  :custom
  (undo-tree-auto-save-history nil "Отключить создание резервных копий файлов")
  :config
  (global-undo-tree-mode 1))


;; -> WEB-MODE
;; https://web-mode.org/
(use-package web-mode
  :pin "nongnu"
  :ensure t
  :custom
  (web-mode-attr-indent-offset 4 "Отступ в атрибутов — 4 пробела")
  (web-mode-enable-block-face t "Отображение")
  (web-mode-enable-css-colorization t "Код или имя цвета при редактировании CSS будут отмечены фоном этого цвета")
  (web-mode-enable-current-element-highlight t "Подсветка активного элемента разметки")
  (web-mode-html-offset 2 "Отступ в 2 знака для корректной работы `highlight-indentation-mode'.")
  (web-mode-markup-indent-offset 2 "Отступ при вёрстке HTML — 2 пробела")
  :init
  (setq-default major-mode 'web-mode)
  :mode "\\.html\\'")


;; -> WHICH-KEY MODE
;; https://github.com/justbur/emacs-which-key
;; Показывает подсказки к сочетаниям клавиш.
(use-package which-key
  :pin "gnu"
  :ensure t
  :diminish nil
  :custom
  (which-key-idle-delay 2 "Задержка появления подсказки")
  (which-key-idle-secondary-delay 0.05 "Ещё одна задержка появления подсказки")
  :config
  (which-key-mode 1)
  (which-key-setup-minibuffer)
  (which-key-setup-side-window-right)) ;; Показывать подсказки справа


;; -> WHITESPACE MODE
;; Встроенный пакет для отображения невидимых символов.
(use-package whitespace
  :diminish nil
  :custom
  (whitespace-display-mappings ;; Отображение нечитаемых символов
   '(
     (space-mark  ?\    [?\xB7]     [?.])      ;; Пробел
     (space-mark  ?\xA0 [?\xA4]     [?_])      ;; Неразрывный пробел
     (newline-mark ?\n  [?¶ ?\n]    [?$ ?\n])  ;; Конец строки
     (tab-mark    ?\t   [?\xBB ?\t] [?\\ ?\t]) ;; TAB
     ))
  (whitespace-line-column 1000 "По умолчанию подсвечиваются длинные строки. Не надо этого делать.")
  :hook
  ((
    adoc-mode
    conf-mode
    css-mode
    dockerfile-mode
    emacs-lisp-mode
    js2-mode
    json-mode
    latex-mode
    lisp-data-mode
    makefile-gmake-mode
    makefile-mode
    markdown-mode
    nxml-mode
    org-mode
    python-mode
    rst-mode
    ruby-mode
    sh-mode
    sql-mode
    terraform-mode
    web-mode
    yaml-mode) . whitespace-mode))


;; -> WINDMOVE
;; Встроенный пакет для перемещения между окнами Emacs
(use-package windmove
  :bind
  (:map global-map
        ("C-x <up>" . windmove-up)
        ("C-x <down>" . windmove-down)))


;; -> WINDOW
;; Встроенный пакет, отвечает за управление размерами окон
(use-package window
  :bind
  (:map global-map
        ("S-C-<left>" . shrink-window-horizontally)   ;; [Ctrl+Shift+←]   Уменьшить размер окна по ширине
        ("S-C-<right>" . enlarge-window-horizontally) ;; [Ctrl+Shift+→]   Увеличить размер окна по ширине
        ("S-C-<down>" . enlarge-window)               ;; [Ctrl+Shift+↓]   Увеличить размер окна по ширине
        ("S-C-<up>" . shrink-window)                  ;; [Ctrl+Shift+↑]   Уменьшить размер окна по высоте
        ([C-tab] . next-buffer)                       ;; [Ctrl+Tab]       Следующий буфер
        ([C-S-iso-lefttab] . previous-buffer)))       ;; [Ctrl+Shift+Tab] Предыдущий буфер)


;; -> WS-BUTLER
;; https://github.com/lewang/ws-butler
;; Удаляет висячие пробелы только из изменённых строк.
(use-package ws-butler
  :pin "nongnu"
  :ensure t
  :defer t
  :hook
  ((
    adoc-mode
    conf-mode
    dockerfile-mode
    emacs-lisp-mode
    js2-mode
    latex-mode
    markdown-mode
    nxml-mode
    python-mode
    rst-mode
    sh-mode
    sql-mode
    terraform-mode
    web-mode
    yaml-mode
    ) . ws-butler-mode))


;; -> YAML-MODE
;; https://github.com/yoshiki/yaml-mode
;; Работа с YAML-файлами
(use-package yaml-mode
  :pin "melpa-stable"
  :ensure t
  :defer t
  :mode
  ("\\.ansible\\-lint\\'"
   "\\.pre\\-commit\\-config\\.yaml\\'"
   "\\.yaml\\'"
   "\\.yamllint\\'"
   "\\.yamllint\\-config\\.yaml\\'"
   "\\.yfm\\'"
   "\\.yml\\'"))


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setup-gui-settings (selected-frame))

;; -> CUSTOM FILE
;; Пользовательские настройки, сделанные через CUSTOMIZE
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init.el)
;;; init.el ends here
