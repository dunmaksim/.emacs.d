;;; init.el --- Dunaevsky Maxim GNU EMACS config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Настройки EMACS для работы техническим писателем.

;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p) ;; Использовать y и n вместо yes и no (сокращает объём вводимого текста для подтверждения команд)

(defconst init-el-config-dir
  (file-name-directory user-init-file)
  "Корневой каталог для размещения настроек.")

(defconst init-el-autosave-dir
  (expand-file-name "saves" init-el-config-dir)
  "Каталог для файлов автосохранения.")

(defconst init-el-package-user-dir
  (expand-file-name "elpa" init-el-config-dir)
  "Пользовательский каталог с пакетами.")

(defconst init-el-is-linux
  (string-equal system-type "gnu/linux")
  "Используется ОС на базе GNU/Linux.")

;; Возвращает t, если версия Emacs больше или равна указанной.
(defun emacs-version-not-less-than (major minor)
  "True when Emacs version is not less than MAJOR and MINOR versions."
  (or
    (> emacs-major-version major)
    (and (= emacs-major-version major)
      (>= emacs-minor-version minor))))

;; Если нужного каталога не существует, его следует создать
(dolist
    (init-el-dir
     (list
      init-el-config-dir
      init-el-autosave-dir
      init-el-package-user-dir))
  (unless (file-directory-p init-el-dir)
    (make-directory init-el-dir)
    (message (format "Создан каталог %s" init-el-dir))))

(defconst emacs-default-font-height 16 "Размер шрифта по умолчанию.")

(require 'custom)

;; Если используется старая версия EMACS, нужно указать параметры протокола TLS.
;; В противном случае будут проблемы при загрузке архива пакетов.
(when (< emacs-major-version 27)
  (require 'gnutls)
  (custom-set-variables
    '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3" "Необходимо для старых версий Emacs.")))

(custom-set-variables
  '(create-lockfiles nil "Не создавать lock-файлы")
  '(cursor-type 'bar "Курсор в виде вертикальной черты")
  '(delete-by-moving-to-trash t "Удалять файлы в Корзину")
  '(gc-cons-threshold (* 50 1000 1000) "Увеличить размер памяти для сборщика мусора")
  '(indent-tabs-mode nil "Отключить `indent-tabs-mode'.")
  '(inhibit-startup-screen t "Не показывать приветственный экран")
  '(initial-scratch-message nil "Пустой буфер *scratch*")
  '(load-prefer-newer t "Если есть файл elc, но el новее, загрузить el-файл.")
  '(menu-bar-mode nil "Выключить отображение меню")
  '(ring-bell-function #'ignore "Отключить звуковое сопровождение событий")
  '(save-place-file (expand-file-name ".emacs-places" init-el-config-dir) "Хранить данные о позициях в открытых файлах в .emacs-places")
  '(save-place-forget-unreadable-files t "Если файл нельзя открыть, то и помнить о нём ничего не надо")
  '(scroll-bar-mode nil "Отключить полосы прокрутки")
  '(scroll-conservatively 101 "TODO: проверить что это такое")
  '(scroll-margin 4 "Отступ от верхней и нижней границ буфера")
  '(show-trailing-whitespace t "Подсветка висячих пробелов")
  '(tab-always-indent 'complete "Если можно — выровнять текст, иначе — автодополнение.")
  '(tool-bar-mode nil "Отключить панель инструментов")
  '(truncate-lines 1 "Обрезать длинные строки")
  '(use-dialog-box nil "Диалоговые окна ОС не нужны")
  '(user-full-name "Dunaevsky Maxim" "Имя пользователя")
  '(user-mail-address "dunmaksim@yandex.ru" "Адрес электронной почты")
  '(visible-bell t "Мигать буфером при переходе в него"))


(global-unset-key (kbd "<insert>")) ;; Режим перезаписи не нужен
(global-unset-key (kbd "M-,"))      ;; Такие маркеры не нужны
(global-set-key (kbd "C-x k")       ;; Закрыть буфер по нажатию [C-x k]
  (lambda()
    (interactive)
    (kill-buffer (current-buffer))))
(global-set-key (kbd "M--")         ;; Вставка длинного тире
  (lambda()
    (interactive)
    (insert "—")))

;; Определение пути к каталогу с исходным кодом
(when init-el-is-linux
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
        (custom-set-variables '(source-directory init-el-emacs-source-path))
        (message (format "Исходный код обнаружен в каталоге %s" init-el-emacs-source-path))))
    ;; Каталог не существует
    (message (format "Каталог %s не существует." init-el-emacs-source-path))))


;; 📦 PACKAGE
;; Встроенный пакет.
;; Управление другими пакетами.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(custom-set-variables
  '(package-archive-priorities '(("gnu" . 50)
                                  ("nongnu" . 40)
                                  ("melpa-stable" . 30)
                                  ("melpa" . 20))) ;; Приоритеты архивов
  '(package-native-compile t "Компиляция пакетов во время установки, а не при первом запуске"))

(add-to-list 'package-pinned-packages '("use-package" . "gnu")) ;; Пакет `use-package' нужно устанавливать из репозитория GNU.
(add-to-list 'package-pinned-packages '("gnu-elpa-keyring-update" . "gnu")) ;; Этот тоже только из репозитория GNU.


(unless (package-installed-p 'gnu-elpa-keyring-update)
  (custom-set-variables '(package-check-signature 'nil "Отключить проверку подписей"))
  (package-refresh-contents)
  (package-install 'gnu-elpa-keyring-update t)
  (custom-set-variables '(package-check-signature 'all "Включить проверку подписей")))

;; Если пакет `use-package' не установлен, нужно это сделать.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

(require 'use-package)

;; Настройки отладочного режима
(when init-file-debug
  (custom-set-variables
    '(use-package-verbose t "Подробный режим работы `use-package'.")
    '(use-package-compute-statistics t "Сбор статистики `use-package'.")
    '(use-package-expand-minimally t "TODO: ???")
    '(debug-on-error t "Автоматически перейти в режим отладки при ошибках.")))


;; 📦 ABBREV-MODE
;; Встроенный пакет.
;; Использование аббревиатур -- фрагментов текста, которые при вводе
;; определённой последовательности символов заменяются на другую,
;; например:
;; tf → Terraform
;; yc → Yandex Cloud
;; Это встроенный пакет
(use-package abbrev
  :ensure nil
  :defer t
  :diminish "abb")


;; 📦 ACE-WINDOW
;; https://github.com/abo-abo/ace-window
;; Быстрое переключение между окнами по Alt+O
(use-package ace-window
  :pin "gnu"
  :ensure t
  :bind (:map global-map
              ("M-o" . ace-window)))


;; 📦 ADJUST-PARENS
;; https://elpa.gnu.org/packages/adjust-parens.html
;; Пакет для автоматического управления скобочками и уровнями отступов.
(use-package adjust-parens
  :pin "gnu"
  :ensure t
  :hook (emacs-lisp-mode . adjust-parens-mode)
  :bind (:map emacs-lisp-mode-map
          ("<tab>" . lisp-indent-adjust-parens)
          ("<backtab>" . lisp-dedent-adjust-parens)))



;; 📦 AGGRESSIVE-INDENT
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


;; 📦 ANACONDA-MODE
;; https://github.com/proofit404/anaconda-mode
;; Расширенная поддержка Python.
(use-package anaconda-mode
  :pin "melpa-stable"
  :ensure t
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))


;; 📦 ANSIBLE
;; https://github.com/k1LoW/emacs-ansible
;; Дополнительные возможности при работе с YAML-файлами Ansible
(use-package ansible
  :pin "melpa-stable"
  :ensure t
  :defer t)


;; 📦 ANZU
;; https://github.com/emacsorphanage/anzu
;; Подсказки о количестве совпадений при поиске с помощью `isearch'.
(use-package anzu
  :pin "nongnu"
  :ensure t
  :diminish ""
  :config
  (global-anzu-mode 1))


;; 📦 APHELEIA
;; https://github.com/radian-software/apheleia
;; Форматирование содержимого буфера с помощью внешних средств
(use-package apheleia
  :pin "melpa-stable"
  :ensure t
  :diminish "")


;; 📦 AUTOREVERT
;; Встроенный пакет.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
;; Автоматическое обновление буферов.
;; По умолчанию `global-auto-revert-mode' работает только с файловыми
;; буферами.
(use-package autorevert
  :custom
  (auto-revert-check-vc-info t "Автоматически обновлять статусную строку")
  :config
  ;; Автоматически перезагружать файловый буфер при изменении файла на диске.
  (global-auto-revert-mode 1)
  :hook
  ;; Включить автообновление буферов с `dired-mode'.
  (dired-mode . auto-revert-mode))


;; 📦 BIND-KEY
;; https://github.com/jwiegley/use-package
;; Позволяет настраивать привязки клавиш более простым и наглядным способом чем
;; тот, что предоставляет Emacs
(use-package bind-key
  :ensure t
  :pin "gnu")


;; 📦 BUFFER-ENV
;; https://github.com/astoff/buffer-env
;; Настройка окружения отдельно для каждого буфера.
;; Настройки загружаются из файла `.env' в каталоге проекта или `.dir-locals.el'.
;; Во первом случае в файле должна быть указана команда для активации окружения, например:
;; source .venv/bin/activate
;; Во втором достаточно задать значение переменной `buffer-env-script-name'.
(when (emacs-version-not-less-than 27 1)
  (use-package buffer-env
    :ensure t
    :pin "gnu"
    :defer t
    :after (files)
    :hook ((
             hack-local-variables
             comint-mode
             ) . buffer-env-update)))


;; 📦 CALENDAR
;; Встроенный пакет
(use-package calendar
  :custom
  (calendar-week-start-day 1 "Начнём неделю с понедельника"))


;; 📦 CHECKDOC
;; Встроенный пакет для проверки строк документации.
(use-package checkdoc
  :hook (emacs-lisp-mode . checkdoc-minor-mode))


;; 📦 COMPANY-MODE
;; https://company-mode.github.io/
;; Автодополнение
(use-package company
  :pin "gnu"
  :ensure t
  :defer t
  :diminish ""
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


;; 📦 CONF-MODE
;; Встроенный пакет.
;; Основной режим для редактирования конфигурационных файлов INI/CONF
(use-package conf-mode
  :ensure nil
  :defer t
  :mode
  ("\\.editorconfig\\'"
   "\\.env\\'"
   "\\.flake8\\'"
   "\\.ini\\'"
   "\\.pylintrc\\'"))


;; 📦 CSS-MODE
;; Встроенный пакет.
;; Поддержка CSS.
(use-package css-mode
  :ensure nil
  :defer t
  :custom
  (css-indent-offset 2)
  :mode "\\.css\\'")


;; 📦 CSV-MODE
;; https://elpa.gnu.org/packages/csv-mode.html
;; Поддержка CSV
(use-package csv-mode
  :pin "gnu"
  :ensure t
  :mode "\\.csv\\'")


;; 📦 CUS-EDIT
;; Встроенный пакет.
;; Управление custom-файлами
(use-package cus-edit
  :custom
  (custom-file
    (expand-file-name
      (convert-standard-filename "custom.el")
      init-el-config-dir)
    "Файл для сохранения пользовательских настроек, сделанных в customize."))


;; 📦 CUSTOM
;; Встроенный пакет
;; Управление настройками, сделанными с помощью customize.
(use-package custom
  :custom
  (custom-safe-themes t "Считать все темы безопасными"))


;; 📦 DELSEL
;; Встроенный пакет.
;; Используется для управления удалением выделенного текста.
(use-package delsel
  :config
  (delete-selection-mode t)) ;; Удалять выделенный фрагмент при вводе текста)


;; 📦 DESKTOP
;; Встроенный пакет.
;; Сохранение состояния Emacs между сессиями.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(use-package desktop
  :ensure nil
  :custom
  (desktop-auto-save-timeout 20 "Автосохранение каждые 20 секунд.")
  (desktop-dirname init-el-config-dir "Каталог для хранения файла .desktop.")
  (desktop-load-locked-desktop t "Загрузка файла .desktop даже если он заблокирован.")
  (desktop-modes-not-to-save '(dired-mode Info-mode info-lookup-mode)) ; А вот эти не сохранять
  (desktop-restore-frames t "Восстанавливать фреймы.")
  (desktop-save t "Сохранять список открытых буферов, файлов и т. д. без лишних вопросов.")
  :config
  (desktop-save-mode 1)
  (add-hook 'server-after-make-frame-hook #'desktop-read))


;; 📦 DIFF-HL
;; https://github.com/dgutov/diff-hl
;; Показывает небольшие маркеры рядом с незафиксированными изменениями. Дополняет функциональность git-gutter,
;; которые показывает изменения только в обычных буферах. Этот пакет умеет работать с dired и другими режимами.
(use-package diff-hl
  :pin "gnu"
  :ensure t
  :commands (diff-hl-mode diff-hl-dired-mode)
  :config (global-diff-hl-mode 1))


;; 📦 DIMINISH
;; https://github.com/myrjola/diminish.el
;; Позволяет изменить или вовсе скрыть название дополнительного режима с панели статуса.
;; Поддерживается `use-package' с помощью ключевого слова `diminish'.
(use-package diminish
  :pin "gnu"
  :ensure t)


;; 📦 DIRED
;; Встроенный пакет для работы с файлами и каталогами.
;; Клавиши:
;; [+] - создание каталога.
;; [C-x C-f] - создание файла с последующим открытием буфера.
(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t "Удалять буфер при переходе в другой каталог.")
  (dired-listing-switches "-lah --group-directories-first"))


;; 📦 DISPLAY-LINE-NUMBERS-MODE
;; Встроенный пакет
;; Показывает номера строк
(use-package display-line-numbers
  :hook
  ((
     adoc-mode
     c-mode
     conf-mode
     css-mode
     csv-mode
     dockerfile-mode
     emacs-lisp-mode
     json-mode
     latex-mode
     lisp-data-mode
     makefile-mode
     markdown-mode
     nxml-mode
     po-mode
     python-mode
     rst-mode
     ruby-mode
     sh-mode
     shell-script-mode
     terraform-mode
     tex-mode
     web-mode
     yaml-mode
     ) . display-line-numbers-mode))


;; 📦 DOCKERFILE-MODE
;; https://github.com/spotify/dockerfile-mode
;; Работа с файлами `Dockerfile'.
(use-package dockerfile-mode
  :ensure t
  :defer t
  :pin "nongnu")


;; 📦 DOOM-MODELINE
;; https://github.com/seagle0128/doom-modeline
;; Красивая статусная строка
(use-package doom-modeline
  :ensure t
  :pin "melpa-stable"
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
  (doom-modeline-major-mode-color-icon t "Отображение иконки основного режима.")
  (doom-modeline-major-mode-icon t "Отображение иконки основного режима.")
  (doom-modeline-minor-modes t "Отображение списка дополнительных режимов.")
  (doom-modeline-project-detection 'auto "Автоматическое определение проектов.")
  (doom-modeline-total-line-number t "Отображение общего количества строк.")
  (doom-modeline-vcs-max-length 40 "Максимальная длина названия ветки VCS.")
  (doom-modeline-workspace-name t "Отображение названия рабочего пространства.")
  :config
  (doom-modeline-mode 1))


;; 📦 DOOM-THEMES
;; https://github.com/doomemacs/themes
;; Темы из DOOM Emacs
(use-package doom-themes
  :pin "melpa-stable"
  :ensure t
  :custom
  (doom-themes-enable-bold t "Включить поддержку полужирного начертания.")
  (doom-themes-enable-italic t "Включить поддержку наклонного начертания.")
  :config
  (load-theme 'doom-molokai t))


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
  :defer t
  :bind (:map global-map
          ("C-c '" . edit-indirect-region)))


;; 📦 EDITORCONFIG
;; Поддержка https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :pin "nongnu"
  :ensure t
  :diminish ""
  :after (nerd-icons)
  :config (editorconfig-mode 1))


;; 📦 ELDOC-MODE
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Programming-Language-Doc.html
;; Отображение подсказок при работе с Emacs Lisp
(use-package eldoc
  :pin "gnu"
  :ensure t
  :config
  ;; Глобально этот режим не нужен
  (global-eldoc-mode nil)
  :hook
  ;; Включаем только там, где это действительно необходимо
  (emacs-lisp-mode . eldoc-mode)
  (js2-mode . eldoc-mode)
  (python-mode . eldoc-mode))


;; 📦 ELEC-PAIR MODE
;; Встроенный пакет.
;; Автоматически вставляет при вводе одной скобки или кавычки парную ей. Если
;; выделен регион, то в скобки обрамляется он.
(use-package elec-pair
  :ensure nil
  :config
  (add-to-list 'electric-pair-pairs '(?\( . ?\))) ;; ()
  (add-to-list 'electric-pair-pairs '(?\[ . ?\])) ;; []
  (add-to-list 'electric-pair-pairs '(?{ . ?}))   ;; {}
  (add-to-list 'electric-pair-pairs '(?« . ?»))   ;; «»
  (add-to-list 'electric-pair-pairs '(?‘ . ’?))   ;; ‘’
  (add-to-list 'electric-pair-pairs '(?‚ . ‘?))   ;; ‚‘
  (add-to-list 'electric-pair-pairs '(?“ . ”?))   ;; “”
  :hook
  ((
     adoc-mode
     conf-mode
     emacs-lisp-data-mode
     emacs-lisp-mode
     lisp-data-mode
     markdown-mode
     python-mode
     ruby-mode
     ) . electric-pair-local-mode))


;; 📦 ELECTRIC-INDENT MODE
;; Встроенный пакет.
;; Автоматический отступ. В основном только мешает, лучше выключить.
(use-package electric
  :ensure nil
  :config (electric-indent-mode -1)
  :custom (electric-indent-inhibit t "Не выравнивать предыдущую строку по нажатию Enter.")
  :hook (emacs-lisp-mode . electric-indent-local-mode))


;; 📦 EGLOT
;; Пакет для поддержки LSP.
;; https://elpa.gnu.org/packages/eglot.html
;;
;; ПОДГОТОВКА К РАБОТЕ
;; Установка серверов:
;; - Ansible:    sudo npm install -g @ansible/ansible-language-server
;; - Dockerfile: sudo npm -g install dockerfile-language-server-nodejs
;; - HTML:       npm install -g vscode-langservers-extracted
;; - Markdown:   sudo snap install marksman
;; - Python:     pip3 install jedi-language-server
;; - YAML:       sudo npm -g install yaml-language-server
(when (emacs-version-not-less-than 26 3)
  (use-package eglot
    :pin "gnu"
    :ensure t
    :defer t
    :config
    (add-to-list 'eglot-server-programs '(ansible-mode . ("ansible-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs '(dockerfile-mode . ("docker-langserver" "--stdio")))
    (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
    (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
    (add-to-list 'eglot-server-programs '(ruby-mode . ("bundle" "exec" "rubocop" "--lsp")))
    (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server")))
    :hook
    ((
       ansible-mode
       dockerfile-mode
       markdown-mode
       python-mode
       ruby-mode
       yaml-mode
       ) . eglot-ensure)))


;; 📦 EMACS-LISP-MODE
;; IT IS NOT A ELISP-MODE!
;; Встроенный пакет для EMACS Lisp
(use-package elisp-mode
  :ensure nil
  :config
  (setq-local tab-width 2)
  :mode
  ("\\abbrev_defs\\'" . lisp-data-mode)
  ("\\.el\\'" . emacs-lisp-mode))


;; 📦 FILES
;; Это встроенный пакет для управления файлами
(use-package files
  :ensure nil
  :custom
  (auto-save-file-name-transforms `((".*" , init-el-autosave-dir) t))
  (delete-old-versions t "Удалять старые резервные копии файлов без лишних вопросов")
  (enable-local-eval t "Разрешить инструкцию вызов `eval' в `.dir-locals.el'")
  (enable-local-variables t "Считать все переменные из файлов `.dir-locals.el' безопасными")
  (large-file-warning-threshold (* 100 1024 1024) "Предупреждение при открытии файлов больше 100 МБ (по умолчанию — 10 МБ)")
  (make-backup-files nil "Резервные копии не нужны, у нас есть undo-tree")
  (save-abbrevs 'silently "Сохранять аббревиатуры без лишних вопросов")
  :config
  (add-to-list 'safe-local-variable-values '(buffer-env-script-name . ".venv/bin/activate"))
  (add-to-list 'safe-local-variable-values '(electric-pair-preserve-balance . t))
  (add-to-list 'safe-local-variable-values '(emacs-lisp-docstring-fill-column . 70))
  (add-to-list 'safe-local-variable-values '(fill-column . 120))
  (add-to-list 'safe-local-variable-values '(fill-column . 70))
  (add-to-list 'safe-local-variable-values '(frozen_string_literal . true))
  (add-to-list 'safe-local-variable-values '(lexical-binding . t))
  (add-to-list 'safe-local-variable-values '(projectile-project-compilation-cmd . "make dirhtml"))
  (add-to-list 'safe-local-variable-values '(projectile-project-test-cmd . "pre-commit run --all")))


;; 📦 FILL-COLUMN
;; Встроенный пакет.
;; Отображение рекомендуемой границы символов.
(use-package display-fill-column-indicator
  :hook
  (emacs-lisp-mode . display-fill-column-indicator-mode))


;; 📦 FLYCHECK
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


;; 📦 FLYCHECK-EGLOT
;; https://github.com/flycheck/flycheck-eglot
;; Интеграция Flycheck + Eglot
(when (emacs-version-not-less-than 28 1)
  (use-package flycheck-eglot
    :ensure t
    :defer t
    :after (eglot flycheck)))


;; 📦 FLYMAKE
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
    ) . flymake-mode))


;; 📦 FLYSPELL-MODE
;; Встроенный пакет.
;; Проверка орфографии с помощью словарей.
;; Использовать пакет только в том случае, когда дело происходит в Linux и
;; Hunspell или Aspell доступны.
(when init-el-is-linux
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
    (progn
      (message (format "Для проверки орфографии используется %s" text-spell-program))
      (use-package flyspell
        :custom (ispell-program-name text-spell-program)
        :hook
        ((
           adoc-mode
           markdown-mode
           rst-mode) . flyspell-mode)
        (emacs-lisp-mode . flyspell-prog-mode)
        :bind
        (:map global-map
          ([f5] . ispell-buffer))))
    ;; Не найдено программ для проверки орфографии
    (message "Не найдено программ для проверки орфографии.")))


;; 📦 FORMAT-ALL
;; https://github.com/lassik/emacs-format-all-the-code
;; Форматирование кода по нажатию [F12]
(use-package format-all
  :pin "melpa-stable"
  :ensure t
  :defer t
  :bind (:map global-map
              ([f12] . format-all-buffer)))


;; 📦 FRAME
;; Встроенный пакет.
;; Управление фреймами.
(use-package frame
  :custom
  (window-divider-default-places 't "Разделители окон со всех сторон (по умолчанию только справа)")
  (window-divider-default-right-width 3  "Ширина в пикселях для линии-разделителя окон")
  :config
  (window-divider-mode t) ;; Отображать разделитель между окнами
  :bind (:map global-map
          ("C-x O" . previous-multiframe-window) ;; Перейти в предыдущее окно
          ("C-x o" . next-multiframe-window)))   ;; Перейти в следующее окно


;; 📦 GIT-COMMIt
;; https://github.com/magit/magit
;; Специальный режим для правки коммитов при работе с `magit'.
(use-package git-commit
  :pin "nongnu"
  :ensure t)


;; 📦 GIT-GUTTER
;; https://github.com/emacsorphanage/git-gutter
;; Подсветка изменённых строк.
(use-package git-gutter
  :pin "melpa-stable"
  :ensure t
  :diminish ""
  :custom
  (git-gutter:hide-gutter t)
  :config (global-git-gutter-mode 1))


;; 📦 GREP
;; Встроенный пакет.
;; Поиск с помощью `grep'.
(use-package grep
  :bind (:map global-map
          ([f6] . find-grep))) ;; Запуск `find-grep' по нажатию [F6].


;; 📦 HELM
;; https://emacs-helm.github.io/
;; Подсказки и автодополнение ввода.
;; [C-o] — переключение между источниками подсказок (история и полный список команд)
(use-package helm
  :pin "nongnu"
  :ensure t
  :diminish ""
  :config
  (helm-mode 1)
  :bind (:map global-map
          ("M-x" . helm-M-x)))


;; 📦 HL-LINE
;; Встроенный пакет, используемый для подсветки текущей строки.
(use-package hl-line
  :config
  (global-hl-line-mode 1)) ;; Подсветка активной строки


;; 📦 HL-TODO
;; https://github.com/tarsius/hl-todo
;; Подсветка TODO, FIXME и т. п.
(use-package hl-todo
  :pin "melpa-stable"
  :ensure t
  :config (global-hl-todo-mode t))


;; 📦 IBUFFER
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
  (ibuffer-default-sorting-mode 'filename/process "Сортировать файлы по имени / процессу")
  (ibuffer-expert 1 "Не запрашивать подтверждение для опасных операций")
  (ibuffer-truncate-lines nil "Не обкусывать длинные строки")
  (ibuffer-use-other-window t "Открывать буфер *Ibuffer* в отдельном окне")
  :commands ibuffer
  :init
  (defalias 'list-buffers 'ibuffer "Замена стандартной функции на ibuffer.")
  :bind (:map global-map
          ([f2] . ibuffer)))

;; 📦 IBUF-EXT
;; Встроенный пакет.
;; Дополнительные настройки `ibuffer'.
(use-package ibuf-ext
  :custom
  (ibuffer-saved-filter-groups                    ;; Группы по умолчанию
    '(
       ("default"
         ("Dired" (mode . dired-mode))
         ("Emacs Lisp"
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
             (name . "^Makefile$")))
         ("Python"
           (or
             (mode . anaconda-mode)
             (mode . python-mode)))
         ("Ruby" (mode . ruby-mode))
         ("SSH keys" (or (name . "^\\*.pub$")))
         ("Shell-script"
           (or
             (mode . shell-script-mode)
             (mode . sh-mode)))
         ("Terraform" (mode . terraform-mode))
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
  (ibuffer-hidden-filter-groups (list "*Internal*" ) "Не показывать эти буферы")
  (ibuffer-show-empty-filter-groups nil "Не показывать пустые группы")
  :init
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (add-hook 'ibuffer-mode-hook #'(lambda ()(ibuffer-switch-to-saved-filter-groups "default"))))


;; 📦 JS-MODE
;; Встроенный пакет.
;; Базовые настройки при работе с JavaScript.
(use-package js
  :custom
  (js-indent-level 2 "Отступ в 2 пробела, а не 4 (по умолчанию).")
  (js-chain-indent t "Выравнивание при цепочке вызовов через точку.")
  :mode ("\\.js\\'" . js-mode))


;; 📦 JS2-MODE
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :pin "gnu"
  :ensure t
  :defer t
  :mode ("\\.js\\'" . js2-mode))


;; 📦 JSON-MODE
;; Поддержка JSON
(use-package json-mode
  :pin "melpa-stable"
  :ensure t
  :defer t
  :mode ("\\.json\\'" . json-mode))


;; 📦 MAGIT
;; https://magit.vc/
;; Magic + Git + Git-gutter. Лучшее средство для управления Git.
(use-package magit
  :pin "nongnu"
  :ensure t
  :defer t
  :custom
  (magit-define-global-key-bindings t "Включить глобальные сочетания Magit."))


;; 📦 MAKEFILE
;; Встроенный пакет.
;; Поддержка Makefile.
(use-package make-mode
  :ensure nil
  :defer t
  :mode
  ("\\Makefile\\'" . makefile-gmake-mode))


;; 📦 MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
;; Позволяет использовать мультикурсорность.
(use-package multiple-cursors
  :pin "nongnu"
  :ensure t
  :bind (:map global-map
          ("C-S-c C-S-c" . mc/edit-lines)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
          ("C-c C-<" . mc/mark-all-like-this))
  :config
  (add-to-list
    'after-make-frame-functions
    (lambda ()
      (when (display-graphic-p)
        ;; Если режим графический, то курсоры можно расставлять с помощью Alt+Click
        (progn
          (global-unset-key (kbd "M-<down-mouse-1>"))
          (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))))))


;; 📦 NEW-COMMENT
;; Встроенный пакет.
;; Работа с комментариями.
(use-package newcomment
  :bind
  (:map global-map ("M-'" . comment-or-uncomment-region)))


;; 📦 NXML-MODE
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


;; 📦 PACKAGE-LINT
;; https://github.com/purcell/package-lint
;; Проверка пакетов Emacs
(use-package package-lint
  :pin "melpa-stable"
  :ensure t
  :defer t)


;; 📦 PAREN
;; Встроенный режим
;; Управление парными скобками.
(use-package paren
  :config
  (show-paren-mode 1)) ;; Подсвечивать парные скобки


;; 📦 PHP-MODE
;; https://github.com/emacs-php/php-mode
;; Работа с файлами PHP
(use-package php-mode
  :pin "melpa-stable"
  :ensure t
  :mode("\\.php\\'" . php-mode))


;; 📦 PROJECTILE
;; https://docs.projectile.mx/projectile/installation.html
;; Управление проектами. Чтобы каталог считался проектом, он должен быть
;; под контролем любой системы версионирования, либо содержать специальные
;; файлы. В крайнем случае сгодится пустой файл .projectile
;; Подробнее здесь: https://docs.projectile.mx/projectile/projects.html
(use-package projectile
  :pin "nongnu"
  :ensure t
  :diminish "PRJ"
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :config
  (projectile-mode 1))


;; 📦 PULSAR
;; Вспыхивание строки, к которой переместился курсор
;; https://git.sr.ht/~protesilaos/pulsar
(when (emacs-version-not-less-than 27 1)
  ;; Этот пакет требует Emacs версии 27.1 или новее
  (use-package pulsar
    :pin "gnu"
    :ensure t
    :custom (pulsar-pulse t)
    :hook
    (after-init . pulsar-global-mode)
    (next-error . pulsar-pulse-line)
    :config
    (add-to-list 'pulsar-pulse-functions 'ace-window)
    (add-to-list 'pulsar-pulse-functions 'flycheck-next-error)
    (add-to-list 'pulsar-pulse-functions 'flyspell-goto-next-error)
    (add-to-list 'pulsar-pulse-functions 'next-multiframe-window)
    (add-to-list 'pulsar-pulse-functions 'recenter-top-bottom)))


;; 📦 PYTHON-MODE
;; Встроенный пакет для работы с Python
(use-package python-mode
  :pin "melpa-stable"
  :ensure t
  :custom
  (py-pylint-command-args "--max-line-length 120" "Дополнительные параметры, передаваемые pylint")
  (setq python-indent-offset 4))


;; 📦 RAINBOW-DELIMITERS-MODE
;; https://github.com/Fanael/rainbow-delimiters
;; Подсветка парных скобок одним и тем же цветом
(use-package rainbow-delimiters
  :pin "nongnu"
  :ensure t
  :diminish ""
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


;; 📦 RAINBOW-MODE
;; https://elpa.gnu.org/packages/rainbow-mode.html
;; Подсветка строк с цветами нужным цветом, например #153415, #223956
(use-package rainbow-mode
  :pin "gnu"
  :ensure t
  :diminish ""
  :hook
  ((
     css-mode
     emacs-lisp-mode
     web-mode
     ) . rainbow-mode))


;; 📦 REPLACE
;; Встроенный пакет.
;; Функции поиска и замены текста.
(use-package replace
  :bind
  (:map global-map ([f3] . replace-string)))


;; 📦 REVERSE-IM
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
  :config (reverse-im-mode 1))


;; 📦 RUSSIAN-TECHWRITER
;; Метод ввода для технических писателей
;; https://github.com/dunmaksim/emacs-russian-techwriter-input-method
(use-package russian-techwriter
  :pin "melpa-stable"
  :ensure t
  :custom
  (default-input-method 'russian-techwriter))


;; 📦 SAVEPLACE
;; Встроенный пакет.
;; Запоминание позиции курсора в посещённых файлах.
(use-package saveplace
  :custom
  (save-place-forget-unreadable-files t "Не запоминать положение в нечитаемых файлах.")
  (save-place-file (expand-file-name ".emacs-places" init-el-config-dir))
  :config
  (save-place-mode 1)) ;; Помнить позицию курсора


;; 📦 RUBY-MODE
;; Встроенный пакет
(use-package ruby-mode
  :ensure nil
  :defer t
  :init
  (defvar ruby-indent-offset 2 "Ширина TAB'а в `ruby-mode'.")
  :mode
  ("\\Vagrantfile\\'"
    "\\.rb\\'"))


;; 📦 SAVE-HIST
;; Встроенный пакет.
;; Запоминает историю введенных команд
(use-package savehist
  :config
  (savehist-mode 1))


;; 📦 SHELL-SCRIPT-MODE
;; Встроенный пакет.
;; Работа со скриптами Shell.
(use-package sh-script
  :ensure nil
  :defer t
  :mode
  ("\\.bashrc\\'" . shell-script-mode)
  ("\\.profile\\'" . shell-script-mode)
  ("\\.sh\\'" . shell-script-mode))


;; 📦 SIMPLE
;; Встроенный пакет.
;; Разные настройки управления элементарным редактированием текста.
(use-package simple
  :custom
  (backward-delete-char-untabify-method 'hungry "Удалять все символы выравнивания при нажатии [Backspace]")
  (blink-matching-paren t "Мигать, когда скобки парные")
  (overwrite-mode-binary nil "Выключить режим перезаписи текста под курсором для бинарных файлов")
  (overwrite-mode-textual nil "Выключить режим перезаписи текста под курсором для текстовых файлов")
  (suggest-key-bindings t "Показывать подсказку клавиатурной комбинации для команды")
  :config
  (column-number-mode 1)      ;; Показывать номер колонки в статусной строке
  (global-visual-line-mode 1) ;; Деление логических строк на видимые
  (line-number-mode t)        ;; Показывать номер строки в статусной строке
  (overwrite-mode 0)          ;; Отключить режим перезаписи текста
  (size-indication-mode 0)    ;; Отображать размер буфера в строке статуса
  :bind
  (:map global-map
    ("<escape>" . keyboard-quit)   ;; ESC работает как и Ctrl+g, т. е. прерывает ввод команды
    ("C-z" . undo)                 ;; Отмена
    ("S-<SPC>" . just-one-space))) ;; Заменить пробелы и TAB'ы до и после курсора на один пробел


;; 📦 SORT
;; Встроенный пакет.
(use-package sort
  :bind (:map global-map ([f9] . sort-lines)))


;; 📦 TERRAFORM-MODE
;; https://github.com/emacsorphanage/terraform-mode
;; Работа с файлами конфигурации Terraform
(use-package terraform-mode
  :pin "melpa-stable"
  :ensure t
  :defer t
  :mode
  ("\\.terraformrc\\'" . terraform-mode)
  ("\\.tf\\'" . terraform-mode))


;; 📦 TOOLTIP
;; Встроенный пакет для вывода подсказок в графической среде
(use-package tooltip
  :custom
  (tooltip-mode nil "Отключить показ подсказок с помощью GUI")
  :config
  ;; Отключить показ подсказок с помощью GUI
  (tooltip-mode -1))


;; 📦 TYPO
;; https://git.sr.ht/~pkal/typo/
;; Автодополнение на основе анализа ввода
(when (emacs-version-not-less-than 27 1)
  (use-package typo
    :pin "gnu"
    :ensure t
    :init
    (add-to-list 'completion-styles 'typo t)))


;; 📦 UNIQUIFY
;; Встроенный пакет.
;; Используется для поддержания уникальности названий буферов, путей и т. д.
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward "Показывать каталог перед именем файла, если буферы одинаковые (по умолчанию имя<каталог>)")
  (uniquify-separator "/" "Разделять буферы с похожими именами, используя /"))


;; 📦 UNDO-TREE
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


;; 📦 VERTICO
;; https://github.com/minad/vertico
;; Пакет для какого-то автодополнения.
;; TODO: понять, надо ли оно, и чем оно лучше `company-mode'.
(use-package vertico
  :pin "gnu"
  :ensure t
  :config
  (vertico-mode 1))


;; 📦 WHICH-KEY MODE
;; https://github.com/justbur/emacs-which-key
;; Показывает подсказки к сочетаниям клавиш.
(use-package which-key
  :pin "gnu"
  :ensure t
  :diminish ""
  :custom
  (which-key-idle-delay 2 "Задержка появления подсказки")
  (which-key-idle-secondary-delay 0.05 "Ещё одна задержка появления подсказки")
  :config
  (which-key-mode 1)
  (which-key-setup-minibuffer)
  (which-key-setup-side-window-right)) ;; Показывать подсказки справа


;; 📦 WHITESPACE MODE
;; Встроенный пакет.
;; Отображение невидимых символов.
(use-package whitespace
  :diminish "ws"
  :custom
  (whitespace-display-mappings ;; Отображение нечитаемых символов
    '(
       (space-mark   ?\    [?\xB7]     [?.])      ;; Пробел
       (space-mark   ?\xA0 [?\xA4]     [?_])      ;; Неразрывный пробел
       (newline-mark ?\n   [?¶ ?\n]    [?$ ?\n])  ;; Конец строки
       (tab-mark     ?\t   [?\xBB ?\t] [?\\ ?\t]) ;; TAB
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
     po-mode
     python-mode
     rst-mode
     ruby-mode
     sh-mode
     sql-mode
     terraform-mode
     tex-mode
     web-mode
     yaml-mode) . whitespace-mode))


;; 📦 WINDMOVE
;; Встроенный пакет для перемещения между окнами Emacs
(use-package windmove
  :bind
  (:map global-map
    ("C-x <up>" . windmove-up)
    ("C-x <down>" . windmove-down)))


;; 📦 WINNER-MODE
;; Встроенный пакет для управления окнами.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Convenience.html
;; Для управления конфигурациями окон используются последовательности
;; [C-c <left>] и [C-c <right>]
(use-package winner
  :config
  (winner-mode 1))


;; 📦 WINDOW
;; Встроенный пакет, отвечает за управление размерами окон
(use-package window
  :bind
  (:map global-map
    ("S-C-<left>" . shrink-window-horizontally)   ;; [Ctrl+Shift+←]   Уменьшить размер окна по ширине
    ("S-C-<right>" . enlarge-window-horizontally) ;; [Ctrl+Shift+→]   Увеличить размер окна по ширине
    ("S-C-<down>" . enlarge-window)               ;; [Ctrl+Shift+↓]   Увеличить размер окна по ширине
    ("S-C-<up>" . shrink-window)                  ;; [Ctrl+Shift+↑]   Уменьшить размер окна по высоте
    ([C-S-iso-lefttab] . next-buffer)             ;; [Ctrl+Tab]       Вернуться в предыдущий буфер
    ([C-tab] . previous-buffer)))                 ;; [Ctrl+Shift+Tab] Следующий буфер


;; 📦 YAML-MODE
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

;; 📦 CUSTOM FILE
;; Пользовательские настройки, сделанные через CUSTOMIZE
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init.el)
;;; init.el ends here
