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

(defconst init-el-font-height 16 "Размер шрифта по умолчанию.")

(require 'custom)

;; Если используется старая версия EMACS, нужно указать параметры протокола TLS.
;; В противном случае будут проблемы при загрузке архива пакетов.
(when (< emacs-major-version 27)
  (require 'gnutls)
  (customize-set-variable
   'gnutls-algorithm-priority
   "NORMAL:-VERS-TLS1.3"
   "Необходимо для старых версий Emacs."))

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
 '(standard-indent 4 "Отступ по умолчанию")
 '(tab-always-indent 'complete "Если можно — выровнять текст, иначе — автодополнение.")
 '(truncate-lines 1 "Обрезать длинные строки")
 '(use-dialog-box nil "Диалоговые окна ОС не нужны")
 '(user-full-name "Dunaevsky Maxim" "Имя пользователя")
 '(user-mail-address "dunmaksim@yandex.ru" "Адрес электронной почты")
 '(vc-follow-symlinks t "Переходить по ссылкам без лишних вопросов")
 '(visible-bell t "Мигать буфером при переходе в него"))


(global-unset-key (kbd "<insert>")) ;; Режим перезаписи не нужен
(global-unset-key (kbd "M-,"))      ;; Такие маркеры не нужны
(global-unset-key (kbd "C-z"))      ;; Такой Ctrl+Z нам не нужен
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
          (customize-set-variable 'source-directory init-el-emacs-source-path)
          (message (format "Исходный код обнаружен в каталоге %s" init-el-emacs-source-path))))
    ;; Каталог не существует
    (message (format "Каталог %s не существует." init-el-emacs-source-path))))


;; 📦 Straight.el
;; https://github.com/radian-software/straight.el
;; Пакет для более строгого управления пакетами
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 📦 USE-PACKAGE
;; https://elpa.gnu.org/packages/use-package.html
(straight-use-package '(use-package :ref "2.4.5"))

;; Настройки отладочного режима
(when init-file-debug
  (custom-set-variables
   '(debug-on-error t "Автоматически перейти в режим отладки при ошибках.")
   '(use-package-compute-statistics t "Сбор статистики `use-package'.")
   '(use-package-expand-minimally t "TODO: ???")
   '(use-package-verbose t "Подробный режим работы `use-package'.")))


;; 📦 DELIGHT
;; https://elpa.gnu.org/packages/delight.html
;; Позволяет спрятать из панели статуса лишние названия режимов.
;; Эти строки находятся здесь потому, что `use-package' активно
;; использует возможности этого пакета далее, поэтому он должен быть
;; загружен как можно раньше.
(use-package delight
  :straight (delight
             :host nil
             :type git
             :repo "https://git.savannah.gnu.org/git/delight.git"
             :ref "1.7"))


;; 📦 Настройки, специфичные для графического режима
(defun setup-gui-settings (frame-name)
  "Настройки, необходимые при запуске EMACS в графической среде.

  FRAME-NAME — имя фрейма, который настраивается."
  (when (display-graphic-p frame-name)
    (global-font-lock-mode t)  ;; Отображать шрифты красиво, используя Font Face's

    (defvar availiable-fonts (font-family-list)) ;; Какие есть семейства шрифтов?
    (defvar default-font-family nil "Шрифт по умолчанию.")

    ;; Перебор шрифтов
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


;; 📦 ABBREV-MODE
;; Встроенный пакет.
;; Использование аббревиатур -- фрагментов текста, которые при вводе
;; определённой последовательности символов заменяются на другую,
;; например:
;; tf → Terraform
;; yc → Yandex Cloud
;; Это встроенный пакет
(use-package abbrev
  :defer t
  :delight "abb")


;; 📦 ACE-WINDOW
;; https://github.com/abo-abo/ace-window
;; Быстрое переключение между окнами по M+o
(use-package ace-window
  :straight '(ace-window
              :host github
              :repo "abo-abo/ace-window"
              :ref "0.10.0")
  :bind (:map global-map
              ("M-o" . ace-window)))


;; 📦 ACTIVITIES
;; https://elpa.gnu.org/packages/activities.html
;; Управление наборами окон, вкладок, фреймов и буферов
(use-package activities
  :straight '(activities
              :ref "0.7.1")
  :config
  (activities-mode 1)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))


;; 📦 ADJUST-PARENS
;; https://elpa.gnu.org/packages/adjust-parens.html
;; Пакет для автоматического управления скобочками и уровнями отступов.
(use-package adjust-parens
  :straight (adjust-parens :ref "3.2")
  :hook (emacs-lisp-mode . adjust-parens-mode)
  :bind (:map emacs-lisp-mode-map
              ("<tab>" . lisp-indent-adjust-parens)
              ("<backtab>" . lisp-dedent-adjust-parens)))


;; 📦 ADOC-MODE
;; https://github.com/bbatsov/adoc-mode
;; Работа с AsciiDoc
(use-package adoc-mode
  :straight (adoc-mode
             :host github
             :repo "bbatsov/adoc-mode")
  :defer t
  :custom
  (adoc-fontify-code-blocks-natively 10000)
  :mode ("\\.adoc\\'" . adoc-mode))


;; 📦 AGGRESSIVE-INDENT
;; https://github.com/Malabarba/aggressive-indent-mode
;; Принудительное выравнивание кода
(use-package aggressive-indent
  :straight (aggressive-indent
             :host github
             :repo "Malabarba/aggressive-indent-mode"
             :ref "1.10.0")
  :defer t
  :hook
  ((emacs-lisp-mode
    js2-mode
    json-mode
    latex-mode
    lisp-data-mode
    nxml-mode
    sh-mode
    sql-mode
    ) . aggressive-indent-mode))


;; 📦 ANACONDA-MODE
;; https://github.com/pythonic-emacs/anaconda-mode
;; Расширенная поддержка Python.
(use-package anaconda-mode
  :straight (anaconda-mode
             :host github
             :repo "pythonic-emacs/anaconda-mode"
             :ref "v0.1.16")
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))


;; 📦 ANSIBLE
;; https://gitlab.com/emacs-ansible/emacs-ansible
;; Дополнительные возможности при работе с YAML-файлами Ansible
(use-package ansible
  :straight (ansible
             :host gitlab
             :repo "emacs-ansible/emacs-ansible"
             :ref "0.3.2")
  :defer t)


;; 📦 ANZU
;; https://github.com/emacsorphanage/anzu
;; Подсказки о количестве совпадений при поиске с помощью `isearch'.
(use-package anzu
  :straight (anzu
             :host github
             :repo "emacsorphanage/anzu"
             :ref "0.64")
  :delight ""
  :config
  (global-anzu-mode 1))


;; 📦 APHELEIA
;; https://github.com/radian-software/apheleia
;; Форматирование содержимого буфера с помощью внешних средств
(use-package apheleia
  :straight (apheleia
             :host github
             :repo "radian-software/apheleia"
             :ref "v4.1")
  :delight "")


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


;; 📦 BBCODE-MODE
;; https://github.com/lassik/emacs-bbcode-mode
;; Режим редактирования BB-кодов
(use-package bbcode-mode
  :straight (bbcode-mode
             :host github
             :repo "lassik/emacs-bbcode-mode"
             :ref "v2.3.0")
  :defer t)


;; 📦 BUFFER-ENV
;; https://github.com/astoff/buffer-env
;; Настройка окружения отдельно для каждого буфера.
;; Настройки загружаются из файла `.env' в каталоге проекта или `.dir-locals.el'.
;; Во первом случае в файле должна быть указана команда для активации окружения, например:
;; source .venv/bin/activate
;; Во втором достаточно задать значение переменной `buffer-env-script-name'.
(when (emacs-version-not-less-than 27 1)
  (use-package buffer-env
    :straight (buffer-env
               :host github
               :repo "astoff/buffer-env")
    :defer t
    :after (files)
    :hook
    ((
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
  :custom
  (checkdoc-minor-mode-string " CheckDoc")
  :hook (emacs-lisp-mode . checkdoc-minor-mode))


;; 📦 COMPANY-MODE
;; https://company-mode.github.io/
;; Автодополнение
(use-package company
  :straight (company
             :host github
             :repo "company-mode/company-mode"
             :ref "0.10.2")
  :delight ""
  :custom
  (company-idle-delay 0.5 "Задержка вывода подсказки — полсекунды")
  (company-minimum-prefix-length 2 "Минимум 2 знака, чтобы company начала работать")
  (company-show-quick-access t "Показывать номера возле потенциальных кандидатов")
  (company-tooltip-align-annotations t "Выровнять текст подсказки по правому краю")
  (company-tooltip-limit 15 "Ограничение на число подсказок")
  :hook
  ((css-mode
    dockerfile-mode
    emacs-lisp-mode
    js2-mode
    latex-mode
    lisp-data-mode
    minibufer-mode
    nxml-mode
    org-mode
    python-mode
    rst-mode
    ruby-mode
    ) . company-mode)
  :bind
  (:map company-active-map
        ("TAB" . company-complete-common-or-cycle)
        ("M-/" . company-complete)
        ("M-." . company-show-location)))


;; 📦 COMPANY-ANACONDA
;; https://github.com/pythonic-emacs/company-anaconda
;; Интеграция Anaconda + Company
(use-package company-anaconda
  :straight (company-anaconda
             :host github
             :repo "pythonic-emacs/company-anaconda"
             :ref "v0.2.0")
  :requires (anaconda-mode company)
  :config
  (add-to-list 'company-backends 'company-anaconda))


;; 📦 COMPANY-ANSIBLE
;; https://github.com/krzysztof-magosa/company-ansible
;; Автодополнение Company в Ansible
(use-package company-ansible
  :straight company-ansible
  :after company
  :requires (company)
  :defer t
  :config
  (add-to-list 'company-backends 'company-ansible))


;; 📦 CONF-MODE
;; Встроенный пакет.
;; Основной режим для редактирования конфигурационных файлов INI/CONF
(use-package conf-mode
  :defer t
  :mode
  ("\\.env\\'"
   "\\.flake8\\'"
   "\\.ini\\'"
   "\\.pylintrc\\'"))


;; 📦 CSS-MODE
;; Встроенный пакет.
;; Поддержка CSS.
(use-package css-mode
  :defer t
  :custom
  (css-indent-offset 2)
  :mode "\\.css\\'")


;; 📦 CSV-MODE
;; https://elpa.gnu.org/packages/csv-mode.html
;; Поддержка CSV
(use-package csv-mode
  :straight (csv-mode
             :ref "1.26")
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


;; 📦 DENOTE
;; https://protesilaos.com/emacs/denote
;; Режим для управления заметками
(when (emacs-version-not-less-than 28 1)
  (use-package denote
    :straight (denote
               :host github
               :repo "protesilaos/denote"
               :ref "3.0.6")
    :ensure t
    :custom
    (denote-directory "~/Документы/Notes/" "Каталог для хранения заметок.")))


;; 📦 DESKTOP
;; Встроенный пакет.
;; Сохранение состояния Emacs между сессиями.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(use-package desktop
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
  :straight (diff-hl
             :host github
             :repo "dgutov/diff-hl"
             :ref "1.9.2")
  :commands (diff-hl-mode diff-hl-dired-mode)
  :config (global-diff-hl-mode 1))


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
  ((adoc-mode
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
  :straight (dockerfile-mode
             :host github
             :repo "spotify/dockerfile-mode"
             :ref "v1.9")
  :defer t
  :mode
  ("\\Dockerfile\\'" . dockerfile-mode))


;; 📦 DOOM-MODELINE
;; https://github.com/seagle0128/doom-modeline
;; Красивая статусная строка
(use-package doom-modeline
  :straight (doom-modeline
             :host github
             :repo "seagle0128/doom-modeline"
             :ref "v4.1.0")
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
  :straight (doom-themes
             :host github
             :repo "doomemacs/themes"
             :ref "v2.3.0")
  :custom
  (doom-themes-enable-bold t "Включить поддержку полужирного начертания.")
  (doom-themes-enable-italic t "Включить поддержку наклонного начертания.")
  :config
  (load-theme 'doom-monokai-classic t))


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
  :straight (edit-indirect
             :host github
             :repo "Fanael/edit-indirect"
             :ref "0.1.13")
  :defer t
  :bind (:map global-map
              ("C-c '" . edit-indirect-region)))


;; 📦 EDITORCONFIG
;; Поддержка https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :straight (editorconfig
             :host github
             :repo "editorconfig/editorconfig-emacs"
             :ref "v0.11.0")
  :delight ""
  :config
  (editorconfig-mode 1)
  :mode
  ("\\.editorconfig\\'" . editorconfig-conf-mode))


;; 📦 ELDOC-MODE
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Programming-Language-Doc.html
;; Отображение подсказок при работе с Emacs Lisp
(use-package eldoc
  :config
  ;; Глобально этот режим не нужен
  (global-eldoc-mode nil)
  :delight ""
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
  :config
  (add-to-list 'electric-pair-pairs '(?\( . ?\))) ;; ()
  (add-to-list 'electric-pair-pairs '(?\[ . ?\])) ;; []
  (add-to-list 'electric-pair-pairs '(?{ . ?}))   ;; {}
  (add-to-list 'electric-pair-pairs '(?« . ?»))   ;; «»
  (add-to-list 'electric-pair-pairs '(?‘ . ’?))   ;; ‘’
  (add-to-list 'electric-pair-pairs '(?‚ . ‘?))   ;; ‚‘
  (add-to-list 'electric-pair-pairs '(?“ . ”?))   ;; “”
  :hook
  ((adoc-mode
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
    :straight (eglot
               :host github
               :repo "joaotavora/eglot"
               :ref "1.17")
    :defer t
    :config
    (add-to-list 'eglot-server-programs '(ansible-mode . ("ansible-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs '(dockerfile-mode . ("docker-langserver" "--stdio")))
    (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
    (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
    (add-to-list 'eglot-server-programs '(ruby-mode . ("bundle" "exec" "rubocop" "--lsp")))
    (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server")))
    :hook
    ((ansible-mode
      dockerfile-mode
      markdown-mode
      python-mode
      ruby-mode
      ) . eglot-ensure)))


;; 📦 EMACS-LISP-MODE
;; IT IS NOT A ELISP-MODE!
;; Встроенный пакет для EMACS Lisp
(use-package elisp-mode
  :config
  (setq-local tab-width 2)
  :mode
  ("\\abbrev_defs\\'" . lisp-data-mode)
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
;; https://www.flycheck.org/
;; Проверка синтаксиса на лету с помощью статических анализаторов
(use-package flycheck
  :straight (flycheck
             :host github
             :repo "flycheck/flycheck"
             :ref "34.1")
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


;; 📦 FLYLISP
;; https://elpa.gnu.org/packages/flylisp.html
;; Подсвекта непарных или неправильно выровненных скобок
(use-package flylisp
  :straight (flylisp
             :ref "0.2")
  :hook
  (emacs-lisp-mode . flylisp-mode))


;; 📦 FLYMAKE
;; Более свежая версия встроенного пакета из репозитория gnu
;; Используется для проверки `init.el'.
;; https://elpa.gnu.org/packages/flymake.html
(use-package flymake
  :straight (flymake
             :ref "1.3.7")
  :hook
  ((emacs-lisp-mode
    lisp-data-mode
    ) . flymake-mode))


;; 📦 FLYSPELL-MODE
;; Встроенный пакет.
;; Проверка орфографии с помощью словарей.
;; Использовать пакет только в том случае, когда дело происходит в
;; Linux и Hunspell или Aspell доступны.
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
          ((adoc-mode
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
;; Форматирование кода с помощью разных внешних средств.
(use-package format-all
  :straight (format-all
             :host github
             :repo "lassik/emacs-format-all-the-code"
             :ref "0.6.0")
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


;; 📦 GIT-GUTTER
;; https://github.com/emacsorphanage/git-gutter
;; Подсветка изменённых строк.
(use-package git-gutter
  :straight (git-gutter
             :host github
             :repo "emacsorphanage/git-gutter"
             :ref "0.92")
  :delight ""
  :custom
  (git-gutter:hide-gutter t)
  :hook
  ((
    adoc-mode
    emacs-lisp-mode
    markdown-mode
    rst-mode
    yaml-mode
    ) . git-gutter-mode))


;; 📦 GOTO-ADDRESS-MODE
;; Встроенный пакет.
;; Подсвечивает ссылки и позволяет переходить по ним с помощью [C-c RET].
;; Возможны варианты (зависит от основного режима).
(use-package goto-addr
  :hook
  ((
    adoc-mode
    emacs-lisp-mode
    markdown-mode
    rst-mode
    text-mode
    web-mode
    ) . goto-address-mode))


;; 📦 GREP
;; Встроенный пакет.
;; Поиск с помощью `grep'.
(use-package grep
  :bind (:map global-map
              ([f6] . find-grep))) ;; Запуск `find-grep' по нажатию [F6].


;; 📦 HELM
;; https://github.com/emacs-helm/helm
;; Подсказки и автодополнение ввода.
;; [C-o] — переключение между источниками подсказок (история и полный список команд)
(use-package helm
  :straight (helm
             :host github
             :repo "emacs-helm/helm"
             :ref "v3.9.9")
  :delight ""
  :config
  (helm-mode 1)
  :bind (:map global-map
              ("C-x C-f" . helm-find-files)
              ("C-x b" . helm-buffers-list)
              ("M-x" . helm-M-x)
              ("M-y" . helm-show-kill-ring)))


;; 📦 HELM-PROJECTILE
;; https://github.com/bbatsov/helm-projectile
;; Интеграция HELM с PROJECTILE
(use-package helm-projectile
  :straight (helm-projectile
             :host github
             :repo "bbatsov/helm-projectile"
             :ref "v1.0.0")
  :delight ""
  :requires (helm projectile)
  :after (helm projectile)
  :config
  (helm-projectile-on))


;; 📦 HL-LINE
;; Встроенный пакет.
;; Подсветка текущей строки.
(use-package hl-line
  :config
  (global-hl-line-mode 1)) ;; Подсветка активной строки


;; 📦 HL-TODO
;; https://github.com/tarsius/hl-todo
;; Подсветка TODO, FIXME и т. п.
(use-package hl-todo
  :straight (hl-todo
             :host github
             :repo "tarsius/hl-todo"
             :ref "v3.7.0")
  :config (global-hl-todo-mode t))


;; 📦 IBUFFER
;; Встроенный пакет для удобной работы с буферами.
;; По нажатию F2 выводит список открытых буферов.
;; Взято из конфига автора пакета
;; https://github.com/jwiegley/dot-emacs/blob/master/init.org
(use-package ibuffer
  :custom
  (ibuffer-formats ;; Форматирование вывода
   '((;; Полный формат
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
   '(("default"
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
  :straight (js2-mode
             :host github
             :repo "mooz/js2-mode")
  :defer t
  :mode ("\\.js\\'" . js2-mode))


;; 📦 JSON-MODE
;; https://github.com/json-emacs/json-mode
;; Поддержка JSON
(use-package json-mode
  :straight (json-mode
             :host github
             :repo "json-emacs/json-mode"
             :ref "v1.9.2")
  :defer t
  :mode ("\\.json\\'" . json-mode))


;; 📦 LSP-MODE
;; https://github.com/emacs-lsp/lsp-mode
;; https://emacs-lsp.github.io/lsp-mode/
;; Альтернативный LSP-сервер
(use-package lsp-mode
  :straight (lsp-mode
             :ref "9.0.0")
  :custom
  (lsp-keymap-prefix "C-c l")
  :commands lsp
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (python-mode . lsp)
  (yaml-mode . lsp))


;; 📦 LSP-UI
;; https://github.com/emacs-lsp/lsp-ui
;; Расширение для красивостей LSP-MODE
(use-package lsp-ui
  :straight (lsp-ui
             :ref "9.0.0")
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode))


;; 📦 MAGIT
;; https://magit.vc/
;; Magic + Git + Git-gutter. Лучшее средство для управления Git.
(use-package magit
  :straight (magit
             :host github
             :repo "magit/magit"
             :ref "v3.3.0")
  :custom
  (magit-define-global-key-bindings t "Включить глобальные сочетания Magit.")
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))


;; 📦 MAGIT-FILE-ICONS
;; https://github.com/gekoke/magit-file-icons
;; Иконки в буферах Magit
(use-package magit-file-icons
  :straight (magit-file-icons
             :host github
             :repo "gekoke/magit-file-icons"
             :ref "v2.0.0")
  :after magit
  :config
  (magit-file-icons-mode 1))


;; 📦 MAKEFILE
;; Встроенный пакет.
;; Поддержка Makefile.
(use-package make-mode
  :defer t
  :mode
  ("\\Makefile\\'" . makefile-gmake-mode))


;; 📦 MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
;; Режим для работы с файлами в формате Markdown
(when (emacs-version-not-less-than 27 1)
  (use-package markdown-mode
    :straight (markdown-mode
               :host github
               :repo "jrblevin/markdown-mode"
               :ref "v2.6")
    :defer t
    :after tree-sitter
    :custom
    (markdown-fontify-code-blocks-natively t "Подсвечивать синтаксис в примерах кода")
    (markdown-header-scaling-values '(1.0 1.0 1.0 1.0 1.0 1.0) "Все заголовки одной высоты")
    (markdown-list-indent-width 4 "Размер отступа для выравнивания вложенных списков")
    :config (setq-local word-wrap t)
    :bind (
           :map markdown-mode-map
           ("M-." . markdown-follow-thing-at-point))
    :mode ("\\.md\\'" . markdown-mode)))


;; 📦 MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
;; Позволяет использовать мультикурсорность.
(use-package multiple-cursors
  :straight (multiple-cursors
             :host github
             :repo "magnars/multiple-cursors.el")
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


;; 📦 NERD-ICONS
;; https://github.com/rainstormstudio/nerd-icons.el
;; Требуется для корректной работы `doom-modeline'.
;; Начиная с версии 4.0.0 пакет `all-the-icons' не поддерживается.
;;
;; Для установки самих шрифтов следует использовать команду `nerd-icons-install-fonts'.
;; В Debian Linux шрифты будут загружены в каталог `~/.local/share/fonts'. Рекомендуется
;; скопировать их в `/usr/local/share/fonts/'.
(use-package nerd-icons
  :straight (nerd-icons
             :host github
             :repo "rainstormstudio/nerd-icons.el")
  :delight ""
  :custom
  (nerd-icons-color-icons t "Использовать цветные иконки."))


;; 📦 NERD-ICONS-DIRED
;; https://github.com/rainstormstudio/nerd-icons-dired
;; Иконки в `dired'.
(use-package nerd-icons-dired
  :straight (nerd-icons-dired
             :host github
             :repo "rainstormstudio/nerd-icons-dired")
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))


;; 📦 NERD-ICONS-IBUFFER
;; https://github.com/seagle0128/nerd-icons-ibuffer
;; Отображение иконок в ibuffer
(use-package nerd-icons-ibuffer
  :straight (nerd-icons-ibuffer
             :host github
             :repo "seagle0128/nerd-icons-ibuffer")
  :after (ibuffer nerd-icons)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; 📦 NEW-COMMENT
;; Встроенный пакет.
;; Работа с комментариями.
(use-package newcomment
  :bind
  (:map global-map ("M-'" . comment-or-uncomment-region)))


;; 📦 NXML-MODE
;; Встроенный пакет.
;; Почти как `xml-mode', только лучше и новее (ну вы поняли...)
(use-package nxml-mode
  :defer t
  :custom
  (nxml-attribute-indent 4 "Выравнивание атрибутов")
  (nxml-auto-insert-xml-declaration-flag nil "Не вставлять декларацию")
  (nxml-bind-meta-tab-to-complete-flag t "Использовать TAB для завершения ввода")
  (nxml-child-indent 4 "Выравнивание дочерних элементов")
  (nxml-slash-auto-complete-flag t "Закрывать теги по вводу /")
  :commands nxml-mode
  :mode
  (
   "\\.pom\\'"
   "\\.xml\\'"))


;; 📦 ORG-MODE
;; https://orgmode.org/
;; Органайзер, заметки и так далее
(use-package org
  :straight (org
             :ref "9.7.9")
  :defer t
  :config
  (setq-local
   truncate-lines nil ;; Не обрезать строки
   word-wrap t))      ;; Перенос длинных строк


;; 📦 PACKAGE-LINT
;; https://github.com/purcell/package-lint
;; Проверка пакетов Emacs
(use-package package-lint
  :straight (package-lint
             :host github
             :repo "purcell/package-lint"
             :ref "0.23")
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
  :straight (php-mode
             :host github
             :repo "emacs-php/php-mode"
             :ref "v1.25.1")
  :mode("\\.php\\'" . php-mode))


;; 📦 PO-MODE
;; https://www.gnu.org/software/gettext/manual/html_node/Installation.html
;; Работа с файлами локализации.
;; Необходимо установить в систему утилиты из набора gettext, иначе
;; работать не будет.
(use-package po-mode
  :straight po-mode
  :defer t
  :mode
  ("\\.po\\'\\|\\.po\\." . po-mode))


;; 📦 PROJECTILE
;; https://docs.projectile.mx/projectile/installation.html
;; Управление проектами. Чтобы каталог считался проектом, он должен быть
;; под контролем любой системы версионирования, либо содержать специальные
;; файлы. В крайнем случае сгодится пустой файл .projectile
;; Подробнее здесь: https://docs.projectile.mx/projectile/projects.html
(use-package projectile
  :straight (projectile
             :host github
             :repo "bbatsov/projectile"
             :ref "v2.8.0")
  :delight ""
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :config
  (projectile-mode 1))


;; 📦 PULSAR
;; Вспыхивание строки, к которой переместился курсор
;; https://github.com/protesilaos/pulsar
(when (emacs-version-not-less-than 27 1)
  ;; Этот пакет требует Emacs версии 27.1 или новее
  (use-package pulsar
    :straight (pulsar
               :host github
               :repo "protesilaos/pulsar"
               :ref "1.0.0")
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
  :straight (python-mode
             :host github
             :repo "emacsmirror/python-mode")
  :custom
  (py-pylint-command-args "--max-line-length 120" "Дополнительные параметры, передаваемые pylint")
  (python-indent-guess-indent-offset-verbose nil "Выключить уведомления")
  (python-indent-offset 4 "Отсуп по умолчанию — 4 пробела"))


;; 📦 RAINBOW-DELIMITERS-MODE
;; https://github.com/Fanael/rainbow-delimiters
;; Подсветка парных скобок одним и тем же цветом
(use-package rainbow-delimiters
  :straight (rainbow-delimiters
             :host github
             :repo "Fanael/rainbow-delimiters"
             :ref "2.1.5")
  :delight ""
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
  :straight (rainbow-mode
             :ref "1.0.6")
  :delight ""
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
  :straight (reverse-im
             :host github
             :repo "a13/reverse-im.el"
             :ref "v0.0.8")
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
  :straight (russian-techwriter
             :host github
             :repo "dunmaksim/emacs-russian-techwriter-input-method")
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


;; 📦 RST-MODE
;; Встроенный пакет.
;; Основной режим для редактирования reStructutedText
;; https://www.writethedocs.org/guide/writing/reStructuredText/
(use-package rst
  :defer t
  :custom
  (rst-default-indent 3)
  (rst-indent-comment 3)
  (rst-indent-field 3)
  (rst-indent-literal-minimized 3)
  (rst-indent-width 3)
  (rst-toc-indent 3)
  :mode
  (
   ("\\.rst\\'" . rst-mode)
   ("\\.txt\\'" . rst-mode)))


;; 📦 RUBY-MODE
;; Встроенный пакет
(use-package ruby-mode
  :defer t
  :init
  (defvar ruby-indent-offset 2 "Ширина TAB'а в `ruby-mode'.")
  :mode
  (
   "\\Vagrantfile\\'"
   "\\.rb\\'"
   ))


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
  :defer t
  :mode
  ("\\.bashrc\\'" . shell-script-mode)
  ("\\.envrc\\'" . shell-script-mode)
  ("\\.profile\\'" . shell-script-mode)
  ("\\.sh\\'" . shell-script-mode))


;; 📦 SIMPLE
;; Встроенный пакет.
;; Разные настройки управления элементарным редактированием текста.
(use-package simple
  :delight (visual-line-mode)
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
;; https://github.com/hcl-emacs/terraform-mode
;; Работа с файлами конфигурации Terraform
(use-package terraform-mode
  :straight (terraform-mode
             :host github
             :repo "hcl-emacs/terraform-mode"
             :ref "1.0.1")
  :defer t
  :mode
  ("\\.terraformrc\\'" . terraform-mode)
  ("\\.tf\\'" . terraform-mode))


;; 📦 TEX-MODE
;; Встроенный пакет.
;; Работа с TeX и LaTeX
(use-package tex-mode
  :mode
  ("\\.tex\\'" . tex-mode))


;; 📦 TOOLBAR
;; Встроенный пакет, недоступный в Emacs NOX
(when (fboundp 'tool-bar-mode)
  (customize-set-variable 'tool-bar-mode nil))


;; 📦 TOOLTIP
;; Встроенный пакет.
;; Вывод подсказок в графической среде.
(use-package tooltip
  :custom
  (tooltip-mode nil "Отключить показ подсказок с помощью GUI")
  :config
  ;; Отключить показ подсказок с помощью GUI
  (tooltip-mode -1))


;; 📦 TREE-SITTER
;; https://emacs-tree-sitter.github.io/
;; Расширенная поддержка синтаксиса
(use-package tree-sitter
  :straight tree-sitter
  :delight ""
  :config
  ;; Подсветка синтаксиса средствами `tree-sitter' вместо
  ;; `font-lock-mode'.
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


(use-package tree-sitter-langs
  :straight tree-sitter-langs
  :requires tree-sitter
  :after tree-sitter
  :hook
  ((css-mode
    csv-mode
    dockerfile-mode
    emacs-lisp-mode
    hcl-mode
    html-mode
    js2-mode
    json-mode
    makefile-mode
    markdown-mode
    nxml-mode
    python-mode
    rst-mode
    ruby-mode
    sql-mode
    terraform-mode
    yaml-mode) . tree-sitter-mode))


;; 📦 UNIQUIFY
;; Встроенный пакет.
;; Используется для поддержания уникальности названий буферов, путей и т. д.
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward "Показывать каталог перед именем файла, если буферы одинаковые (по умолчанию имя<каталог>)")
  (uniquify-separator "/" "Разделять буферы с похожими именами, используя /"))


;; 📦 WEB-MODE
;; https://web-mode.org/
;; Режим для редактирования HTML и не только.
(use-package web-mode
  :straight (web-mode
             :host github
             :repo "fxbois/web-mode"
             :ref "v17.3.13")
  :custom
  (major-mode 'web-mode)
  (web-mode-attr-indent-offset 4 "4 пробела при выравнивании")
  (web-mode-enable-block-face t "Раскрашивать блок в соответствующий цвет")
  (web-mode-enable-css-colorization t "Код или имя цвета при редактировании CSS будут отмечены фоном этого цвета")
  (web-mode-enable-current-column-highlight t "Подсветка отступа активного элемента")
  (web-mode-enable-current-element-highlight t "Подсветка активного элемента разметки")
  (web-mode-enable-part-face t)
  (web-mode-html-offset 2 "Отступ в 2 знака для корректной работы `highlight-indentation-mode'.")
  (web-mode-markup-indent-offset 2 "Отступ при вёрстке HTML — 2 пробела")
  :mode "\\.html\\'")


;; 📦 WHICH-KEY MODE
;; https://elpa.gnu.org/packages/which-key.html
;; Показывает подсказки к сочетаниям клавиш.
(use-package which-key
  :straight (which-key :ref "v3.6.1")
  :delight ""
  :custom
  (which-key-computer-remaps t "Выводить актуальные сочетания клавиш, а не «как должно быть»")
  (which-key-idle-delay 2 "Задержка появления подсказки")
  (which-key-idle-secondary-delay 0.05 "Ещё одна задержка появления подсказки")
  (which-key-show-major-mode t "То же самое что и [C-h m], но в формате which-key")
  :config
  (which-key-mode 1)
  (which-key-setup-minibuffer)
  (which-key-setup-side-window-right)) ;; Показывать подсказки справа


;; 📦 WHITESPACE MODE
;; Встроенный пакет.
;; Отображение невидимых символов.
(use-package whitespace
  :delight ""
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
  ((adoc-mode
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
;; Встроенный пакет.
;; Перемещение между окнами Emacs.
(use-package windmove
  :bind
  (:map global-map
        ("C-x <up>" . windmove-up)
        ("C-x <down>" . windmove-down)))


;; 📦 WINNER-MODE
;; Встроенный пакет.
;; Управление окнами.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Convenience.html
;; Для управления конфигурациями окон используются последовательности
;; [C-c <left>] и [C-c <right>]
(use-package winner
  :config (winner-mode 1))


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
  :straight (yaml-mode
             :host github
             :repo "yoshiki/yaml-mode"
             :ref "0.0.16")
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
