;;; init.el --- Dunaevsky Maxim GNU EMACS config -*- lexical-binding:t -*-
;;; Commentary:
;;; Настройки EMACS для работы техническим писателем.

;;; Code:

(require 'cyrillic)
(require 'derived)
(require 'face-remap)
(require 'ispell)
(require 'saveplace)

(fset 'yes-or-no-p 'y-or-n-p) ;; Использовать y и n вместо yes и no (скоращает объём вводимого текста для подтверждения команд)

(defconst emacs-config-dir (file-name-directory user-init-file) "Корневая директория для размещения настроек.") ;; ~/.emacs.d/
(defconst emacs-autosave-dir (concat emacs-config-dir "saves") "Директория для файлов автосохранения.") ;; ~/.emacs.d/saves/
(defconst emacs-package-user-dir (expand-file-name "elpa" user-emacs-directory) "Пользовательский каталог с пакетами.") ;; ~/.emacs.d/elpa/
(defconst emacs-save-place-file (expand-file-name ".emacs-places" emacs-config-dir) "Имя файла с историей посещенных файлов.") ;; ~/.emacs.d/.emacs-places
(defconst emacs-custom-file (expand-file-name "custom.el" emacs-config-dir) "Путь к файлу пользовательских настроек.") ;; ~/.emacs.d/custom.el

;; Если нужного каталога не существует, его следует создать
(dolist
  (emacs-directory
    (list
      emacs-config-dir
      emacs-autosave-dir
      emacs-package-user-dir))
  (unless (file-directory-p emacs-directory)
    (make-directory emacs-directory)
    (message (format "Создана директория %s" emacs-directory))))

(defconst emacs-default-font-height 15 "Размер шрифта по умолчанию.")

;; -> Стандартные настройки
(setq-default
  abbrev-mode t ; Включить поддержку аббревиатур глобально
  auto-save-file-name-transforms `((".*" , emacs-autosave-dir) t)
  blink-matching-paren t                         ;; Мигать, когда скобки парные
  create-lockfiles nil                           ;; Не надо создавать lock-файлы, от них одни проблемы
  cursor-type 'bar                               ;; Курсор в виде вертикальной черты
  custom-file emacs-custom-file                  ;; Файл для сохранения пользовательских настроек, сделанных в customize
  delete-old-versions t                          ;; Удалять старые резервные копии файлов без лишних вопросов
  gc-cons-threshold (* 50 1000 1000)             ;; Увеличим лимит для сборщика мусора с 800 000 до 50 000 000
  indent-line-function (quote insert-tab)        ;;
  indent-tabs-mode nil                           ;; Использовать для выравнивания по нажатию TAB пробелы вместо табуляций
  inhibit-splash-screen t                        ;; Не надо показывать загрузочный экран
  inhibit-startup-message t                      ;; Не надо показывать приветственное сообщение
  initial-scratch-message nil                    ;; В новых буферах не нужно ничего писать
  large-file-warning-threshold (* 100 1024 1024) ;; Предупреждение при открытии файлов больше 100 МБ (по умолчанию — 10 МБ)
  load-prefer-newer t                            ;; Если есть файл elc, но el новее, загрузить el-файл
  make-backup-files nil                          ;; Резервные копии не нужны, у нас есть undo-tree
  overwrite-mode-binary nil                      ;; Выключить режим перезаписи текста под курсором для бинарных файлов
  overwrite-mode-textual nil                     ;; Выключить режим перезаписи текста под курсором для текстовых файлов
  package-user-dir emacs-package-user-dir        ;; Хранить все пакеты в каталоге ~/.emacs.d/elpa/
  require-final-newline t                        ;; Автоматически вставлять в конец файла пустую строку, если её там нет
  ring-bell-function #'ignore                    ;; Заблокировать пищание
  save-abbrevs 'silently                         ;; Сохранять аббревиатуры без лишних вопросов
  save-place-file emacs-save-place-file          ;; Хранить данные о позициях в открытых файлах в .emacs-places
  save-place-forget-unreadable-files 1           ;; Если файл нельзя открыть, то и помнить о нём ничего не надо
  scroll-conservatively 100000                   ;; TODO: проверить, что это такое
  scroll-margin 5                                ;; При прокрутке помещать курсор на 5 строк выше / ниже верхней / нижней границы окна
  scroll-preserve-screen-position 1              ;; TODO: проверить, что это такое
  show-trailing-whitespace t                     ;; Показывать висячие пробелы
  source-directory "/usr/share/emacs/27.1/src/"  ;; Путь к исходному коду EMACS
  suggest-key-bindings t                         ;; Показывать подсказку клавиатурной комбинации для команды
  tab-width 4                                    ;; Обменный курс на TAB — 4 SPACES
  text-scale-mode-step 1.1                       ;; Шаг увеличения масштаба
  truncate-lines 1                               ;; Обрезать длинные строки
  uniquify-buffer-name-style 'forward            ;; Показывать директорию перед именем файла, если буферы одинаковые (по умолчанию имя<директория>)
  uniquify-separator "/"                         ;; Разделять буферы с похожими именами, используя /
  use-dialog-box nil                             ;; Диалоговые окна не нужны, будем использовать текстовый интерфейс
  user-full-name "Dunaevsky Maxim"               ;; Имя пользователя
  visible-bell t                                 ;; Эффект мигания при переходе в буфер
  window-divider-default-places 't               ;; Разделители окон со всех сторон (по умолчанию только справа)
  window-divider-default-right-width 3           ;; Ширина в пикселях для линии-разделителя окон
  x-underline-at-descent-line t)


;; Если используется старая версия EMACS, нужно указать параметры протокола TLS.
;; В противном случае будут проблемы при загрузке архива пакетов.
(when (< emacs-major-version 27)
  (require 'gnutls)
  (setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; -> Стандартные режимы
(column-number-mode 1)      ;; Показывать номер колонки в статусной строке
(delete-selection-mode t)   ;; Если регион выделен, удалить его, а не последний символ.
(global-font-lock-mode t)   ;; Отображать шрифты красиво, используя Font Face's
(global-auto-revert-mode 1) ;; Автоматически перезагружать буфер при изменении файла на дискею
(global-visual-line-mode 1) ;; Подсвечивать текущую строку
(line-number-mode t)        ;; Показывать номер строки в статусной строке
(save-place-mode 1)         ;; Помнить позицию курсора в открытых когда-либо файлах.
(scroll-bar-mode -1)        ;; Отключить полосы прокрутки
(size-indication-mode 1)    ;; Отображать размер буфера в строке статуса
(window-divider-mode t)     ;; Визуально разделять окна EMACS


;; -> КОДИРОВКИ
(defvar default-coding 'utf-8 "Кодировка по умолчанию.")
(when (string-equal system-type "windows-nt")
  (setq default-coding 'cp-1251))
(prefer-coding-system default-coding)              ;; При попытке определить кодировку файла начинать перебор с UTF-8
(set-default-coding-systems default-coding)        ;; Кодировка по умолчанию
(set-keyboard-coding-system default-coding)        ;; Кодировка символов при вводе текста в терминале
(set-language-environment default-coding)          ;; Кодировка языка по умолчанию
(set-selection-coding-system default-coding)       ;; Кодировка символов для передачи скопированных в буфер данных другим приложениям X11
(set-terminal-coding-system default-coding)        ;; Кодировка символов для вывода команд, запущенных в терминале
(setq-default locale-coding-system default-coding) ;; UTF-8 по умолчанию


;; -> ПАКЕТЫ
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(setq
  package-archive-priorites '( ;; Порядок использования архивов. Чем выше приоритет, тем важнее архив
    ("nongnu" . 50)
    ("gnu" . 40)
    ("melpa-stable" . 30)
    ("melpa" . 20)
  )
  package-install-upgrade-built-in t) ;; Обновлять в том числе встроенные пакеты



;; Проверка наличия индекса пакетов
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

(require 'use-package)
(require 'use-package-ensure)
(setq
  use-package-always-ensure t ;; Автоматическая установка отсутствующих пакетов
  use-package-always-pin "nongnu") ;; ПО умолчанию использовать версию из nongnu


;; -> Создание пустого буфера
(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programming).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))


;; -> Сочетания клавиш
;; Изменение размеров окон
;;
;;                         enlarge-window
;;                              ↑
;; shrink-window-horizontally ←   → enlarge-window-horizontally
;;                              ↓
;;                         shrink-window
;;
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)   ;; Уменьшить размер окна по ширине
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally) ;; Увеличить размер окна по ширине
(global-set-key (kbd "S-C-<down>") 'enlarge-window)               ;; Увеличить размер окна по ширине
(global-set-key (kbd "S-C-<up>") 'shrink-window)                  ;; Уменьшить размер окна по высоте

;; Разные сочетания клавиш
(global-set-key (kbd "S-<SPC>") 'just-one-space)                  ;; Заменить пробелы и TAB'ы до и после курсора на один пробел
(global-set-key (kbd "<escape>") 'keyboard-quit)                  ;; ESC работает как и Ctrl+g, т. е. прерывает ввод команды
(global-set-key (kbd "C-z") 'undo)                                ;; Отмена
(global-set-key (kbd "C-x k") 'kill-this-buffer)                  ;; Закрыть буфер
(global-set-key (kbd "C-v") 'yank)                                ;; Вставить текст из временного буфера
(global-set-key (kbd "C-x o") 'next-multiframe-window)            ;; Перейти в следующее окно
(global-set-key (kbd "C-x O") 'previous-multiframe-window)        ;; Перейти в предыдущее окно
(global-set-key (kbd "<C-tab>") 'next-buffer)                     ;; Следующий буфер

;; Save/close/open
(global-set-key (kbd "M-'") 'comment-or-uncomment-region)         ;; Закомментировать/раскомментировать область

(global-set-key (kbd "<f3>") 'replace-string)                     ;; Поиск и замена
(global-set-key (kbd "<f7>") 'xah-new-empty-buffer)               ;; Создание пустого буфера
(global-set-key (kbd "<f9>") 'sort-lines)                         ;; Отсортировать выделенные строки
(global-set-key (kbd "<esc>") 'keyboard-quit)                     ;; Аналог Ctrl+g

;; Переключение буферов с помощью Ctrl+X и стрелочек
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(global-set-key (kbd "M--") (lambda () (interactive) (insert "—"))) ;; Вставка длинного тире по нажатию Alt+-

(global-unset-key (kbd "<insert>")) ;; Запретить нажатие клавиши Insert (переключает режим перезаписи)
(global-unset-key (kbd "M-,"))      ;; Disable M-, as markers


;; -> Настройки, специфичные для графического режима
(defun setup-gui-settings (frame-name)
  "Настройки, необходимые при запуске EMACS в графической среде.

  FRAME-NAME — имя фрейма, который настраивается."
  (when (display-graphic-p frame-name)
    (defvar availiable-fonts (font-family-list)) ;; Какие есть семейства шрифтов?
    (defvar default-font-family nil "Шрифт по умолчанию.")

    ;; Перебор шрифтов
    (cond
      (
        (member "Fira Code" availiable-fonts)
        (setq default-font-family "Fira Code"))
      (
        (member "DejaVu Sans Mono" availiable-fonts)
        (setq default-font-family "DejaVu Sans Mono"))
      (
        (member "Source Code Pro" availiable-fonts)
        (setq default-font-family "Source Code Pro"))
      (
        (member "Consolas" availiable-fonts)
        (setq default-font-family "Consolas")))

    (when default-font-family
      (message "Выбран шрифт по умолчанию.")
      ;; Это формат  X Logical Font Description Conventions, XLFD
      ;; https://www.x.org/releases/X11R7.7/doc/xorg-docs/xlfd/xlfd.html
      (set-frame-font (format "-*-%s-normal-normal-normal-*-%d-*-*-*-m-0-iso10646-1" default-font-family emacs-default-font-height) nil t)
      (set-face-attribute 'default nil :family default-font-family)
      )

    (set-face-attribute 'default nil :height (* emacs-default-font-height 10))

    ;; Если режим графический, между вкладками следует переходить по Ctrl+PgUp и Ctrl+PgDn

    (global-set-key (kbd "C-<prior>") 'centaur-tabs-backward)
    (global-set-key (kbd "C-<next>") 'centaur-tabs-forward)

    ;; Если режим графический, для расстановки курсоров MultipleCursors можно использовать Alt+Click
    (when (fboundp 'multiple-cursors)
      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))))

;; Правильный способ определить, что EMACS запущен в графическом режиме. Подробнее здесь:
;; https://emacsredux.com/blog/2022/06/03/detecting-whether-emacs-is-running-in-terminal-or-gui-mode/
(add-to-list 'after-make-frame-functions #'setup-gui-settings)


;; -> ACE-WINDOW
;; https://github.com/abo-abo/ace-window
;; Быстрое переключение между окнами по Alt+O
(use-package ace-window
  :bind
  ("M-o" . ace-window))


;; -> ADOC-MODE
;; https://github.com/bbatsov/adoc-mode
;; Работа с AsciiDoc
(use-package adoc-mode
  :custom
  (adoc-fontify-code-blocks-natively 10000)
  :mode
  ("\\.adoc\\'" . adoc-mode)
  ("\\.txt\\'" . adoc-mode))


;; -> AGGRESSIVE-INDENT
;; Принудительное выравнивание кода
(use-package aggressive-indent
  :hook
  ((
     emacs-lisp-mode
     js2-mode
     json-mode
     nxml-mode
     php-mode
     protobuf-mode
     sh-mode
     sql-mode
     ) . aggressive-indent-mode))


;; -> AIRLINE-THEMES
;; https://github.com/AnthonyDiGirolamo/airline-themes
;; Темы для панели информации
(use-package airline-themes)


;; -> ALL-THE-ICONS
;; Настройка иконочных шрифтов и немножко GUI.
;; Для установки самих шрифтов следует использовать команду `all-the-icons-install-fonts'.
;; В Debian Linux шрифты будут загружены в каталог `~/.local/share/fonts'. Рекомендуется
;; скопировать их в `/usr/local/share/fonts/'.
(use-package all-the-icons)

;; -> ALL-THE-ICONS-DIRED
;; https://github.com/wyuenho/all-the-icons-dired
(use-package all-the-icons-dired
:after (all-the-icons dired)
:hook (dired-mode . all-the-icons-dired-mode))

;; -> ALL-THE-ICONS-IBUFFER
;; https://github.com/seagle0128/all-the-icons-ibuffer
(use-package all-the-icons-ibuffer
  :custom
  (all-the-icons-ibuffer-human-readable-size t "Показывать размер файлов в ibuffer в человекочитаемом виде")
  (all-the-icons-ibuffer-icon t "Показывать иконки файлов в ibuffer")
  :after (all-the-icons ibuffer)
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))


;; -> ANSIBLE
;; https://github.com/k1LoW/emacs-ansible
;; Дополнительные возможности при работе с YAML-файлами Ansible
(use-package ansible)


;; -> APHELEIA
;; https://github.com/radian-software/apheleia
;; Форматирование содержимого буфера. Почти как `format-all-buffer', только
;; немного другое.
(use-package apheleia
  :config
  (apheleia-global-mode 1))


;; -> APT SOURCES LIST MODE
;; https://git.korewanetadesu.com/apt-sources-list.git
;; Режим для редактирования файлов настройки репозиториев APT
(use-package apt-sources-list
  :mode
  ("\\.list\\'" . apt-sources-list-mode))


;; -> CALENDAR
(use-package calendar
  :custom
  (calendar-week-start-day 1 "Начнём неделю с понедельника"))


;; -> CENTAUR-TABS
;; https://github.com/ema2159/centaur-tabs
;; Вкладки с иконками и прочими удобствами
(use-package centaur-tabs
  :custom
  (centaur-tabs-close-button "×" "Будем использовать вот этот символ вместо X")
  (centaur-tabs-enable-key-bindings t "Включить комбинации клавиш из `centaur-tabs'.")
  (centaur-tabs-height 36 "Высота вкладок")
  (centaur-tabs-modified-marker t "Показывать маркер, если содержимое вкладки изменилось")
  (centaur-tabs-set-bar 'under "Доступные значения: over, under")
  (centaur-tabs-set-icons t "Включить иконки. если это графический режим")
  (centaur-tabs-style "slant" "Также доступны: bar, alternate, box, chamfer, rounded, slant, wawe, zigzag")
  :config
  (centaur-tabs-mode 1)
  (setq x-underline-at-descent-line t) ;; "Если пакет используется вне Spacemacs, необходимо включить это, чтобы подчёркивание отображалось корректно"
  :hook
  ((
     dashboard-mode
     dired-mode) . centaur-tabs-local-mode))


;; -> CHECKDOC
;; Встроенный пакет для проверки строк документации.
(use-package checkdoc
  :hook (emacs-lisp-mode . checkdoc-minor-mode))


;; -> COMPANY-MODE
;; https://company-mode.github.io/
;; Автодополнение
(use-package company
  :custom
  (company-idle-delay 0.5 "Задержка вывода подсказки — полсекунды")
  (company-minimum-prefix-length 2 "Минимум 2 знака, чтобы company начала работать")
  (company-show-quick-access t "Показывать номера возле потенциальных кандидатов")
  (company-tooltip-align-annotations t "TODO")
  (company-tooltip-limit 10 "Ограничение на число подсказок")
  :hook
  ((
     css-mode
     dockerfile-mode
     emacs-lisp-mode
     js2-mode
     nxml-mode
     php-mode
     rst-mode
     ruby-mode
     ) . company-mode)
  :bind
  ([tab] . company-indent-or-complete-common))


;; -> COMPANY-BOX
;; https://github.com/sebastiencs/company-box
;; Красивости для `company-mode':
;; - разные цвета для разных бэкендов
;; - иконки
;; - вывод документации для кандидатов
;; - нет ограничений
(use-package company-box
  :after (company)
  :hook (company-mode . company-box-mode))


;; -> CONF MODE
;; Встроенный пакет. Основной режим для редактирования конфигурационных файлов INI/CONF
(use-package conf-mode
  :mode
  ("\\.editorconfig\\'" . conf-mode)
  ("\\.env\\'" . conf-mode)
  ("\\.flake8\\'" . conf-mode)
  ("\\.ini\\'" . conf-mode)
  ("\\.pylintrc\\'" . conf-mode)
  ("\\.terraformrc\\'" . conf-mode))


;; -> CSS-MODE
;; Встроенный режим
(use-package css-mode
  :custom
  (css-indent-offset 2)
  :mode
  ("\\.css\\'" . css-mode))


;; -> DASHBOARD
;; https://github.com/emacs-dashboard/emacs-dashboard
;; Отображает дашборд при запуске EMACS
(use-package dashboard
  :custom
  (dashboard-display-icons-p t "Включить отображение иконок")
  (dashboard-icon-type 'nerd-icons "Использовать иконки из пакета `nerd-icons'")
  (dashboard-items                    ;; Элементы дашборда
    '(
       (recents . 15)                  ;; Последние открытые файлы
       (bookmarks . 10)                ;; Последние закладки
       (projects . 10)                 ;; Последние проекты
       (agenda . 0)                    ;; Агенда
       (registers . 0)))                ;; Регистры
  (dashboard-set-footer nil "Скрыть \"весёлые\" надписи в нижней части дашборда")
  (dashboard-set-file-icons t "Показывать иконки рядом с элементами списков")
  :config
  (dashboard-setup-startup-hook))


;; -> DESKTOP-SAVE-MODE
;; Встроенный пакет
;; Позволяет сохранять состояние EMACS между сессиями
(use-package desktop
  :custom
  (desktop-modes-not-to-save '(dired-mode Info-mode info-lookup-mode)) ; А вот эти не сохранять
  (desktop-save t "Сохранять список открытых буферов, файлов и т. д. без лишних вопросов")
  :config
  (desktop-save-mode t)
  (add-hook 'server-after-make-frame-hook #'desktop-read))


;; -> DIFF-HL
;; https://github.com/dgutov/diff-hl
;; Показывает небольшие маркеры рядом с незафиксированными изменениями. Дополняет функциональность git-gutter,
;; которые показывает изменения только в обычных буферах. Этот пакет умеет работать с dired и другими режимами.
(use-package diff-hl
  :config
  (global-diff-hl-mode 1))


;; -> DIRED
;; Встроенный пакет для работы с файлами и каталогами.
(require 'dired)
(when (string-equal system-type "gnu/linux")
  ;; Это может не работать в Windows, надо проверить
  (setq dired-listing-switches "-lahX --group-directories-first"))
(global-set-key (kbd "C-o") 'dired)
(add-hook 'dired-mode-hook #'auto-revert-mode)


;; -> DISPLAY-LINE-NUMBERS-MODE
;; Встроенный пакет
;; Показывает номера строк
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode 1))


;; -> DOCKERFILE-MODE
;; https://github.com/spotify/dockerfile-mode
;; Работа с файлами `Dockerfile'.
(use-package dockerfile-mode)


;; -> DOOM-MODELINE
;; https://github.com/seagle0128/doom-modeline
;; Красивая и многофункциональная статусная панель. Для корректной работы требуется
;; пакет `nerd-icons' и установленные шрифты.
(use-package doom-modeline
  :custom
  (doom-modeline-buffer-encoding t "Показывать кодировку")
  (doom-modeline-buffer-modification-icon t "Показывать наличие изменений в буфере")
  (doom-modeline-buffer-name t "Показывать имя буфера")
  (doom-modeline-buffer-state-icon t "Показывать состояние буфера")
  (doom-modeline-env-enable-go t "Показывать версию Golang")
  (doom-modeline-env-enable-python t "Показывать версию Python")
  (doom-modeline-env-enable-ruby t "Показывать версию Ruby")
  (doom-modeline-hud nil "Использовать HUD. Лучше выключить, т. к. иначе строка отображается некорректно.")
  (doom-modeline-icon t "Показывать иконки")
  (doom-modeline-indent-info t "Информация об отступах")
  (doom-modeline-lsp t "Показывать статус LSP")
  (doom-modeline-major-mode-color-icon t "Иконка основного режима вместо текста")
  (doom-modeline-major-mode-icon t "Показывать иконку основного режима")
  (doom-modeline-project-detection 'auto "Определение того, что идёт работа с проектом")
  (doom-modeline-vcs-max-length 12 "Ограничение на длину имени активной ветки VCS")
  (doom-modeline-window-width-limit nil "Нет ограничений на ширину окна")
  :config
  (doom-modeline-mode 1))


;; -> DOOM-THEMES
;; https://github.com/doomemacs/themes
;; Темы из DOOM Emacs
(use-package doom-themes
  :config
  (load-theme 'doom-monokai-classic t)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config))


;; -> EASY KILL
;; https://github.com/leoliu/easy-kill
;; Удобнее работать с удалением текстовых блоков
;; TODO: в каком смысле "удобнее"?
(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))


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
  :bind
  ("C-c '" . edit-indirect-region))


;; -> EDITORCONFIG EMACS
;; Поддержка https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :after (ws-butler)
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode "Очистка лишних пробелов методом `ws-butler'.")
  :config
  (editorconfig-mode 1))


;; -> ELEC-PAIR MODE
;; Встроенный пакет.
;; Автоматически вставляет при вводе одной скобки парную ей. Если
;; выделен регион, то в скобки обрамляется он.
(use-package elec-pair
  :config
  (add-to-list 'electric-pair-pairs '(?« . ?»))
  (add-to-list 'electric-pair-pairs '(?{ . ?}))
  (add-to-list 'electric-pair-pairs '(?‘ . ’?))
  (add-to-list 'electric-pair-pairs '(?“ . ”?))
  (add-to-list 'electric-pair-pairs '(?‚ . ‘?))
  (electric-pair-mode t)) ;; Глобальный режим


;; -> ELECTRIC-INDENT MODE
;; Автоматический отступ. В основном только мешает, лучше выключить.
(use-package electric
  :config
  (electric-indent-mode -1)
  :hook
  (emacs-lisp-mode . electric-indent-mode)
  (rst-mode . electric-indent-mode))


;; -> ELPY
;; Python IDE
;; https://elpy.readthedocs.io/en/latest/index.html
;; Краткая справка по использованию:
;; Проверка состояния: `elpy-config'.
;; Активация окружения: `pyenv-activate', указать путь к каталогу с окружением.
(use-package elpy
  :after
  (python-mode)
  :hook python-mode)


;; -> EMACS-LISP MODE
;; IT IS NOT A ELISP-MODE!
;; Встроенный пакет для EMACS Lisp
(defun setup-emacs-lisp-mode ()
  "Настройки для `emacs-lisp-mode'."
  (highlight-indentation-set-offset 2)
  )
(add-to-list 'auto-mode-alist '("\\abbrev_defs\\'" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook #'setup-emacs-lisp-mode)


;; -> FLYCHECK
;; https://flycheck.org
;; Проверка синтаксиса на лету с помощью статических анализаторов
(use-package flycheck
  :custom
  (flycheck-textlint-config ".textlintrc.yaml" "Файл настроек Textlint")
  (flycheck-check-syntax-automatically '(mode-enabled save new-line))
  (flycheck-locate-config-file-functions '(
                                            flycheck-locate-config-file-by-path
                                            flycheck-locate-config-file-ancestor-directories
                                            flycheck-locate-config-file-home))
  (flycheck-highlighting-mode 'lines "Стиль отображения проблемных мест — вся строка")
  (flycheck-indication-mode 'left-fringe "Место размещения маркера ошибки — левая граница")
  (flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc")
  :hook
  ((
     adoc-mode
     conf-mode
     css-mode
     dockerfile-mode
     emacs-lisp-mode
     js2-mode
     json-mode
     markdown-mode
     nxml-mode
     php-mode
     protobuf-mode
     rst-mode
     ruby-mode
     sh-mode
     sql-mode
     terraform-mode
     web-mode
     yaml-mode) . flycheck-mode))

;; -> FLYCHECK-COLOR-MODE-LINE
;; https://github.com/flycheck/flycheck-color-mode-line
(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))


;; -> FLYSPELL-MODE
;; Проверка орфографии с помощью словарей
(when
  ;; Использовать пакет только в том случае, когда дело происходит в Linux и
  ;; Aspell доступен
  (and
    (string-equal system-type "gnu/linux") ;; Aspell для Linux, в Windows без проверки орфографии
    (file-exists-p "/usr/bin/aspell"))      ;; Надо убедиться, что программа установлена в ОС
  (use-package flyspell
    :custom
    (ispell-program-name "/usr/bin/aspell")
    :hook
    ((
       adoc-mode
       markdown-mode
       rst-mode
       ) . flyspell-mode)))


;; -> FLYMAKE
;; Встроенный пакет
(use-package flymake
  :hook (emacs-lisp-mode . flymake-mode))



;; -> FORMAT-ALL
;; https://github.com/lassik/emacs-format-all-the-code
;; Форматирование кода по нажатию [F12]
(use-package format-all
  :bind (([f12] . format-all-buffer)))


;; -> GO-MODE
;; https://github.com/dominikh/go-mode.el
;; Поддержка Golang
(use-package go-mode)


;; -> HELM
;; Подсказки при вводе текста в минибуфере и не только
;; https://helm.io/
(use-package helm
;;  :bind
;;  ("M-x" . helm-M-x)
  :config
  (helm-mode 1))


;; -> HIGHLIGHT-INDENTATION-MODE
;; https://github.com/antonj/Highlight-Indentation-for-Emacs
;; Показывает направляющие для отступов
(use-package highlight-indentation
  :hook
  ((
     makefile-mode
     makefile-gmake-mode
     emacs-lisp-mode
     markdown-mode
     python-mode
     rst-mode
     terraform-mode
     web-mode
     yaml-mode
     ) . highlight-indentation-mode))


;; -> HL-TODO
;; https://github.com/tarsius/hl-todo
;; Подсветка TODO, FIXME и т. п.
(use-package hl-todo
  :config
  (global-hl-todo-mode t))


;; -> IBUFFER
;; Встроенный пакет для удобной работы с буферами.
;; По нажатию F2 выводит список открытых буферов.
(require 'ibuffer)
(require 'ibuf-ext)
(defalias 'list-buffers 'ibuffer)
(setq
 ibuffer-expert 1                                        ;; Расширенный  режим для ibuffer
 ibuffer-hidden-filter-groups (list "Helm" "*Internal*") ;; Не показывать эти буферы
 ibuffer-show-empty-filter-groups nil                    ;; Если группа пустая, ibuffer не должен её отображать.
 ibuffer-sorting-mode 'filename/process                  ;; Сортировать файлы в ibuffer по имени / процессу.
 ibuffer-truncate-lines nil                              ;; Не обкусывать строки в ibuffer
 ibuffer-use-other-window nil                            ;; Не надо открывать ibuffer в другом окне, пусть открывается в текущем
 ibuffer-saved-filter-groups                             ;; Группы по умолчанию
 '(
   ("default"
    ("Dired" (mode . dired-mode))
    ("EMACS Lisp" (mode . emacs-lisp-mode))
    ("Org" (mode . org-mode))
    ("Markdown" (mode . markdown-mode))
    ("AsciiDoc" (mode . adoc-mode))
    ("ReStructured Text" (mode . rst-mode))
    ("CONF / INI"
     (mode . conf-mode)
     (name . "\\.editorconfig\\'")
     (name . "\\.ini\\'")
     (name . "\\.conf\\'"))
    ("XML"
     (or
      (mode . xml-mode)
      (mode . nxml-mode)))
    ("YAML" (mode . yaml-mode))
    ("Makefile"
     (or
      (mode . makefile-mode)
      (name  . "^Makefile$")))
    ("Protobuf" (mode . protobuf-mode))
    ("Golang" (mode . go-mode))
    ("Python"
     (or
      (mode . python-mode)
      (mode . anaconda-mode)))
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
      (mode . web-mode)
      (name . "^\\*.js$")))
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
      (name . "\\*\\(Echo\\|Minibuf\\)")))))
 ibuffer-formats ;; Форматирование вывода
 '(
   (
    mark                      ;; Отметка
    modified                  ;; Буфер изменён?
    read-only                 ;; Только чтение?
    locked                    ;; Заблокирован?
    " "
    (name 30 40 :left :elide) ;; Имя буфера: от 30 до 40 знаков
    " "
    (mode 8 -1 :left)         ;; Активный режим: от 8 знаков по умолчанию, при необходимости увеличить
    " "
    filename-and-process)     ;; Имя файла и процесс
   ( ;; Если отображать особо нечего, использовать сокращённый формат
    mark         ;; Отметка?
    " "
    (name 32 -1) ;; Имя буфера: 32 знака, при неоходимости — расширить на сколько нужно
    " "
    filename)))  ;; Имя файла
(defun setup-ibuffer-mode ()
  "Настройки `ibuffer-mode'."
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))
(add-hook 'ibuffer-mode-hook #'setup-ibuffer-mode)
(global-set-key (kbd "<f2>") 'ibuffer)


;; -> ICOMPLETE-MODE
;; Встроенный пакет
;; Подсказки в минибуфере
;; (use-package icomplete
;;   :config
;;   (icomplete-vertical-mode 1))


;; -> JS2-MODE
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode
  ("\\.js\\'" . js2-mode))


;; -> JSON-MODE
;; Встроенный пакет
(use-package json-mode)


;; -> LEUVEN-THEME
;; Тема оформления
;; (use-package leuven-theme
;;   :config
;;   (load-theme 'leuven-dark t))


;; -> LSP
;; https://emacs-lsp.github.io/lsp-mode/
;; Базовый пакет, необходимый для работы LSP
;; Требуется EMACS версии 26.3 или новее.
;;
;; Полный список поддерживаемых языков и технологий:
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-dockerfile/
;;
;; Чтобы LSP "видел" директорию, её нужно добавить с помощью `lsp-workspace-folder-add'
;;
;; Для работы пакета нужны дополнительные средства:
;;
;; DOCKERFILE: npm install -g dockerfile-language-server-nodejs
;; GOLANG: go install golang.org/x/tools/gopls@latest
;; JSON: npm install -g vscode-json-languageserver
;; MAKEFILE: sudo pip3 install cmake-language-server
;; MARKDOWN: npm install -g remark-language-server remark
;; NXML: lsp-install-server, выбрать xmlls, установить на уровне системы JDK
;; SQL: go install github.com/lighttiger2505/sqls@latest
;; TERRAFORM: нужен установленный в системе terraform-ls. Можно скачать с сайта hashicorp.com
;; XML: lsp-install-server, выбрать xmlls, установить на уровне системы JDK
;; YAML: npm install -g yaml-language-server

(when ;; Нужна версия Emacs 26.3 или выше
	(or
    (and
      (= emacs-major-version 26)
      (>= emacs-minor-version 3))
    (> emacs-major-version 26))

  (use-package lsp-mode
    :custom
    (lsp-headerline-breadcrumb-enable t "Показывать \"хлебные крошки\" в заголовке")
    (lsp-modeline-diagnostics-enable t "Показывать ошибки LSP в статусной строке")
    :hook (
            (ansible . lsp)
            (go-mode . lsp)
            (python-mode . lsp)))

  (use-package lsp-ui
    :custom
    (lsp-ui-doc-enable t "Показывать документацию в LSP-UI")
    (lsp-ui-peek-always-show t "TODO")
    (lsp-ui-peek-enable t "TODO")
    (lsp-ui-sideline-enable t "TODO")
    :after (lsp-mode)
    :hook lsp-mode))


;; -> MAGIT
;; https://magit.vc/
;; Magic + Git + Git-gutter. Лучшее средство для управления Git.
(use-package magit
  :bind
  ([f5] . magit-status)
  ([f6] . magit-checkout))


;; -> MAKEFILE
;; Встроенный пакет для работы с Makefile
(use-package make-mode
  :config
  (setq-local indent-tabs-mode t))


;; -> MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
;; Режим для работы с файлами в формате Markdown
(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t "Подсвечивать синтаксис в примерах кода")
  (markdown-header-scaling-values '(1.0 1.0 1.0 1.0 1.0 1.0) "Все заголовки одной высоты")
  (markdown-list-indent-width 4 "Размер отступа для выравнивания вложенных списков")
  :config
  (setq-local word-wrap t)
  :bind (
          :map markdown-mode-map
          ("M-." . markdown-follow-thing-at-point)))


;; -> MENU-BAR-MODE
;; Отключить меню за ненадобностью.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))


;; -> MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
;; Позволяет использовать мультикурсорность.
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))


;; -> NERD-ICONS
;; https://github.com/rainstormstudio/nerd-icons.el
;; Требуется для корректной работы `doom-modeline'. Начиная с версии 4.0.0
;; пакет `all-the-icons' не поддерживается.
;; Для установки самих шрифтов следует использовать команду `nerd-icons-install-fonts'.
;; В Debian Linux шрифты будут загружены в каталог `~/.local/share/fonts'. Рекомендуется
;; скопировать их в `/usr/local/share/fonts/'.
(use-package nerd-icons)


;; -> NXML-MODE
;; Встроенный пакет
;; Почти как xml-mode, только лучше и новее
(with-eval-after-load 'nxml-mode
  (setq
    nxml-attribute-indent 4                   ;; Выравнивание атрибутов
    nxml-auto-insert-xml-declaration-flag nil ;; Не вставлять декларацию
    nxml-bind-meta-tab-to-complete-flag t     ;; Использовать TAB для завершения ввода
    nxml-child-indent 4                       ;; Выравнивание дочерних элементов
    nxml-slash-auto-complete-flag t)          ;; "Закрывать теги по вводу /"
  (setq-local
    tab-width 2) ;; Ширина TAB'а будет 2 пробела
  (add-to-list 'auto-mode-alist '("\\.pom\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode)))


;; -> ORG-MODE
;; https://orgmode.org/
;; Органайзер, и не только
(use-package org
  :config
  (setq-local
    truncate-lines nil    ;; Не обрезать строки
    left-margin-width 4   ;; Отступ слева
    org-todo-keywords '(( ;; Ключевые слова для статусов
                          sequence
                          "НОВАЯ"
                          "|"
                          "ВЫПОЛНЕНА"))
    right-margin-width 4 ;; Отступ справа
    word-wrap t))        ;; Перенос длинных строк


;; -> PACKAGE-LINT
;; https://github.com/purcell/package-lint
;; Проверка пакетов Emacs
(use-package package-lint)


;; -> PAREN
;; Встроенный пакет
;; Подсвечивает парные скобки
(use-package paren
  :config (show-paren-mode 1))

;; -> PHP-MODE
;; https://github.com/emacs-php/php-mode
;; Поддержка PHP.
(use-package php-mode)


;; -> PROJECTILE
;; https://docs.projectile.mx/projectile/installation.html
;; Управление проектами. Чтобы каталог считался проектом, он должен быть
;; под контролем любой системы версионирования, либо содержать специальные
;; файлы. В крайнем случае сгодится пустой файл .projectile
;; Подробнее здесь: https://docs.projectile.mx/projectile/projects.html
(use-package projectile
  :bind
  (:map projectile-mode-map
    ("M-p" . projectile-command-map))
  :config
  (projectile-global-mode 1))


;; -> PROTOBUF-MODE
;; https://github.com/emacsmirror/protobuf-mode
;; Работа с файлами Protobuf: подсветка синтаксиса, переход по ссылками и т. д.
(use-package protobuf-mode
  :mode
  ("\\.proto\\'" . protobuf-mode))


;; Подсвечивать курсор при его перемещении на несколько строк
(if
  ;; -> PULSAR-MODE
  ;; https://github.com/protesilaos/pulsar
  (or ;; Нужна версия Emacs 27.1 или выше
    (and
      (= emacs-major-version 27)
      (>= emacs-minor-version 1))
    (> emacs-major-version 27))
  (use-package pulsar
    :custom
    (pulsar-pulse t)
    :config
    (pulsar-global-mode 1)
		(add-hook 'next-error-hook #'pulsar-pulse-line))
  ;; EMACS более старый, чем 27.1
  (when (fboundp 'after-focus-change-function)
    (use-package beacon
      :config
      (beacon-mode 1))))


;; -> PYTHON-MODE
;; Встроенный пакет для работы с Python
(use-package python-mode
  :custom
  (py-electric-comment-p t "TODO")
	(py-pylint-command-args "--max-line-length 120" "TODO"))


;; -> PYVENV
;; http://github.com/jorgenschaefer/pyvenv
;; Позволяет активировать виртуальные окружения из Emacs
(use-package pyvenv
  :after (python-mode)
  :hook python-mode)


;; -> RAINBOW-DELIMITERS-MODE
;; https://github.com/Fanael/rainbow-delimiters
;; Подсветка парных скобок одним и тем же цветом
(use-package rainbow-delimiters
  :hook
  ((
     adoc-mode
     apt-sources-list-mode
     conf-mode
     css-mode
     emacs-lisp-mode
     go-mode
     js2-mode
     json-mode
     makefile-mode
     makefile-gmake-mode
     markdown-mode
     nxml-mode
     org-mode
     php-mode
     protobuf-mode
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
  :init
  (setq default-input-method 'russian-techwriter))


;; -> REVERSE-IM
;; https://github.com/a13/reverse-im.el
;; Чтобы сочетания клавиш работали в любой раскладке.
(use-package reverse-im
  :custom
  (reverse-im-input-methods '(
                               "russian-computer"
                               "russian-techwriter"))
  :config
  (reverse-im-mode t))


;; -> RST-MODE
;; Основной режим для редактирования reStructutedText
;; Больше здесь: https://www.writethedocs.org/guide/writing/reStructuredText/
(require 'rst)
(defun setup-rst-mode()
  "Настройки для `rst-mode'."
  (highlight-indentation-set-offset 3))
(add-hook 'rst-mode-hook #'setup-rst-mode)
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))


;; -> RUBY-MODE
;; Встроенный пакет
(use-package ruby-mode
  :mode
  ("\\Vagrantfile\\'" . ruby-mode))


;; -> SHELL-SCRIPT-MODE
;; Встроенный пакет
(use-package sh-script
  :mode
  ("\\.bashrc\\'" . shell-script-mode)
  ("\\.profile\\'" . shell-script-mode)
  ("\\.sh\\'" . shell-script-mode))


;; -> SQL MODE
;; Это встроенный пакет
(use-package sql
  :config
  (when (fboundp 'lsp-mode)
    (lsp-mode 1))
  :mode
  ("\\.sql\\'" . sql-mode))


;; -> SWIPER MODE
;; https://github.com/abo-abo/swiper
;; Пакет для быстрого поиска.
;; По кажатию C-7 можно выполнить быстрое редактирование найденных фрагментов, но чтобы
;; оно сработало правильно, нужно добавить команду swiper-mc в список mc/cmds-to-run-once.
;;(use-package swiper
;;  :bind
;;   ("C-s" . swiper-isearch)) ;; Заменить стандартный isearch на swiper


;; -> TERRAFORM-MODE
;; https://github.com/emacsorphanage/terraform-mode
;; Работа с файлами конфигурации Terraform
(use-package terraform-mode
  :init
  (setq flycheck-checker 'terraform))


;; -> TOOLBAR-MODE
;; Встроенная панель с кнопками. Не нужна. В EMACS NOX вовсе отсутствует.
;; Перед отключением нужно проверить, что такая функциональность поддерживается.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))


;; -> TOOLTIP-MODE
;; Встроенный режим. Показывает подсказки, используя возможности GUI.
;; Не нужен, поэтому лучше выключить (всё будет отображаться в мини-буфере).
(when (fboundp 'tooltip-mode)
  (tooltip-mode nil))


;; -> TREEMACS — awesome file manager (instead NeoTree)
;; https://github.com/Alexander-Miller/treemacs
;; Дерево файлов и каталогов
(use-package treemacs
  :custom
  (treemacs-width 35 "Ширина окна Treemacs")
  :bind
  ([f8] . treemacs)
  (:map treemacs-mode-map
    ("f" . find-grep))
  :config
  (treemacs-follow-mode 1) ;; При смене буфера TreeMacs сменит позицию в дереве
  (treemacs-git-mode 'simple) ;; Простой режим
  (treemacs-filewatch-mode 1) ;; Отслеживание изменений в ФС на лету
  (define-key treemacs-mode-map (kbd "f") 'find-grep))


;; -> TREEMACS-ALL-THE-ICONS
;; https://github.com/Alexander-Miller/treemacs
;; Поддержка иконок all-the-icons.
(use-package treemacs-all-the-icons
  :after treemacs)


;; -> TREEMACS-ICONS-DIRED
;; Отображать иконки файлов из TreeMacs в `dired-mode'
(use-package treemacs-icons-dired)


;; -> UNDO-TREE
;; https://gitlab.com/tsc25/undo-tree
;; Не только предоставляет привычное поведение при отмене команд, но и даёт мощные возможности по
;; ведению дерева правок.
(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil "Отключить создание резервных копий файлов")
  :config
  (global-undo-tree-mode 1))


;; -> WEB-MODE
;; https://web-mode.org/
(use-package web-mode
  :custom
  (web-mode-attr-indent-offset 4 "Отступ в атрибутов — 4 пробела")
  (web-mode-css-indent-offset 2 "При редактировании CSS отступ будет 2 пробела")
  (web-mode-enable-block-face t "Отображение")
  (web-mode-enable-css-colorization t "Код или имя цвета при редактировании CSS будут отмечены фоном этого цвета")
  (web-mode-enable-current-element-highlight t "Подсветка активного элемента разметки")
  (web-mode-markup-indent-offset 2 "Отступ при вёрстке HTML — 2 пробела")
  :config
  (highlight-indentation-set-offset 2)
  :mode
  ("\\.html\\'" . web-mode))


;; -> WHICH-KEY MODE
;; https://github.com/justbur/emacs-which-key
;; Показывает подсказки к сочетаниям клавиш.
(use-package which-key
  :custom
  (which-key-idle-delay 2 "Задержка появления подсказки")
  (which-key-idle-secondary-delay 0.05 "Ещё одна задержка появления подсказки")
  :config
  (which-key-setup-side-window-right) ;; Показывать подсказки справа
  (which-key-mode 1))


;; -> WHITESPACE MODE
;; Встроенный пакет для отображения невидимых символов.
(use-package whitespace
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
     apt-sources-list-mode
     conf-mode
     css-mode
     dockerfile-mode
     emacs-lisp-mode
     go-mode
     js2-mode
     json-mode
     makefile-mode
     makefile-gmake-mode
     markdown-mode
     nxml-mode
     php-mode
     protobuf-mode
     python-mode
     rst-mode
     ruby-mode
     sh-mode
     sql-mode
     terraform-mode
     web-mode
     yaml-mode) . whitespace-mode))


;; -> WS-BUTLER
(use-package ws-butler
  :hook
  ((
     adoc-mode
     apt-sources-list-mode
     conf-mode
     dockerfile-mode
     emacs-lisp-mode
     go-mode
     js2-mode
     markdown-mode
     nxml-mode
     php-mode
     protobuf-mode
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
  :mode
  ("\\.ansible\\-lint\\'" . yaml-mode)
  ("\\.pre\\-commit\\-config\\.yaml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode)
  ("\\.yamllint\\'" . yaml-mode)
  ("\\.yamllint\\-config\\.yaml\\'" . yaml-mode)
  ("\\.yfm\\'" . yaml-mode)
  ("\\.yml\\'" . yaml-mode))

;; -> YASCROLL-MODE
;; https://github.com/emacsorphanage/yascroll
;; Альтернативная полоса прокрутки
(use-package yascroll
  :config
  (global-yascroll-bar-mode 1))


;; -> YASNIPPET
;; http://github.com/joaotavora/yasnippet
;; Предоставляет функциональность сниппетов — блоков кода, в которые всего-лишь нужно подставить значения.
(use-package yasnippet
  ;; Если директории для сниппектов нет, её нужно создать.
  :config
  (progn
    (defconst yas-snippet-root-dir (expand-file-name emacs-config-dir "snippets"))
    (unless (file-directory-p yas-snippet-root-dir)
      (mkdir yas-snippet-root-dir))
    (yas-global-mode 1)))


;; -> YASNIPPET-SNIPPETS
;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setup-gui-settings (selected-frame))

;; -> CUSTOM FILE
;; Пользовательские настройки, сделанные через CUSTOMIZE
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init.el)
;;; init.el ends here
