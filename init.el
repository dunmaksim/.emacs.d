;;; init.el --- Summary
;;; Commentary:
;;; Main EMACS settings file, load settings from parts.

;;; Code:

(defvar emacs-config-dir (file-name-directory user-init-file) "Корневая директория для размещения настроек.")
(defvar autosave-dir (concat emacs-config-dir "saves") "Директория для файлов автосохранения.")
(defvar backups-dir (concat emacs-config-dir "backups") "Директория для резервных копий.")
(defvar is-gui-mode (display-graphic-p) "EMACS запущен в графическом режиме.")
(defvar is-linux (equal system-type "gnu/linux") "EMACS запущен в GNU/Linux.")
(defvar is-windows (equal system-type "windows-nt") "EMACS запущен в Microsoft Windows.")
(defvar default-font-size 140 "Размер шрифта по умолчанию.")
(defvar default-font "DejaVu Sans Mono" "Шрифт по умолчанию.")
(defvar default-font-family "DejaVu Sans Mono" "Семейство шрифтов по умолчанию.")

;;; Создание каталогов для резервных копий и файлов автосохранения
(unless (file-exists-p autosave-dir)
  (progn
    (make-directory autosave-dir)
    (message "Создана директория для файлов автосохранения.")))

(require 'calendar)
(require 'face-remap)
(require 'ispell)
(require 'widget)
(setq
 auto-save-file-name-transforms `((".*" , autosave-dir) t)
 calendar-week-start-day 1 ; Начнём неделю с понедельника
 create-lockfiles nil ; Не надо создавать lock-файлы, от них одни проблемы
 custom-file (expand-file-name "custom.el" emacs-config-dir)
 delete-old-versions t ; Удалять старые версии файлов
 indent-line-function (quote insert-tab)
 indent-tabs-mode nil ; Отключить выравнивание по TAB
 inhibit-splash-screen t ; Не надо показывать загрузочный экран
 inhibit-startup-message t ; Не надо показывать приветственное сообщение
 initial-buffer-choice (lambda () (get-buffer "*dashboard*")) ; Буфер по умолчанию — дашборд
 initial-major-mode (quote markdown-mode) ; Режим по умолчанию сменим с EMACS Lisp на Markdown
 initial-scratch-message nil ; В новых буферах не нужно ничего писать
 make-backup-files nil ; Резервные копии не нужны, у нас есть undo-tree
 overwrite-mode-binary nil ; Выключить режим перезаписи текста под курсором для бинарных файлов
 overwrite-mode-textual nil ; Выключить режим перезаписи текста под курсором для текстовых файлов
 ring-bell-function #'ignore ; Заблокировать пищание
 save-abbrevs 'silently ; Сохранять аббревиатуры без лишних вопросов
 scroll-bar-mode nil ; Выключить scroll-bar
 suggest-key-bindings t ; Показывать подсказку клавиатурной комбинации для команды
 tab-width 4 ; Обменный курс на TAB — 4 SPACES
 text-scale-mode-step 1.1 ;; Шаг увеличения масштаба
 truncate-lines 1 ; Обрезать длинные строки
 uniquify-buffer-name-style 'forward ; Показывать директорию перед именем файла, если буферы одинаковые (по умолчанию имя<директория>)
 uniquify-separator "/" ; Разделять буферы с похожими именами, используя /
 use-dialog-box nil ; Диалоговые окна не нужны, будем использовать текстовый интерфейс
 user-full-name "Dunaevsky Maxim"
 visible-bell t ;; Заблокировать пищание
 widget-image-enable is-gui-mode;; Разрешить пиктграммы в виджетах
 window-divider-default-places 't ; Разделители окон со всех сторон (по умолчанию только справа)
 window-divider-default-right-width 3
 x-underline-at-descent-line t
 )

;; Правильный способ определить, что EMACS запущен в графическом режиме. Подробнее здесь:
;; https://emacsredux.com/blog/2022/06/03/detecting-whether-emacs-is-running-in-terminal-or-gui-mode/
(add-hook
 'after-make-frame-functions
 (lambda()
   (when (display-graphic-p)
     (
      ;; Код для графики
      ))
   (unless (display-graphic-p)
     (
      ;; Код для терминала
      ))))

(defun load-if-exists (filename)
  "Загрузить файл, если он существует.

  FILENAME — имя файла."
  (if (file-exists-p filename)
      (load-file filename)))

(defun set-minor-mode-for-hooks (minor-mode-name hooks-list)
  "Выполняет установку минорного режима minor-mode-name для списка хуков hooks.

  MINOR-MODE-NAME — имя минорного режима.
  HOOKS-LIST — список хуков, при которых должен активироваться режим."
  (dolist (hook-name hooks-list)
    (add-hook hook-name minor-mode-name)))

;; Aspell для Linux, в Windows без проверки орфографии
(if
    (string-equal system-type "gnu/linux")
    (if
	(file-exists-p "/usr/bin/aspell")
	(setq ispell-program-name "/usr/bin/aspell")))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar generic-packages
  '(
    anaconda-mode
    ansible
    apache-mode
    apt-sources-list
    centaur-tabs
    company
    company-anaconda
    company-terraform
    company-web
    dash
    dashboard
    diff-hl ; https://github.com/dgutov/diff-hl
    direnv
    dockerfile-mode
    doom-modeline ;; https://github.com/seagle0128/doom-modeline
    doom-themes ;; https://github.com/doomemacs/themes
    easy-hugo
    easy-kill ; https://github.com/leoliu/easy-kill
    edit-indirect
    editorconfig
    embark ;; https://github.com/oantolin/embark
    emmet-mode ; https://github.com/smihica/emmet-mode
    flycheck
    flycheck-clang-tidy
    flycheck-color-mode-line
    flycheck-indicator
    format-all
    git-gutter ; https://github.com/emacsorphanage/git-gutter
    go-mode
    helm ; https://github.com/emacs-helm/helm
    helm-ag ; https://github.com/syohex/emacs-helm-ag
    highlight-indentation ; https://github.com/antonj/Highlight-Indentation-for-Emacs
    js2-mode
    json-mode
    lsp-mode ; https://github.com/emacs-lsp
    lsp-ui ; https://github.com/emacs-lsp/lsp-ui
    magit
    markdown-mode
    multiple-cursors
    org
    pandoc-mode ; http://joostkremers.github.io/pandoc-mode/
    php-mode
    projectile
    protobuf-mode
    pulsar ; https://protesilaos.com/emacs/pulsar
    pyenv-mode ; https://github.com/pythonic-emacs/pyenv-mode
    python
    python-mode
    rainbow-delimiters ; https://github.com/Fanael/rainbow-delimiters
    restclient ; https://github.com/pashky/restclient.el
    rg ; https://github.com/dajva/rg.el
    scala-mode
    smartparens ; https://github.com/Fuco1/smartparens
    swiper ; https://github.com/abo-abo/swiper
    terraform-mode
    tide
    treemacs
    treemacs-icons-dired
    treemacs-magit
    typescript-mode
    undo-tree
    verb
    vertico ; https://github.com/minad/vertico
    web-beautify
    web-mode
    wgrep ; https://github.com/mhayashi1120/Emacs-wgrep
    which-key
    ws-butler
    yaml-mode
    yascroll ; https://github.com/emacsorphanage/yascroll

    airline-themes ; THEMES
    base16-theme
    ) "Packages for any EMACS version: console and UI.")

(defvar graphic-packages
  '(
    all-the-icons
    all-the-icons-dired ;; https://github.com/wyuenho/all-the-icons-dired
    all-the-icons-ibuffer ;; https://github.com/seagle0128/all-the-icons-ibuffer
    company-box
    mode-icons ; https://github.com/ryuslash/mode-icons
    treemacs-all-the-icons
    ) "Packages only for graphical mode.")

(if is-gui-mode
    (setq package-selected-packages (append generic-packages graphic-packages))
  (setq package-selected-packages generic-packages))

;; Установка необходимых пакетов
(package-initialize)
(progn
  (message "Проверка наличия необходимых пакетов...")
  (defvar packages-refreshed nil "Список пакетов обновлён.")
  (dolist (pkg package-selected-packages)
    (unless (package-installed-p pkg)
      (unless packages-refreshed
        (progn
          (package-refresh-contents)
          (message "Список доступных пакетов обновлён...")
          (setq packages-refreshed 1)))
      (package-install pkg t))))


;; (setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
;;     projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when
    (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(fset 'yes-or-no-p 'y-or-n-p) ;;; Shortcuts for yes and no


;; Изменение размеров окон
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "<escape>") 'keyboard-quit) ; ESC работает как и Ctrl+g, т. е. прерывает ввод команды
(global-set-key (kbd "C-z") 'undo) ; Отмена
(global-set-key (kbd "C-x k") 'kill-this-buffer) ; Закрыть буфер
;; (global-set-key (kbd "<C-tab>") 'mode-line-other-buffer) ; Перейти в другой буфер

(global-set-key (kbd "C-v") 'yank) ; Вставить текст из временного буфера
(global-set-key (kbd "C-x o") 'next-multiframe-window) ; Перейти в следующее окно
(global-set-key (kbd "C-x O") 'previous-multiframe-window) ; Перейти в предыдущее окно


;;; Save user settings in dedicated file
(setq custom-file (expand-file-name "settings.el" emacs-config-dir))
(load-if-exists custom-file)

;; Если EMACS запущен в графическом режиме, нужно настроить шрифты.
(when is-gui-mode
  (message "Настройка интерфейса.")
  (defvar availiable-fonts (font-family-list)) ;; Какие есть семейства шрифтов?

  (when is-linux
    (message "Это Linux")
    ;; Если каталог не существует, установить шрифты
    (unless (file-directory-p "~/.local/share/fonts")
      (all-the-icons-install-fonts))

    (cond
     (
      (member "DejaVu Sans Mono" availiable-fonts)
      (setq
       default-font-family "DejaVu Sans Mono"
       default-font "DejaVu Sans Mono"))
     (
      (member "FiraCode" availiable-fonts)
      (setq
       default-font-family "FiraCode"
       default-font "FiraCode"))
     (
      (member "Source Code Pro" availiable-fonts)
      (setq
       default-font-family "Source Code Pro"
       default-font "Source Code Pro")))) ;; /is-linux
  ;; /Linux

  (when is-windows
    (message "Это Windows.")
    (message "Скачайте и установите шрифты с помощью команды all-the-icons-install-fonts.")
    (cond
     (
      (member "Consolas" availiable-fonts)
      (setq
       default-font-family "Consolas"
       default-font "Consolas"))
     (
      (member "Courier New" availiable-fonts)
      (setq
       default-font-family "Courier New"
       default-font "Courier New"))))
  ;; /Windows

  ;; Настройка иконочных шрифров и немножко GUI.
  (require 'all-the-icons)
  (require 'all-the-icons-dired)
  (require 'all-the-icons-ibuffer)
  (require 'mode-icons)
  (setq
   all-the-icons-ibuffer-human-readable-size t ;; Показывать размер файлов в ibuffer в человекочитаемом виде
   all-the-icons-ibuffer-icon t
   )
  (all-the-icons-ibuffer-mode t)
  (mode-icons-mode t)
  (tooltip-mode nil) ;; Убрать всплывающие подсказки в tooltip'ах при наведении мыши

  ;; Настройки шрифтов
  (message "Настройки шрифтов")
  (set-face-attribute
   'default nil
   :font default-font
   :family default-font-family
   :height default-font-size
   )
  ) ;; /when is gui-mode


;; Auto-revert mode
(global-auto-revert-mode 1)


;; ENCODING
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)


;; Settings for hotkeys on any layout
(require 'quail)
(defun cfg:reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (if input-method
	(activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(cfg:reverse-input-method 'russian-computer)


(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let
      (
       ($buf
        (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))


;; Save/close/open
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-r") 'revert-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-'") 'comment-or-uncomment-region)
(global-set-key (kbd "C-o") 'dired)


(global-set-key (kbd "<f7>") 'xah-new-empty-buffer) ;; Buffers and windows
(global-set-key (kbd "<f9>") 'sort-lines) ;; Sort lines
(global-set-key (kbd "<esc>") 'keyboard-quit) ;; Execute commands — like [g]
(global-set-key (kbd "<f3>") 'replace-string)

;; Switch windows with C-x and arrow keys
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(global-set-key (kbd "M--") (lambda () (interactive) (insert "—"))) ;; Long dash by Alt+-

(global-unset-key (kbd "<insert>")) ;; Disable overwrite mode
(global-unset-key (kbd "M-,")) ;; Disable M-, as markers

(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))

;; ABBREV-MODE
;; Работа с аббревиатурами. Безо всяких нажатий клавиш текст будет заменяться на расширенную фразу
(require 'abbrev)
(set-minor-mode-for-hooks
 'abbrev-mode
 '(markdown-mode-hook))

;; ACE-WINDOW
;; https://github.com/abo-abo/ace-window
(message "Загрузка пакета ace-window")
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)


;; ANSIBLE MODE
(message "Загрузка пакета ansible-mode")
(require 'ansible)
(defun setup-ansible-mode ()
  "Settings for 'ansible-mode'."
  (interactive)
  (company-mode 1))
(add-hook 'ansible-mode-hook #'setup-ansible-mode)

;; APT SOURCES LIST MODE
;; https://git.korewanetadesu.com/apt-sources-list.git
(message "Загрузка пакета apt-sources-list-mode")
(defun setup-apt-sources-list-mode ()
  "Settings for 'apt-sources-list-mode'."
  (interactive)
  (company-mode 1)
  (diff-hl-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (visual-line-mode 1)
  (whitespace-mode 1))
(add-to-list 'auto-mode-alist (cons "\\.list\\'" 'apt-sources-list-mode))
(add-hook 'apt-sources-list-mode #'setup-apt-sources-list-mode)


;; CENTAUR-TABS
;; https://github.com/ema2159/centaur-tabs
;; Вкладки с иконками и прочими удобствами
(require 'centaur-tabs)
(setq
 centaur-tabs-enable-key-bindings t; Включить комбинации клавиш из `centaur-tabs`.
 centaur-tabs-close-button "×" ; Будем использовать вот этот символ вместо X
 centaur-tabs-height 48 ; Высота вкладок
 centaur-tabs-modified-marker t ; Показывать маркер, если содержимое вкладки изменилось
 centaur-tabs-set-bar 'under ; Доступные значения: over, under
 centaur-tabs-set-icons is-gui-mode ; Включить иконки. если это графический режим
 centaur-tabs-style "slant" ; Также доступны: bar, alternate, box, chamfer, rounded, slant, wawe, zigzag
 x-underline-at-descent-line t ; Если пакет используется вне Spacemacs, необходимо включить это, чтобы подчёркивание отображалось корректно
 )
(centaur-tabs-mode 1)
(if is-gui-mode ;; В терминале эти клавиши перехватываются и не будут работать
    (progn
      (global-set-key (kbd "C-<prior>") 'centaur-tabs-backward)
      (global-set-key (kbd "C-<next>") 'centaur-tabs-forward)))


;; COLUMN NUMBER MODE
;; BUILTIN
;; Show column number in modeline
(column-number-mode 1)


;; COMPANY-MODE
;; https://company-mode.github.io/
(message "Загрузка пакета company.")
(require 'company)
(require 'company-dabbrev)
(setq
 company-dabbrev-ignore-case nil
 company-dabbrev-downcase nil
 company-dabbrev-ignore-case nil
 company-idle-delay 0
 company-minimum-prefix-length 2
 company-tooltip-align-annotations t)
(set-minor-mode-for-hooks
 'company-mode
 '(
   conf-mode-hook
   markdown-mode-hook
   python-mode-hook
   ruby-mode-hook
   terraform-mode-hook
   xml-mode-hook
   nxml-mode-hook))


;; COMPANY-BOX
;; https://github.com/sebastiencs/company-box
;; Расширение для company-mode, которое показывает доступные варианты, используя иконки
(message "Загрузка пакета company-box.")
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)


;; COMPANY-WEB
;; Автодополнение для режима web-mode
(message "Загрузка пакета company-web.")
(require 'company-web)
(require 'company-web-html)
(add-to-list 'company-backends 'company-web-html)


;; CONF MODE
;; Основной режим для редактирования конфигурационных файлов.
(message "Загрузка пакета conf-mode.")
(require 'conf-mode)
(defun setup-conf-mode ()
  "Settings for 'conf-mode'."
  (display-line-numbers-mode 1)
  (whitespace-mode 1))
(add-hook 'conf-mode-hook #'setup-conf-mode)
(add-to-list
 'auto-mode-alist
 '("\\.\\(?:env\\|flake8\\|ini\\|list\\|pylintrc\\|terraformrc\\)\\'" . conf-mode))


;; Dashboard
;; https://github.com/emacs-dashboard/emacs-dashboard
;; Отображает дашборд при запуске EMACS
(message "Загрузка пакета dashboard.")
(require 'dashboard)
(setq
 dashboard-items
 '(
   (recents . 15) ;; Последние открытые файлы
   (bookmarks . 10) ;; Последние закладки
   (projects . 10) ;; Последние проекты
   (agenda . 10) ;; Агенда
   (registers . 5)) ;; Регистры
 dashboard-set-heading-icons is-gui-mode ;; Иконка EMACS в графическом режиме
 dashboard-set-file-icons is-gui-mode) ;; Иконки типов файлов в графическом режиме
(add-hook 'dashboard-mode-hook 'centaur-tabs-local-mode)
(dashboard-setup-startup-hook)


;; DELETE SELECTION MODE
;; BUILTIN
;; Delete selected region
(delete-selection-mode 1)


;; DESKTOP-SAVE-MODE
(message "Загрузка пакета desktop.")
(require 'desktop)
(setq
 desktop-save t
 desktop-modes-not-to-save
 '(
   dired-mode
   Info-mode
   info-lookup-mode))
(desktop-save-mode 1)


;; DIFF-HL
;; https://github.com/dgutov/diff-hl
;; Показывает небольшие маркеры рядом с незафиксированными изменениями. Дополняет функциональность git-gutter,
;; которые показывает изменения только в обычных буферах. Этот пакет умеет работать с dired и другими режимами.
(require 'diff-hl)
(global-diff-hl-mode 1)


;; DIRED
(message "Загрузка пакета dired.")
(require 'dired)
(when (string-equal system-type "gnu/linux")
  ;; Это может не работать в Windows, надо проверить
  (setq dired-listing-switches "-lahX --group-directories-first"))
(defun setup-dired-mode ()
  "Settings for 'dired-mode'."
  (auto-revert-mode 1)
  (hl-line-mode 1))
(add-hook 'dired-mode-hook #'setup-dired-mode)
(add-hook 'dired-mode-hook #'centaur-tabs-local-mode)

;; DIRENV-MODE
;; https://github.com/wbolster/emacs-direnv
(message "Загрузка пакета direnv.")
(require 'direnv)
(setq direnv-use-faces-in-summary nil)
(direnv-mode 1)


;; DISPLAY-LINE-NUMBERS-MODE
;; Встроенный пакет
(message "Загрузка пакета display-line-numbers-mode.")
(require 'display-line-numbers)
(display-line-numbers-mode 1)


;; DOCKERFILE-MODE
(message "Загрузка пакета dockerfile-mode")
(require 'dockerfile-mode)
(defun setup-dockerfile-mode ()
  "Settings for 'dockerfile-mode'."
  (company-mode 1)
  (whitespace-mode 1))
(add-to-list 'auto-mode-alist '("Dockerfile'" . dockerfile-mode))
(add-hook 'dockerfile-mode-hook #'setup-dockerfile-mode)


;; DOOM-MODELINE
;; https://github.com/seagle0128/doom-modeline
(message "Загрузка пакета doom-modeline.")
(require 'doom-modeline)
(setq
 doom-modeline-buffer-encoding t ;; Кодировка
 doom-modeline-buffer-modification-icon t ;; Наличие изменений
 doom-modeline-buffer-state-icon t
 doom-modeline-hud is-gui-mode
 doom-modeline-icon is-gui-mode ;; Иконки?
 doom-modeline-lsp t
 doom-modeline-major-mode-color-icon t
 doom-modeline-major-mode-icon is-gui-mode
 doom-modeline-project-detection 'auto
 doom-modeline-vcs-max-length 0)
(doom-modeline-mode 1)


;; LOAD THEME
(message "Загрузка темы.")
(require 'doom-themes)
(load-theme 'doom-monokai-classic t)
;; (load-theme 'doom-one t)
(doom-themes-org-config)
(doom-themes-visual-bell-config)


;; EASY KILL
;; https://github.com/leoliu/easy-kill
;; Удобнее работать с удалением текстовых блоков
(message "Загрузка пакета easy-kill.")
(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)


;; EDITORCONFIG EMACS
;; https://github.com/editorconfig/editorconfig-emacs
(message "Загрузка пакета editorconfig.")
(require 'editorconfig)
(setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
(editorconfig-mode 1)


;; ELECTRIC-PAIR-MODE
;; EMBEDDED
(message "Загрузка пакета electric-pair-mode.")
(require 'electric)
(electric-pair-mode 1)


;; EMACS LISP MODE
;; IT IS NOT A ELISP-MODE!
;; Встроенный пакет для EMACS Lisp
(message "Загрузка пакета emacs-lisp-mode.")
(defun setup-emacs-lisp-mode ()
  "Settings for EMACS Lisp Mode."
  (interactive)
  (company-mode 1)
  (diff-hl-mode)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'emacs-lisp-mode-hook #'setup-emacs-lisp-mode)
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\abbrev_defs" . emacs-lisp-mode))


;; EMBARK
;; https://github.com/oantolin/embark
(message "Загрузка пакета embark.")
(require 'embark)
(setq prefix-help-command #'embark-prefix-help-command)


;; EMMET MODE
;; https://github.com/smihica/emmet-mode
;; Позволяет быстро писать HTML-, CSS и XML-теги, например:
;;
;; Пример:
;;
;; div>p>ul>li*3
;;
;; по нажатию Ctrl+J превратится в такой код
;;
;; <div>
;;   <p>
;;     <ul>
;;       <li></li>
;;       <li></li>
;;       <li></li>
;;     </ul>
;;   </p>
;; </div>
(message "Загрузка пакета emmet-mode")
(require 'emmet-mode)
(set-minor-mode-for-hooks
 'emmet-mode
 '(
   css-mode-hook
   html-mode-hook
   nxml-mode-hook
   xml-mode-hook
   web-mode-hook
   ))


;; FLYCHECK
;; https://flycheck.org
;; Проверка синтаксиса на лету с помощью статических анализаторов
(message "Загрузка пакета flycheck")
(require 'flycheck)
(require 'flycheck-color-mode-line) ;; https://github.com/flycheck/flycheck-color-mode-line
(require 'flycheck-indicator)
(setq
 flycheck-check-syntax-automatically '(mode-enabled save new-line)
 flycheck-locate-config-file-functions
 '(
   flycheck-locate-config-file-by-path
   flycheck-locate-config-file-ancestor-directories
   flycheck-locate-config-file-home)
 flycheck-highlighting-mode 'lines
 flycheck-indication-mode 'left-fringe
 flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc"
 )
(defun setup-flycheck-mode ()
  "Minor modes for 'flycheck-mode'."
  (interactive)
  (flycheck-color-mode-line-mode 1)
  (flycheck-indicator-mode 1)
  )
(add-hook 'flycheck-mode-hook #'setup-flycheck-mode)
(set-minor-mode-for-hooks
 'flycheck-mode
 '(
   apt-sources-list-mode-hook
   conf-mode-hook
   dockerfile-mode-hook
   emacs-lisp-mode-hook
   go-mode-hook
   java-mode-hook
   javascript-mode-hook
   js2-mode-hook
   json-mode-hook
   makefile-mode-hook
   markdown-mode-hook
   nxml-mode-hook
   php-mode-hook
   protobuf-mode-hook
   python-mode-hook
   rst-mode-hook
   ruby-mode-hook
   scala-mode-hook
   shell-script-mode-hook
   sql-mode-hook
   terraform-mode-hook
   tide-mode-hook
   web-mode-hook
   xml-mode-hook
   yaml-mode-hook
   ))


;; FORMAT ALL
;; https://github.com/lassik/emacs-format-all-the-code
;; Форматирование кода по нажатию [F12]
(message "Загрузка пакета format-all.")
(require 'format-all)
(global-set-key (kbd "<f12>") 'format-all-buffer)


;; GIT-GUTTER
;; https://github.com/emacsorphanage/git-gutter
(message "Загрузка пакета git-gutter")
(require 'git-gutter)
(setq git-gutter:visual-line t)
(global-git-gutter-mode 1)


;; GO-MODE
;; https://github.com/dominikh/go-mode.el
;; Поддержка Golang
(message "Загрузка пакета go-mode")
(require 'go-mode)
(defun setup-go-mode()
  "Settings for 'go-mode'."
  (interactive)
  (abbrev-mode 1)
  (buffer-face-mode 1)
  (company-mode 1)
  (diff-hl-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (visual-line-mode 1) ;; Highlight current line
  (whitespace-mode 1)) ;; Show spaces, tabs and other
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook #'setup-go-mode)


;; HELM
;; Подсказки в минибуфере
(message "Загрузка пакета helm.")
(require 'helm)
(setq completion-styles '(flex))
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-S-p") 'helm-M-x)
(helm-mode 1)


;; HELM-AG
;; https://github.com/syohex/emacs-helm-ag
;; Поиск и замена строк в нескольких файлах.
(message "Загрузка пакета helm-ag.")
(require 'helm-ag)


;; GLOBAL HL MODE
;; Подсвечивает текущую строку
(global-hl-line-mode 1)


;; IBUFFER
;; Встроенный пакет для удобной работы с буферами.
;; По нажатию F2 выводит список открытых буферов.
(message "Загрузка пакета ibuffer")
(require 'ibuffer)
(require 'ibuf-ext)
(defalias 'list-buffers 'ibuffer)
(setq
 ibuffer-expert 1
 ibuffer-hidden-filter-groups (list "Helm" "*Internal*")
 ibuffer-saved-filter-groups
 '(
   ("default"
    ("Dired"
     (mode . dired-mode))
    ("Org"
     (mode . org-mode))
    ("Markdown"
     (mode . markdown-mode))
    ("EMACS Lisp"
     (mode . emacs-lisp-mode))
    ("XML"
     (mode . xml-mode)
     (mode . nxml-mode))
    ("YAML"
     (mode . yaml-mode))
    ("Protobuf"
     (mode . protobuf-mode))
    ("Golang"
     (mode . go-mode))
    ("Python"
     (or
      (mode . python-mode)
      (mode . elpy-mode)
      (mode . anaconda-mode)))
    ("SSH keys"
     (or
      (name . "^\\*.pub")))
    ("Shell-script"
     (or
      (mode . shell-script-mode)
      (mode . sh-mode)))
    ("Terraform"
     (or
      (mode . terraform-mode)))
    ("SQL"
     (or
      (mode . sql-mode)))
    ("Web"
     (or
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
      (mode . shell-mode)
      (mode . eshell-mode)
      (mode . term-mode)
      (mode . compilation-mode)))
    ("Emacs"
     (or
      (name . "^\\*scratch\\*$")
      (name . "^\\*Messages\\*$")
      (name . "^\\*\\(Customize\\|Help\\)")
      (name . "\\*\\(Echo\\|Minibuf\\)")))))
 ibuffer-show-empty-filter-groups nil ;; Do not show empty groups
 ibuffer-truncate-lines nil
 ibuffer-sorting-mode 'filename/process
 ibuffer-formats
 '(
   (
    mark
    modified
    read-only
    locked
    " "
    (name 30 40 :left :elide)
    " "
    (mode 8 -1 :left)
    " "
    filename-and-process)
   (
    mark
    " "
    (name 32 -1)
    " "
    filename))
 ibuffer-use-other-window nil)

(defun setup-ibuffer-mode ()
  "Settings for 'ibuffer-mode'."
  (interactive)
  (hl-line-mode 1)
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))
(global-set-key (kbd "<f2>") 'ibuffer)
(add-hook 'ibuffer-mode-hook #'setup-ibuffer-mode)


;; JAVA-MODE
(defun setup-java-mode ()
  "Settings for 'java-mode'."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'java-mode-hook #'setup-java-mode)


;; JS2-MODE
;; https://github.com/mooz/js2-mode
(message "Загрузка пакета js2-mode.")
(require 'js2-mode)
(defun setup-js2-mode ()
  "Settings for 'js2-mode'."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'javascript-mode-hook #'setup-js2-mode)
(add-hook 'js2-mode-hook #'setup-js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;; JSON-MODE
;; https://github.com/joshwnj/json-mode
;; Работа с JSON
(message "Загрузка пакета json-mode.")
(require 'json)
(defun setup-json-mode()
  "Settings for json-mode."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-to-list 'auto-mode-alist '("\\.\\(?:json\\|bowerrc\\|jshintrc\\)\\'" . json-mode))
(add-hook 'json-mode-hook #'setup-json-mode)


;; LSP MODE
;; https://emacs-lsp.github.io/lsp-mode/
;; Базовый пакет, необходимый для работы LSP
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
;; MARKDOWN: npm install -g remark-language-server
;; NXML: lsp-install-server, выбрать xmlls
;; SQL: go install github.com/lighttiger2505/sqls@latest
;; XML: lsp-install-server, выбрать xmlls
;; YAML: npm install -g yaml-language-server
(message "Загрузка пакета lsp-mode.")
(require 'lsp-mode)
(setq
 lsp-sqls-workspace-config-path nil ; Работать с любыми SQL-командами
 lsp-headerline-breadcrumb-enable t ; Показывать "хлебные крошки" в заголовке
 lsp-modeline-diagnostics-enable t ; Показывать ошибки LSP в статусной строке
 )
(set-minor-mode-for-hooks
 'lsp
 '(
   dockerfile-mode-hook
   go-mode-hook
   json-mode-hook
   makefile-mode-hook
   markdown-mode-hook
   nxml-mode-hook
   sql-mode-hook
   xml-mode-hook
   yaml-mode-hook
   ))


;; MAGIT
;; https://magit.vc/
;; Magic + Git + Git-gutter. Лучшее средство для управления Git.
(message "Загрузка пакета magit.")
(require 'magit)
(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'magit-checkout)


;; Makefile
(defun setup-makefile-mode ()
  "Settings for Makefile-mode."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (highlight-indentation-mode 1)
  (hl-line-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'makefile-mode-hook #'setup-makefile-mode)


;; MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
(message "Загрузка пакета markdown-mode.")
(require 'markdown-mode)
(setq
 header-line-format " "
 left-margin-width 4
 markdown-list-indent-width 4
 markdown-fontify-code-blocks-natively t ; Подсвечивать синтаксис в примерах кода
 markdown-header-scaling-values '(1.0 1.0 1.0 1.0 1.0 1.0)
 word-wrap t)
(set-face-attribute 'markdown-inline-code-face nil :family default-font-family)
(defun setup-markdown-mode()
  "Settings for editing markdown documents."
  (interactive)
  ;; Additional modes
  (abbrev-mode 1)
  (buffer-face-mode 1)
  (diff-hl-mode 1)
  (display-line-numbers-mode 1)
  (electric-pair-mode 1)
  (highlight-indentation-mode 1)
  (hl-line-mode 1)
  (pandoc-mode 1)
  (rainbow-delimiters-mode 1)
  (visual-line-mode 1) ;; Highlight current line
  (whitespace-mode 1) ;; Show spaces, tabs and other
  (cond ;; Turn on spell-checking only in Linux
   (
    (string-equal system-type "gnu/linux")
    (flyspell-mode 1))))
(define-key markdown-mode-map (kbd "M-.") 'markdown-follow-thing-at-point)
(add-to-list
 'auto-mode-alist
 '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'setup-markdown-mode)


;; Turn off menu bar
(require 'menu-bar)
(menu-bar-mode -1)


;; MULTIPLE CURSORS
(message "Загрузка пакета multiple-cursors.")
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-e") 'mc/edit-lines)
(if is-gui-mode
    (progn
      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)))


;; NXML-MODE
;; Почти как xml-mode, только лучше и новее
(message "Загрузка пакета nxml-mode.")
(require 'nxml-mode)
(defun setup-nxml-mode ()
  "Настройка режима `nxml-mode'."
  (interactive)
  (abbrev-mode 1)
  (company-mode 1)
  (whitespace-mode 1))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))


;; ORG-MODE
;; https://orgmode.org/
;; Органайзер, и не только
(message "Загрузка пакета org-mode.")
(require 'org)
(setq
 truncate-lines nil
 left-margin-width 4
 org-todo-keywords '((
                      sequence
                      "НОВАЯ"
                      "РАСПАКОВКА"
                      "ПРИОСТАНОВЛЕНА"
                      "В РАБОТЕ"
                      "НУЖНА ИНФОРМАЦИЯ"
                      "РЕВЬЮ"
                      "|"
                      "ВЫПОЛНЕНА"))
 right-margin-width 4
 word-wrap t)
(defun setup-org-mode ()
  "Minor modes for 'org-mode'."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook #'setup-org-mode)


;; PANDOC-MODE
;; http://joostkremers.github.io/pandoc-mode/
;; Позволяет экспортировать текст из одного формата в другой
;; Подробнее на странице проекта: https://pandoc.org/
(message "Загрузка пакета pandoc-mode.")
(require 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)


;; PHP-MODE
(message "Загрузка пакета php-mode.")
(require 'php)
(defun setup-php-mode ()
  "Minor modes for 'php-mode'."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'php-mode-hook #'setup-php-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))


;; PROJECTILE
;; Управление проектами
(message "Загрузка пакета projectile.")
(require 'projectile)
(load-if-exists "projects.el")


;; PROTOBUF-MODE
;; https://github.com/emacsmirror/protobuf-mode
;; Работа с файлами Protobuf: подсветка синтаксиса, переход по ссылками и т. д.
(message "Загрузка пакета protobuf-mode.")
(require 'protobuf-mode)
(defun setup-protobuf-mode ()
  "Settings for 'protobuf-mode'."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'protobuf-mode-hook #'setup-protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))


;; PULSAR
;; https://protesilaos.com/emacs/pulsar
;; Подсвечивает текущую строку при наступлении различных событий: смена или закрытие буфера, переход по ссылке и т. д.
(message "Загрузка пакета pulsar.")
(require 'pulsar)
(pulsar-global-mode 1)


;; PYTHON-MODE
(message "Загрузка пакета python.")
(require 'python)
(setq
 doom-modeline-env-enable-python t
 ;; py-company-pycomplete-p t
 ;; py-electric-comment-p t
 ;; py-pylint-command-args "--max-line-length 120"
 ;; py-virtualenv-workon-home "~/.virtualenvs"
 python-shell-interpreter "python3")
(defun setup-python-mode ()
  "Settings for 'python-mode'."
  (interactive)
  (anaconda-mode 1)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (highlight-indentation-mode 1)
  (hl-line-mode 1)
  (lsp-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (define-key python-mode-map (kbd "M-/") 'jedi:show-doc)
  (define-key python-mode-map (kbd "M-?") 'helm-jedi-related-names))
(add-hook 'python-mode-hook #'lsp)
(add-hook 'python-mode-hook #'setup-python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


;; RST-MODE
;; Основной режим для редактирования reStructutedText
;; Больше здесь: https://www.writethedocs.org/guide/writing/reStructuredText/
(message "Загрузка пакета rst-mode.")
(require 'rst)
(defun setup-rst-mode ()
  "Settings for 'rst-mode'."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (pandoc-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'rst-mode-hook #'setup-rst-mode)
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))


;; RUBY-MODE
;;Поддержка Ruby on Rails
(message "Загрузка пакета ruby-mode.")
(require 'ruby-mode)
(defun setup-ruby-mode ()
  "Settings for 'ruby-mode'."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (highlight-indentation-mode 1)
  (hl-line-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'ruby-mode-hook #'setup-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb\\'" .ruby-mode))


;; PAREN-MODE
;; Редактирование парных скобок
(message "Загрузка пакета paren.")
(require 'paren)
(show-paren-mode t)


;; REST-CLIENT_MODE
;; https://github.com/pashky/restclient.el
;; Старый пакет для работы с REST API, более удобная замена — verb
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))


;; RG (ripgrep
;; https://github.com/dajva/rg.el
(message "Загрузка пакета rg.")
(require 'rg)
(rg-enable-default-bindings)


;; SAVE-PLACE-MODE
;; https://www.emacswiki.org/emacs/SavePlace
;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file.
(message "Загрузка пакета saveplace.")
(require 'saveplace)
(setq save-place-file (expand-file-name ".emacs-places" emacs-config-dir)
      save-place-forget-unreadable-files 1)
(save-place-mode 1)


;; SCALA MODE
;; https://github.com/hvesalai/emacs-scala-mode
(message "Загрузка пакета scala-mode.")
(require 'scala-mode)
(defun setup-scala-mode ()
  "Settings for 'scala-mode'."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'scala-mode-hook #'setup-scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))


;; SHELL-SCRIPT-MODE
(message "Загрузка пакета shell-script-mode.")
(require 'sh-script)
(defun setup-shell-script-mode ()
  "Settings for 'shell-script-mode'."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  ;; (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))
(add-hook 'shell-script-mode #'setup-shell-script-mode)
(add-hook 'sh-mode-hook #'setup-shell-script-mode)


;; SMARTPARENS
;; https://github.com/Fuco1/smartparens
;; Умная работа с парными скобками (и не только)
(message "Загрузка пакета smartparens.")
(require 'smartparens-config)
(smartparens-global-mode 1)


;; SQL MODE
(message "Загрузка пакета sql-mode.")
(require 'sql)
(defun setup-sql-mode ()
  "Settings for SQL-mode."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
(add-hook 'sql-mode-hook #'setup-sql-mode)


;; SWIPER MODE
;; https://github.com/abo-abo/swiper
;; Пакет для быстрого поиска
;; По кажатию C-7 можно выполнить быстрое редактирование найденных фрагментов, но чтобы
;; оно сработало правильно, нужно добавить команду swiper-mc в список mc/cmds-to-run-once
(require 'swiper)
;; (add-to-list 'mc/cmds-to-run-once 'swiper-mc)
(global-set-key (kbd "C-f") 'swiper-isearch)


;; TIDE-MODE (Typescript IDE)
;; https://github.com/ananthakumaran/tide/
(message "Загрузка пакета tide.")
(require 'tide)
(defun setup-tide-mode ()
  "Settings for 'tide-mode'."
  (interactive)
  (company-mode 1)
  (eldoc-mode 1)
  (rainbow-delimiters-mode 1)
  (tide-hl-identifier-mode 1)
  (tide-setup)
  (whitespace-mode 1))
(add-hook 'tide-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tide-mode))
(add-to-list 'auto-mode-alist '("\\.d.ts\\'" . tide-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" .tide-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . tide-mode))


;; TERRAFORM-MODE
;; https://github.com/emacsorphanage/terraform-mode
;; Работа с файлами конфигурации Terraform
(message "Загрузка пакета terraform-mode.")
(require 'terraform-mode)
(with-eval-after-load "terraform-mode"
  (setq flycheck-checker 'terraform))
(defun setup-terraform-mode ()
  "Settings for terraform-mode."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-to-list 'auto-mode-alist (cons "\\.tf\\'" 'terraform-mode))
(add-hook 'terraform-mode-hook #'setup-terraform-mode)
(add-hook 'hcl-mode-hook #'setup-terraform-mode)


;; TEXT MODE
;; Просто указываю, что вместо TAB'ов надо использовать пробелы. Почему-то настройки выше игнорируются.
(require 'text-mode)
(add-hook
 'text-mode-hook
 '(lambda ()
    (setq
     indent-tabs-mode nil
     tab-width 4)
    (whitespace-mode 1)
    (display-line-numbers-mode 1)))


;; (NO TOOLTIP MODE
;; Не надо показывать подсказки с использованием GUI, для этого есть минибуфер.
(tooltip-mode 0)


;; TOOL-BAR-MODE OFF
;; Панель инструментов не нужна, всё через клавиатурные комбинации.
(tool-bar-mode -1)


;; TREEMACS — awesome file manager (instead NeoTree)
;; https://github.com/Alexander-Miller/treemacs
;; Дерево файлов и каталогов
(message "Загрузка пакета treemacs.")
(require 'treemacs)
(setq treemacs-width 35)
(defun treemacs-get-ignore-files (filename absolute-path)
  "Функция позволяет игнорировать некоторые имена файлов и каталогов при построении дерева. Подробнее см. в документации пакета.

  FILENAME — имя файла.
  ABSOLUTE-PATH — абсолютный путь."
  (or
   (string-equal filename ".emacs.desktop.lock")
   (string-equal filename "__pycache__")))
(add-to-list 'treemacs-ignored-file-predicates #'treemacs-get-ignore-files)
(define-key treemacs-mode-map (kbd "f") 'projectile-grep)
(global-set-key (kbd "<f8>") 'treemacs)
(treemacs-follow-mode 1)
(treemacs-git-mode 'simple)
(treemacs-filewatch-mode 1)


;; TREEMACS-ICONS-DIRED
;; Отображать иконки файлов из  TreeMacs в dired-mode
(message "Загрузка пакета treemacs-icons")
(require 'treemacs-icons)
(add-hook 'dired-mode-hook 'treemacs-icons-dired-enable-once)


;; UNDO-TREE
(require 'undo-tree)
(setq undo-tree-auto-save-history nil) ; Не срать в рабочий каталог! Не нужны мне твои backup'ы!
(global-undo-tree-mode 1)


;; VERB-MODE
;; Удобная работа с REST API
;; https://github.com/federicotdn/verb
(require 'verb)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))


;; VERTICO
;; https://github.com/minad/vertico
;; Автодополнение на основе встроенной функциональности EMACS
(require 'vertico)
()

;; WEB-MODE
;; https://web-mode.org/
(message "Загрузка пакета web-mode.")
(require 'web-mode)
(setq
 web-mode-attr-indent-offset 4
 web-mode-css-indent-offset 2 ;; CSS
 web-mode-enable-block-face t
 web-mode-enable-css-colorization t
 web-mode-enable-current-element-highlight t
 web-mode-markup-indent-offset 2)
(defun setup-web-mode()
  "Settings for web-mode."
  (interactive)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-to-list
 'auto-mode-alist
 '("\\.\\(?:css\\|djhtml\\|html\\)\\'" . web-mode))
(add-hook 'web-mode-hook #'setup-web-mode)


;; WGREP
;; https://github.com/mhayashi1120/Emacs-wgrep
;; Поиск и замена по нескольким файлам
(require 'wgrep)


;; WHICH-KEY MODE
;; https://github.com/justbur/emacs-which-key
;; Показывает подсказки к командам.
(require 'which-key)
(which-key-setup-side-window-right)
(which-key-mode 1)


;; WHITESPACE MODE
;; https://www.emacswiki.org/emacs/WhiteSpace
(message "Загрузка пакета whitespace.")
(require 'whitespace)
(setq
 whitespace-display-mappings
 '(
   (space-mark   ?\    [?\xB7]     [?.]) ; space
   (space-mark   ?\xA0 [?\xA4]     [?_]) ; hard space
   (newline-mark ?\n   [?¶ ?\n]    [?$ ?\n]) ; end of line
   (tab-mark     ?\t   [?\xBB ?\t] [?\\ ?\t]) ; tab
   )
 whitespace-line-column 1000) ;; Highlight lines with length bigger than 1000 chars)
(set-face-attribute
 'whitespace-space nil
 :family default-font-family
 :foreground "#75715E")
(set-face-attribute
 'whitespace-indentation nil
 :family default-font-family
 :foreground "#E6DB74")
(dolist
    (mode-name-hook
     '(
       go-mode-hook
       markdown-mode-hook
       xml-mode-hook
       nxml-mode-hook
       python-mode-hook
       php-mode-hook
       ruby-mode-hook
       terraform-mode-hook
       ))
  (add-hook mode-name-hook 'whitespace-mode))


;; WINDOW-DIVIDER-MODE
;; Встроенный режим, управляющий визуальным разделением окон EMACS
(window-divider-mode 1)


;; WS-BUTLER MODE
;; https://github.com/lewang/ws-butler
;; Чистит висячие пробелы только в измененных строках.
(message "Загрузка пакета ws-butler.")
(require 'ws-butler)
(set-minor-mode-for-hooks
 'ws-butler-mode
 '(
   apt-sources-list-mode-hook
   conf-mode-hook
   dockerfile-mode-hook
   emacs-lisp-mode-hook
   go-mode-hook
   java-mode-hook
   js2-mode-hook
   json-mode-hook
   markdown-mode-hook
   nxml-mode-hook
   org-mode-hook
   php-mode-hook
   protobuf-mode-hook
   python-mode-hook
   rst-mode-hook
   ruby-mode-hook
   scala-mode-hook
   sh-mode-hook
   shell-script-mode-hook
   sql-mode-hook
   terraform-mode-hook
   tide-mode-hook
   web-mode-hook
   xml-mode-hook
   yaml-mode-hook))

;; YAML-MODE
;; https://github.com/yoshiki/yaml-mode
(require 'yaml-mode)
(defun setup-yaml-mode ()
  "Settings for yaml-mode."
  (interactive)
  (company-mode 1)
  (diff-hl-mode 1)
  (display-line-numbers-mode 1)
  (highlight-indentation-mode 1)
  (hl-line-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-to-list
 'auto-mode-alist
 '("\\.\\(?:yaml\\|yfm\\|yml\\)\\'" . yaml-mode))
(add-hook 'yaml-mode-hook #'setup-yaml-mode)


;; YASCROLL-MODE
;; https://github.com/emacsorphanage/yascroll
;; Альтернативная полоса прокрутки
(message "Загрузка пакета yascroll-mode")
(require 'yascroll)
(global-yascroll-bar-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(load-if-exists custom-file)

(provide 'init.el)

;;; init.el ends here
