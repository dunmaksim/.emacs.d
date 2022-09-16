;;; init.el --- Summary
;;; Commentary:
;;; Main EMACS settings file, load settings from parts.

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p) ;; Использовать y и n вместо yes и no (скоращает объём вводимого текста для подтверждения команд)

(defun load-pkg-msg (pkg-name)
  "Показывает сообщение о загрузке указанного пакета.

  PKG-NAME — имя пакета"
  (message (concat "Загрузка пакета " pkg-name)))


(defun load-if-exists (filename)
  "Загрузить файл, если он существует.

  FILENAME — имя файла."
  (when (file-exists-p filename)
        (load-file filename)))

(defun set-minor-mode (minor-mode-name modes-list)
  "Выполняет установку минорного режима minor-mode-name для списка режимов `modes-list'.

  MINOR-MODE-NAME — имя минорного режима.
  MODES-LIST — список основных режимов, при которых должен активироваться указанный дополнительный."
  (dolist (mode-name modes-list)
          (add-hook
            (derived-mode-hook-name mode-name) ; Эта функция позвращает имя хука, вызываемого при активации режима mode-name.
            minor-mode-name t)))

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
(unless (file-directory-p autosave-dir)
  (progn
    (make-directory autosave-dir)
    (message "Создана директория для файлов автосохранения.")))

(require 'calendar)
(require 'cyrillic)
(require 'derived)
(require 'desktop)
(require 'elec-pair)
(require 'electric)
(require 'face-remap)
(require 'ibuf-ext)
(require 'ibuffer)
(require 'ispell)
(require 'menu-bar)
(require 'paren)
(require 'quail)
(require 'saveplace)
(require 'widget)

(setq-default
 auto-save-file-name-transforms `((".*" , autosave-dir) t)
 blink-matching-paren t ; Мигать, когда скобки парные
 calendar-week-start-day 1 ; Начнём неделю с понедельника
 create-lockfiles nil ; Не надо создавать lock-файлы, от них одни проблемы
 cursor-type 'bar ; Курсор в виде вертикальной черты
  custom-file (expand-file-name "custom.el" emacs-config-dir)
  default-input-method 'russian-computer ; Чтобы хоткеи работали в любой раскладке
 delete-old-versions t ; Удалять старые версии файлов
 desktop-modes-not-to-save '(dired-mode Info-mode info-lookup-mode) ; А вот эти не сохранять
 desktop-save 1 ;; Сохранять список открытых буферов, файлов и т. д.
 gc-cons-threshold (* 50 1000 1000) ; Увеличим лимит для сборщика мусора с 800 000 до 50 000 000
 ibuffer-expert 1 ; Расширенный  режим для ibuffer
 ibuffer-hidden-filter-groups (list "Helm" "*Internal*")
 ibuffer-show-empty-filter-groups nil ; Если группа пустая, ibuffer не должен её отображать.
 ibuffer-sorting-mode 'filename/process ; Сортировать файлы в ibuffer по имени / процессу.
 ibuffer-truncate-lines nil ; Не обкусывать строки в ibuffer
 ibuffer-use-other-window nil ; Не надо открывать ibuffer в другом окне, пусть открывается в текущем
 indent-line-function (quote insert-tab)
 indent-tabs-mode nil ; Использовать для выравнивания по нажатию TAB пробелы вместо табуляций
 inhibit-splash-screen t ; Не надо показывать загрузочный экран
 inhibit-startup-message t ; Не надо показывать приветственное сообщение
 ;; initial-buffer-choice (lambda () (get-buffer "*dashboard*")) ; Буфер по умолчанию — дашборд
 initial-major-mode (quote markdown-mode) ; Режим по умолчанию сменим с EMACS Lisp на Markdown
 initial-scratch-message nil ; В новых буферах не нужно ничего писать
 large-file-warning-threshold (* 100 1024 1024) ; Предупреждение при открытии файлов больше 100 МБ (по умолчанию — 10 МБ)
 locale-coding-system 'utf-8 ; UTF-8 по умолчанию
 make-backup-files nil ; Резервные копии не нужны, у нас есть undo-tree
 overwrite-mode-binary nil ; Выключить режим перезаписи текста под курсором для бинарных файлов
 overwrite-mode-textual nil ; Выключить режим перезаписи текста под курсором для текстовых файлов
 ring-bell-function #'ignore ; Заблокировать пищание
 save-abbrevs 'silently ; Сохранять аббревиатуры без лишних вопросов
 save-place-file (expand-file-name ".emacs-places" emacs-config-dir) ; Хранить данные о позициях в открытых файлах в .emacs-places
 save-place-forget-unreadable-files 1 ; Если файл нельзя открыть, то и помнить о нём ничего не надо
 scroll-bar-mode -1 ; Выключить scroll-bar
 show-trailing-whitespace t ; Показывать висячие пробелы
 suggest-key-bindings t ; Показывать подсказку клавиатурной комбинации для команды
 tab-width 4 ; Обменный курс на TAB — 4 SPACES
 text-scale-mode-step 1.1 ;; Шаг увеличения масштаба
 truncate-lines 1 ; Обрезать длинные строки
 uniquify-buffer-name-style 'forward ; Показывать директорию перед именем файла, если буферы одинаковые (по умолчанию имя<директория>)
 uniquify-separator "/" ; Разделять буферы с похожими именами, используя /
 use-dialog-box nil ; Диалоговые окна не нужны, будем использовать текстовый интерфейс
 user-full-name "Dunaevsky Maxim"
 visible-bell t ;; Заблокировать пищание
 window-divider-default-places 't ; Разделители окон со всех сторон (по умолчанию только справа)
 window-divider-default-right-width 3 ; Ширина в пикселях для линии-разделителя окон
 x-underline-at-descent-line t
 )

(load-if-exists custom-file)

;; Правильный способ определить, что EMACS запущен в графическом режиме. Подробнее здесь:
;; https://emacsredux.com/blog/2022/06/03/detecting-whether-emacs-is-running-in-terminal-or-gui-mode/
(add-to-list 'after-make-frame-functions
  (lambda (frame-name)
    (if (display-graphic-p frame-name)
      (progn ;; GUI
        (message "Графический режим.")
        )
      (progn
        ;; TUI
        (message "Текстовый режим.")
        ))))

(column-number-mode 1) ;; Показывать номер колонки в статусной строке
(delete-selection-mode 1) ; Если регион выделен, удалить его, а не последний символ.
(desktop-save-mode 1) ; Запоминать список открытых файлов и буферов, а также установленных для них режимов.
(electric-pair-mode 1) ; Автоматически закрывает парные скобки. Это глобальный режим.
(global-font-lock-mode 1)
(global-auto-revert-mode 1) ; Автоматически перезагружать буфер при изменении файла на дискею
(global-hl-line-mode 1) ; Подсветить активные строки во всех открытых буферах
(global-visual-line-mode 1) ; Подсвечивать текущую строку
(line-number-mode 1) ;; Показывать номер строки в статусной строке
(menu-bar-mode 0) ; Меню не нужно
(prefer-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(save-place-mode 1) ; Помнить позицию курсора в открытых когда-либо файлах.
(scroll-bar-mode -1) ; Отключить полосы прокрутки
(set-default-coding-systems 'utf-8) ; Кодировка по умолчанию
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8) ; Кодировка языка по умолчанию
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(show-paren-mode 1) ; Подсвечивать парные скобки и текст между ними. Это глобальный режим.
(tooltip-mode 0) ; Не надо показывать подсказки в GUI, используй мини-буфер.
(tool-bar-mode 0) ; Выключить тулбар с кнопками
(window-divider-mode 1) ; Визуально разделять окна EMACS


;; Aspell для Linux, в Windows без проверки орфографии
(if
    (string-equal system-type "gnu/linux")
    (if
	    (file-exists-p "/usr/bin/aspell")
	    (setq ispell-program-name "/usr/bin/aspell")))

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archive-priorities '("gnu" . 1))
(add-to-list 'package-archive-priorities '("melpa" . 0))

(setq
 package-selected-packages
 '(
   airline-themes ; THEMES
   adoc-mode ; https://github.com/bbatsov/adoc-mode
   all-the-icons
   all-the-icons-dired ;; https://github.com/wyuenho/all-the-icons-dired
   all-the-icons-ibuffer ;; https://github.com/seagle0128/all-the-icons-ibuffer
   anaconda-mode
   ansible
   apache-mode
   apt-sources-list
   base16-theme
   centaur-tabs
   company
   company-anaconda
   company-box
   company-terraform
   company-web
   csharp-mode ; https://github.com/emacs-csharp/csharp-mode
   dash
   dashboard
   demap ; https://gitlab.com/sawyerjgardner/demap.el
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
   mode-icons ; https://github.com/ryuslash/mode-icons
   multiple-cursors
   org
   php-mode
   projectile ; https://docs.projectile.mx/projectile/installation.html
   protobuf-mode
   pulsar ; https://protesilaos.com/emacs/pulsar
   pyenv-mode ; https://github.com/pythonic-emacs/pyenv-mode
   python
   python-mode
   rainbow-delimiters ; https://github.com/Fanael/rainbow-delimiters
   restclient ; https://github.com/pashky/restclient.el
   rg ; https://github.com/dajva/rg.el
   scala-mode
   swiper ; https://github.com/abo-abo/swiper
   terraform-mode
   tide
   treemacs
   treemacs-all-the-icons
   treemacs-icons-dired
   treemacs-magit
   typescript-mode
   undo-tree
   vagrant ; https://github.com/ottbot/vagrant.el
   verb
   vertico ; https://github.com/minad/vertico
   web-beautify
   web-mode
   wgrep ; https://github.com/mhayashi1120/Emacs-wgrep
   which-key
   ws-butler
   yaml-mode
   yascroll ; https://github.com/emacsorphanage/yascroll
   yasnippet ; http://github.com/joaotavora/yasnippet
   yasnippet-snippets ; https://github.com/AndreaCrotti/yasnippet-snippets
   ))

(message "After setq-default package-selected-packages")
(message "%s" package-selected-packages)

;; Установка необходимых пакетов
(if
  (cl-find-if-not #'package-installed-p package-selected-packages)
  (progn
    (package-refresh-contents)
    (dolist (pkg-name package-selected-packages)
      (message "Before check")
      (unless (package-installed-p pkg-name)
        (progn
          (message (format "Install package %s" pkg-name))
          (package-install pkg-name t))))))


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
  (setq all-the-icons-ibuffer-human-readable-size t ;; Показывать размер файлов в ibuffer в человекочитаемом виде
        all-the-icons-ibuffer-icon t)
  (all-the-icons-ibuffer-mode t)
  (mode-icons-mode t)
  (tooltip-mode nil) ;; Убрать всплывающие подсказки в tooltip'ах при наведении мыши

  ;; Настройки шрифтов
  (message "Настройки шрифтов")
  (set-face-attribute 'default nil
                      :font default-font
                      :family default-font-family
                      :height default-font-size)) ;; /when is gui-mode


;; Settings for hotkeys on any layout
;; (require 'quail)
;; (defun cfg:reverse-input-method (input-method)
;;   "Build the reverse mapping of single letters from INPUT-METHOD."
;;   (interactive
;;     (list (read-input-method-name "Use input method (default current): ")))
;;   (if (and input-method (symbolp input-method))
;;     (setq input-method (symbol-name input-method)))
;;   (let ((current current-input-method)
;;          (modifiers '(nil (control) (meta) (control meta))))
;;     (if input-method
;;       (activate-input-method input-method))
;;     (when (and current-input-method quail-keyboard-layout)
;;       (dolist (map (cdr (quail-map)))
;;         (let* ((to (car map))
;;               (from (quail-get-translation
;;                     (cadr map) (char-to-string to) 1)))
;;           (when (and (characterp from) (characterp to))
;;             (dolist (mod modifiers)
;;               (define-key local-function-key-map
;;                 (vector (append mod (list from)))
;;                 (vector (append mod (list to)))))))))
;;     (when input-method
;;       (activate-input-method current))))
;; (cfg:reverse-input-method 'russian-computer)
(set-input-method 'russian-computer)


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
;; Работа с аббревиатурами. Безо всяких нажатий клавиш текст будет заменяться на расширенную фразу, например,
;; кл будет разворачиваться в кластер, а п в пользователя.
(load-pkg-msg "abbrev")
(require 'abbrev)
(set-minor-mode
 'abbrev-mode
 '(
   markdown-mode
   nxml-mode
   xml-mode
   ))

;; ACE-WINDOW
;; https://github.com/abo-abo/ace-window
(load-pkg-msg "ace-window")
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)


;; ADOC-MODE
;; https://github.com/bbatsov/adoc-mode
;; Работа с AsciiDoc
(load-pkg-msg "adoc-mode")
(require 'adoc-mode)
(require 'markup-faces)
(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
(add-hook 'adoc-mode-hook (lambda () (buffer-face-mode t)))


;; ANSIBLE MODE
(load-pkg-msg "ansible")
(require 'ansible)


;; APT SOURCES LIST MODE
;; https://git.korewanetadesu.com/apt-sources-list.git
(load-pkg-msg "apt-sources-list-mode")
(add-to-list 'auto-mode-alist (cons "\\.list$" 'apt-sources-list-mode))


;; CENTAUR-TABS
;; https://github.com/ema2159/centaur-tabs
;; Вкладки с иконками и прочими удобствами
(require 'centaur-tabs)
(setq-default
 centaur-tabs-enable-key-bindings t; Включить комбинации клавиш из `centaur-tabs`.
 centaur-tabs-close-button "×" ; Будем использовать вот этот символ вместо X
 centaur-tabs-height 36 ; Высота вкладок
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
;; В этих режимах не нужно показывать вкладки
(set-minor-mode
 'centaur-tabs-local-mode
 '(
   dashboard-mode
   dired-mode
   ))


;; COMPANY-MODE
;; https://company-mode.github.io/
;; Автодополнение
(load-pkg-msg "company")
(require 'company)
(require 'company-dabbrev)
(setq-default
 company-dabbrev-downcase nil
 company-dabbrev-ignore-case nil
 company-dabbrev-ignore-case nil
 company-idle-delay 0.5 ; Задержка вывода подсказки — полсекунды
 company-minimum-prefix-length 2
 company-show-quick-access t ; Показывать номера возле потенциальных кандидатов
 company-tooltip-align-annotations t
 company-tooltip-limit 10
 )
(set-minor-mode
 'company-mode
 '(
   ansible-mode
   apt-sources-list-mode
   conf-mode
   csharp-mode
   dockerfile-mode
   emacs-lisp-mode
   go-mode
   java-mode
   js2-mode
   json-mode
   makefile-mode
   markdown-mode
   nxml-mode
   org-mode
   php-mode
   protobuf-mode
   python-mode
   rst-mode
   ruby-mode
   scala-mode
   shell-script-mode
   sql-mode
   terraform-mode
   tide-mode
   web-mode
   xml-mode
   yaml-mode
   ))


;; COMPANY-BOX
;; https://github.com/sebastiencs/company-box
;; Расширение для company-mode, которое показывает доступные варианты, используя иконки
;; (load-pkg-msg "company-box")
;; (require 'company-box)
;; (add-hook 'company-mode-hook 'company-box-mode)


;; COMPANY-WEB
;; Автодополнение для режима web-mode
(load-pkg-msg "company-web")
(require 'company-web)
(require 'company-web-html)
(add-to-list 'company-backends 'company-web-html)


;; CONF MODE
;; Основной режим для редактирования конфигурационных файлов.
(load-pkg-msg "conf-mode")
(require 'conf-mode)
(add-to-list 'auto-mode-alist '("\\.env$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.flake8$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.ini$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.pylintrc$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.terraformrc$" . conf-mode))


;; Dashboard
;; https://github.com/emacs-dashboard/emacs-dashboard
;; Отображает дашборд при запуске EMACS
(load-pkg-msg "dashboard")
(require 'dashboard)
(setq-default
 dashboard-items
 '(
   (recents . 15) ;; Последние открытые файлы
   (bookmarks . 10) ;; Последние закладки
   (projects . 10) ;; Последние проекты
   (agenda . 10) ;; Агенда
   (registers . 5)) ;; Регистры
 dashboard-set-heading-icons is-gui-mode ;; Иконка EMACS в графическом режиме
 dashboard-set-file-icons is-gui-mode) ;; Иконки типов файлов в графическом режиме
(dashboard-setup-startup-hook)


;; DEMAP
;; https://gitlab.com/sawyerjgardner/demap.el
;; Мини-карта и плавный скроллинг
(require 'demap)
(setq-default demap-minimap-window-width 15) ; Ширина мини-карты


;; DIFF-HL
;; https://github.com/dgutov/diff-hl
;; Показывает небольшие маркеры рядом с незафиксированными изменениями. Дополняет функциональность git-gutter,
;; которые показывает изменения только в обычных буферах. Этот пакет умеет работать с dired и другими режимами.
(load-pkg-msg "diff-hl")
(require 'diff-hl)
(global-diff-hl-mode 1)


;; DIRED
;; Встроенный пакет для работы с файлами и каталогами.
(load-pkg-msg "dired")
(require 'dired)
(when (string-equal system-type "gnu/linux")
  ;; Это может не работать в Windows, надо проверить
  (setq-default dired-listing-switches "-lahX --group-directories-first"))
(add-hook 'dired-mode-hook #'auto-revert-mode)


;; DIRENV-MODE
;; https://github.com/wbolster/emacs-direnv
(load-pkg-msg "direnv")
(require 'direnv)
(setq direnv-use-faces-in-summary nil)
(direnv-mode 1)


;; DISPLAY-LINE-NUMBERS-MODE
;; Встроенный пакет
;; Показывает номера строк
(load-pkg-msg "display-line-numbers")
(require 'display-line-numbers)
(set-minor-mode
 'display-line-numbers-mode
 '(
   apt-sources-list-mode
   conf-mode
   emacs-lisp-mode
   go-mode
   hcl-mode
   java-mode
   javascript-mode
   js2-mode
   json-mode
   makefile-mode
   markdown-mode
   org-mode
   php-mode
   protobuf-mode
   python-mode
   rst-mode
   ruby-mode
   scala-mode
   shell-script-mode
   sql-mode
   terraform-mode
   text-mode
   web-mode
   yaml-mode
   ))


;; DOCKERFILE-MODE
(load-pkg-msg "dockerfile-mode")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("^Dockerfile$" . dockerfile-mode))


;; DOOM-MODELINE
;; https://github.com/seagle0128/doom-modeline
;; Красивая и многофункциональная статусная панель
(load-pkg-msg "doom-modeline")
(require 'doom-modeline)
(setq-default
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
(load-pkg-msg "doom-themes")
(require 'doom-themes)
(load-theme 'doom-monokai-classic t)
;; (load-theme 'doom-one t)
(doom-themes-org-config)
(doom-themes-visual-bell-config)


;; EASY KILL
;; https://github.com/leoliu/easy-kill
;; Удобнее работать с удалением текстовых блоков
(load-pkg-msg "easy-kill")
(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)


;; EDITORCONFIG EMACS
;; https://github.com/editorconfig/editorconfig-emacs
(load-pkg-msg "editorconfig")
(require 'editorconfig)
(setq-default editorconfig-trim-whitespaces-mode 'ws-butler-mode)
(editorconfig-mode 1)


;; ELECTRIC-PAIR MODE
;; Встроенный пакет.
(require 'elec-pair)
(add-to-list 'electric-pair-pairs '(?« . ?»))
(add-to-list 'electric-pair-pairs '(?{ . ?}))
(electric-pair-mode 1)


;; EMACS LISP MODE
;; IT IS NOT A ELISP-MODE!
;; Встроенный пакет для EMACS Lisp
(add-to-list 'auto-mode-alist '("\\.el$'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\abbrev_defs$" . emacs-lisp-mode))


;; EMBARK
;; https://github.com/oantolin/embark
(load-pkg-msg "embark")
(require 'embark)
(setq-default prefix-help-command #'embark-prefix-help-command)


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
(load-pkg-msg "emmet-mode")
(require 'emmet-mode)
(set-minor-mode
 'emmet-mode
 '(
   css-mode
   html-mode
   nxml-mode
   xml-mode
   web-mode
   ))


;; FLYCHECK
;; https://flycheck.org
;; Проверка синтаксиса на лету с помощью статических анализаторов
(load-pkg-msg "flycheck")
(require 'flycheck)
(require 'flycheck-color-mode-line) ;; https://github.com/flycheck/flycheck-color-mode-line
(require 'flycheck-indicator)
(setq-default
 flycheck-check-syntax-automatically '(mode-enabled save new-line)
 flycheck-locate-config-file-functions '(
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
(set-minor-mode
 'flycheck-mode
 '(
   adoc-mode
   apt-sources-list-mode
   conf-mode
   dockerfile-mode
   emacs-lisp-mode
   go-mode
   java-mode
   javascript-mode
   js2-mode
   json-mode
   makefile-mode
   markdown-mode
   nxml-mode
   php-mode
   protobuf-mode
   python-mode
   rst-mode
   ruby-mode
   scala-mode
   shell-script-mode
   sql-mode
   terraform-mode
   tide-mode
   web-mode
   xml-mode
   yaml-mode
   ))


;; FORMAT ALL
;; https://github.com/lassik/emacs-format-all-the-code
;; Форматирование кода по нажатию [F12]
(load-pkg-msg "format-all")
(require 'format-all)
(global-set-key (kbd "<f12>") 'format-all-buffer)


;; GIT-GUTTER
;; https://github.com/emacsorphanage/git-gutter
(load-pkg-msg "git-gutter")
(require 'git-gutter)
(setq-default git-gutter:visual-line t)


;; GO-MODE
;; https://github.com/dominikh/go-mode.el
;; Поддержка Golang
(load-pkg-msg "go-mode")
(require 'go-mode)
(add-to-list 'auto-mode-alist '("\\.go$'" . go-mode))


;; HELM
;; https://github.com/emacs-helm/helm
;; Подсказки в минибуфере
(load-pkg-msg "helm")
(require 'helm)
(setq-default completion-styles '(flex))
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-S-p") 'helm-M-x)
(helm-mode 1)


;; HELM-AG
;; https://github.com/syohex/emacs-helm-ag
;; Поиск и замена строк в нескольких файлах.
(load-pkg-msg "helm-ag")
(require 'helm-ag)


;; HIGHLIGHT-INDENTATION-MODE
;; https://github.com/antonj/Highlight-Indentation-for-Emacs
;; Показывает направляющие для отступов
(load-pkg-msg "highlight-indentation")
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
(set-minor-mode
 'highlight-indentation-mode
 '(
   makefile-mode
   markdown-mode
   python-mode
   ruby-mode
   terraform-mode
   yaml-mode
   ))


;; IBUFFER
;; Встроенный пакет для удобной работы с буферами.
;; По нажатию F2 выводит список открытых буферов.
(defalias 'list-buffers 'ibuffer)
(setq-default
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
     (or
      (mode . xml-mode)
      (mode . nxml-mode)))
    ("YAML"
     (mode . yaml-mode))
    ("Makefile"
     (or
      (mode . makefile-mode)
      (name  . "^Makefile$")))
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
      (name . "^\\*.pub$")))
    ("Shell-script"
     (or
      (mode . shell-script-mode)
      (mode . sh-mode)))
    ("Terraform"
     (or
      (mode . terraform-mode)
      (name . "^\\*.tf$")))
    ("SQL"
     (or
      (mode . sql-mode)))
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
    filename)))
(defun setup-ibuffer-mode ()
  "Настройки при запуске `ibuffer-mode'."
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))
(add-hook 'ibuffer-mode-hook #'setup-ibuffer-mode)
(global-set-key (kbd "<f2>") 'ibuffer)


;; JS2-MODE
;; https://github.com/mooz/js2-mode
(load-pkg-msg "js2-mode")
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; JSON-MODE
;; https://github.com/joshwnj/json-mode
;; Работа с JSON
(load-pkg-msg "json-mode")
(require 'json)
(add-to-list 'auto-mode-alist '("\\.bowerrc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))


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
;; MARKDOWN: npm install -g remark-language-server remark
;; NXML: lsp-install-server, выбрать xmlls, установить на уровне системы JDK
;; SQL: go install github.com/lighttiger2505/sqls@latest
;; TERRAFORM: нужен установленный в системе terraform-ls. Можно скачать с сайта hashicorp.com
;; XML: lsp-install-server, выбрать xmlls, установить на уровне системы JDK
;; YAML: npm install -g yaml-language-server
(load-pkg-msg "lsp-mode")
(require 'lsp-mode)
(require 'lsp-ui)
(setq-default
 lsp-headerline-breadcrumb-enable t ; Показывать "хлебные крошки" в заголовке
 lsp-modeline-diagnostics-enable t ; Показывать ошибки LSP в статусной строке
 lsp-ui-doc-enable t
 lsp-ui-peek-always-show t
 lsp-ui-peek-enable t
 lsp-ui-sideline-enable t
 )
(set-minor-mode
 'lsp
 '(
   dockerfile-mode
   go-mode
   json-mode
   makefile-mode
   nxml-mode
   sql-mode
   terraform-mode
   xml-mode
   yaml-mode
   ))


;; MAGIT
;; https://magit.vc/
;; Magic + Git + Git-gutter. Лучшее средство для управления Git.
(load-pkg-msg "magit")
(require 'magit)
(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'magit-checkout)


;; MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
(load-pkg-msg "markdown-mode")
(require 'markdown-mode)
(setq
 header-line-format " "
 left-margin-width 4
 markdown-fontify-code-blocks-natively t ; Подсвечивать синтаксис в примерах кода
 markdown-header-scaling-values '(1.0 1.0 1.0 1.0 1.0 1.0) ; Все заголовки одной высоты
 markdown-list-indent-width 4
 word-wrap t ; Перенос по словам
 )
(set-face-attribute 'markdown-inline-code-face nil :family default-font-family)
(defun setup-markdown-mode()
  "Settings for editing markdown documents."
  (interactive)
  (cond ;; Turn on spell-checking only in Linux
   (
    (string-equal system-type "gnu/linux")
    (flyspell-mode 1))))
(define-key markdown-mode-map (kbd "M-.") 'markdown-follow-thing-at-point)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook #'setup-markdown-mode)


;; MULTIPLE CURSORS
;; Позволяет использовать мультикурсорность.
(load-pkg-msg "multiple-cursors")
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-e") 'mc/edit-lines)
(if is-gui-mode
    (progn
      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)))


;; NXML-MODE
;; Встроенный пакет
;; Почти как xml-mode, только лучше и новее
(load-pkg-msg "nxml-mode")
(require 'nxml-mode)
(add-to-list 'auto-mode-alist '("\\.xml$'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.pom$'" . nxml-mode))
(setq-default
 nxml-attribute-indent 4 ; Выравнивание атрибутов
 nxml-auto-insert-xml-declaration-flag nil ; Не вставлять декларацию
 nxml-bind-meta-tab-to-complete-flag t ; Использовать TAB для завершения ввода
 nxml-child-indent 4 ; Выравнивание дочерних элементов
 nxml-slash-auto-complete-flag t ; Закрывать теги по вводу /
 )


;; ORG-MODE
;; https://orgmode.org/
;; Органайзер, и не только
(load-pkg-msg "org-mode")
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
(add-to-list 'auto-mode-alist '("\\.org$'" . org-mode))


;; PHP-MODE
(load-pkg-msg "php-mode")
(require 'php)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))


;; PROJECTILE
;; https://docs.projectile.mx/projectile/installation.html
;; Управление проектами
(load-pkg-msg "projectile")
(require 'projectile)
(load-if-exists (expand-file-name "projects.el" emacs-config-dir))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; PROTOBUF-MODE
;; https://github.com/emacsmirror/protobuf-mode
;; Работа с файлами Protobuf: подсветка синтаксиса, переход по ссылками и т. д.
(load-pkg-msg "protobuf-mode")
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))


;; PULSAR
;; https://protesilaos.com/emacs/pulsar
;; Подсвечивает текущую строку при наступлении различных событий: смена или закрытие буфера, переход по ссылке и т. д.
(load-pkg-msg "pulsar")
(require 'pulsar)
(pulsar-global-mode 1)


;; PYTHON-MODE
(load-pkg-msg "python")
(require 'python)
(setq-default
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
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (define-key python-mode-map (kbd "M-/") 'jedi:show-doc)
  (define-key python-mode-map (kbd "M-?") 'helm-jedi-related-names))
(add-hook 'python-mode-hook #'setup-python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))


;; RAINBOW-DELIMITERS-MODE
;; https://github.com/Fanael/rainbow-delimiters
;; Каждая пара скобок отрисовывается своим цветом
(load-pkg-msg "rainbow-delimiters")
(require 'rainbow-delimiters)
(set-minor-mode
 'rainbow-delimiters-mode
 '(
   apt-sources-list-mode
   emacs-lisp-mode
   go-mode
   java-mode
   js2-mode
   json-mode
   makefile-mode
   markdown-mode
   nxml-mode
   org-mode
   php-mode
   protobuf-mode
   python-mode
   rst-mode
   ruby-mode
   scala-mode
   shell-script-mode
   sql-mode
   terraform-mode
   tide-mode
   web-mode
   xml-mode
   yaml-mode
   ))


;; RST-MODE
;; Основной режим для редактирования reStructutedText
;; Больше здесь: https://www.writethedocs.org/guide/writing/reStructuredText/
(load-pkg-msg "rst-mode")
(require 'rst)
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))


;; RUBY-MODE
;; Это встроенный пакет
;;Поддержка Ruby on Rails
(load-pkg-msg "ruby-mode")
(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" .ruby-mode))


;; REST-CLIENT_MODE
;; https://github.com/pashky/restclient.el
;; Старый пакет для работы с REST API, более удобная замена — verb
(load-pkg-msg "restclient")
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))


;; RG (ripgrep)
;; https://github.com/dajva/rg.el
;; Для работы пакета требуется наличие в системе утилиты ripgrep
(load-pkg-msg "rg")
(require 'rg)
(rg-enable-default-bindings)


;; SCALA MODE
;; https://github.com/hvesalai/emacs-scala-mode
(load-pkg-msg "scala-mode")
(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sc$" . scala-mode))


;; SHELL-SCRIPT-MODE
(load-pkg-msg "shell-script-mode")
(require 'sh-script)
(add-to-list 'auto-mode-alist '("\\.bashrc$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.profile$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.sh$" . shell-script-mode))


;; SQL MODE
;; Это встроенный режим
(load-pkg-msg "sql")
(require 'sql)
(add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))


;; SWIPER MODE
;; https://github.com/abo-abo/swiper
;; Пакет для быстрого поиска.
;; По кажатию C-7 можно выполнить быстрое редактирование найденных фрагментов, но чтобы
;; оно сработало правильно, нужно добавить команду swiper-mc в список mc/cmds-to-run-once.
(load-pkg-msg "swiper")
(require 'swiper)
;; (add-to-list 'mc/cmds-to-run-once 'swiper-mc)
(global-set-key (kbd "C-f") 'swiper-isearch)


;; TIDE-MODE (Typescript IDE)
;; https://github.com/ananthakumaran/tide/
(load-pkg-msg "tide")
(require 'tide)
(defun setup-tide-mode ()
  "Settings for 'tide-mode'."
  (interactive)
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (tide-setup))
(add-hook 'tide-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.tsx$" . tide-mode))
(add-to-list 'auto-mode-alist '("\\.d\\.ts$" . tide-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" .tide-mode))
(add-to-list 'auto-mode-alist '("\\.ts$" . tide-mode))


;; TERRAFORM-MODE
;; https://github.com/emacsorphanage/terraform-mode
;; Работа с файлами конфигурации Terraform
(load-pkg-msg "terraform-mode")
(require 'terraform-mode)
(with-eval-after-load "terraform-mode"
  (setq-default flycheck-checker 'terraform))
(add-to-list 'auto-mode-alist (cons "\\.tf$" 'terraform-mode))



;; TEXT MODE
;; Просто указываю, что вместо TAB'ов надо использовать пробелы. Почему-то настройки выше игнорируются.
(load-pkg-msg "text-mode")
(require 'text-mode)
(add-hook
 'text-mode-hook
 '(lambda ()
    (setq
     indent-tabs-mode nil
     tab-width 4)
    (whitespace-mode 1)
    (rainbow-delimiters-mode 1)
    (ws-butler-mode 1)))


;; TREEMACS — awesome file manager (instead NeoTree)
;; https://github.com/Alexander-Miller/treemacs
;; Дерево файлов и каталогов
(load-pkg-msg "treemacs")
(require 'treemacs)
(setq-default treemacs-width 35)
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
(load-pkg-msg "treemacs-icons")
(require 'treemacs-icons)
(add-hook 'dired-mode-hook 'treemacs-icons-dired-enable-once)


;; UNDO-TREE
;; Не только предоставляет привычное поведение при отмене команд, но и даёт мощные возможности по
;; ведению дерева правок.
(load-pkg-msg "undo-tree")
(require 'undo-tree)
(setq undo-tree-auto-save-history nil) ; Отключить создание резервных копий файлов
(global-undo-tree-mode 1)


;; VAGRANT MODE
;; Работа с файлами Vagrant
;; https://github.com/ottbot/vagrant.el
(load-pkg-msg "vagrant")
(require 'vagrant)
(add-to-list 'auto-mode-alist (cons "Vagrantfile$\\'" 'vagrant-mode))


;; VERB-MODE
;; Удобная работа с REST API
;; https://github.com/federicotdn/verb
(load-pkg-msg "verb")
(require 'verb)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))


;; VERTICO
;; https://github.com/minad/vertico
;; Автодополнение на основе встроенной функциональности EMACS
(load-pkg-msg "vertico")
(require 'vertico)


;; WEB-MODE
;; https://web-mode.org/
(load-pkg-msg "web-mode")
(require 'web-mode)
(setq
 web-mode-attr-indent-offset 4
 web-mode-css-indent-offset 2 ;; CSS
 web-mode-enable-block-face t
 web-mode-enable-css-colorization t
 web-mode-enable-current-element-highlight t
 web-mode-markup-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))


;; WGREP
;; https://github.com/mhayashi1120/Emacs-wgrep
;; Поиск и замена по нескольким файлам
(load-pkg-msg "wgrep")
(require 'wgrep)


;; WHICH-KEY MODE
;; https://github.com/justbur/emacs-which-key
;; Показывает подсказки к командам.
(load-pkg-msg "which-key")
(require 'which-key)
(which-key-setup-side-window-right)
(which-key-mode 1)


;; WHITESPACE MODE
;; Встроенный пакет для отображения невидимых символов
;; Показывает невидимые символы.
(load-pkg-msg "whitespace")
(require 'whitespace)
(setq-default
 whitespace-display-mappings
 '(
   (space-mark   ?\    [?\xB7]     [?.]) ; Пробел
   (space-mark   ?\xA0 [?\xA4]     [?_]) ; Неразрывный пробел
   (newline-mark ?\n   [?¶ ?\n]    [?$ ?\n]) ; Конец строки
   (tab-mark     ?\t   [?\xBB ?\t] [?\\ ?\t]) ; TAB
   )
 whitespace-line-column 1000 ;; Highlight lines with length bigger than 1000 chars)
 )
(set-face-attribute
 'whitespace-space nil
 :family default-font-family
 :foreground "#75715E")
(set-face-attribute
 'whitespace-indentation nil
 :family default-font-family
 :foreground "#E6DB74")
(set-minor-mode
 'whitespace-mode
 '(
   adoc-mode
   apt-sources-list-mode
   conf-mode
   dockerfile-mode
   emacs-lisp-mode
   go-mode
   hcl-mode
   java-mode
   js2-mode
   json-mode
   makefile-mode
   markdown-mode
   nxml-mode
   org-mode
   php-mode
   protobuf-mode
   python-mode
   rst-mode
   ruby-mode
   scala-mode
   sh-mode
   shell-script-mode
   sql-mode
   terraform-mode
   text-mode
   tide-mode
   web-mode
   xml-mode
   yaml-mode
   ))


;; WS-BUTLER MODE
;; https://github.com/lewang/ws-butler
;; Чистит висячие пробелы только в измененных строках.
(load-pkg-msg "ws-butler")
(require 'ws-butler)
(set-minor-mode
 'ws-butler-mode
 '(
   adoc-mode
   apt-sources-list-mode
   conf-mode
   dockerfile-mode
   emacs-lisp-mode
   go-mode
   java-mode
   js2-mode
   json-mode
   markdown-mode
   nxml-mode
   org-mode
   php-mode
   protobuf-mode
   python-mode
   rst-mode
   ruby-mode
   scala-mode
   sh-mode
   shell-script-mode
   sql-mode
   terraform-mode
   tide-mode
   web-mode
   xml-mode
   yaml-mode
   ))


;; YAML-MODE
;; https://github.com/yoshiki/yaml-mode
;; Работа с YAML-файлами
(load-pkg-msg "yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ansible\\-lint" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.pre\\-commit\\-config\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yamllint$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yamllint\\-config\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yfm$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; YASCROLL-MODE
;; https://github.com/emacsorphanage/yascroll
;; Альтернативная полоса прокрутки
(load-pkg-msg "yascroll")
(require 'yascroll)
(global-yascroll-bar-mode 1)


;; YASNIPPET
;; http://github.com/joaotavora/yasnippet
;; Предоставляет функциональность сниппетов — блоков кода, в которые всего-лишь нужно подставить значения.
(load-pkg-msg "yasnippet")
(require 'yasnippet)
;; Если директории для сниппектов нет, её нужно создать.
(defvar yas-snippet-root-dir (concat emacs-config-dir "snippets"))
(unless (file-directory-p yas-snippet-root-dir)
  (mkdir yas-snippet-root-dir))
(yas-global-mode 1)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
