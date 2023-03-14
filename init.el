;;; init.el --- Dunaevsky Maxim GNU EMACS config -*- lexical-binding:t -*-
;;; Commentary:
;;; Настройки EMACS для работы техническим писателем.

;;; Code:

(require 'calendar)
(require 'cyrillic)
(require 'derived)
(require 'face-remap)
(require 'ibuf-ext)
(require 'ibuffer)
(require 'ispell)
(require 'menu-bar)
(require 'paren)
(require 'quail)
(require 'saveplace)
(require 'widget)

(fset 'yes-or-no-p 'y-or-n-p) ;; Использовать y и n вместо yes и no (скоращает объём вводимого текста для подтверждения команд)

(defun load-if-exists (filename)
  "Загрузить файл, если он существует.

  FILENAME — имя файла."
  (when (file-exists-p filename)
    (load-file filename)))

(defun set-minor-mode (minor-mode-name modes-list)
  "Выполняет установку minor-mode-name для списка режимов `modes-list'.

  MINOR-MODE-NAME — имя минорного режима.
  MODES-LIST — список основных режимов, при которых должен активироваться указанный дополнительный."
  (dolist (mode-name modes-list)
    (add-hook
      (derived-mode-hook-name mode-name) ; Эта функция позвращает имя хука, вызываемого при активации режима mode-name.
      minor-mode-name t)))

(defconst emacs-config-dir (file-name-directory user-init-file) "Корневая директория для размещения настроек.")
(defconst autosave-dir (concat emacs-config-dir "saves") "Директория для файлов автосохранения.")
(defconst backups-dir (concat emacs-config-dir "backups") "Директория для резервных копий.")
(defconst is-gui-mode (display-graphic-p) "EMACS запущен в графическом режиме.")
(defconst default-font-height 15 "Размер шрифта по умолчанию.")

;;; Создание каталогов для резервных копий и файлов автосохранения
(unless (file-directory-p autosave-dir)
  (make-directory autosave-dir)
  (message "Создана директория для файлов автосохранения."))

(setq-default
  abbrev-mode t ; Включить поддержку аббревиатур глобально
  auto-save-file-name-transforms `((".*" , autosave-dir) t)
  blink-matching-paren t ; Мигать, когда скобки парные
  calendar-week-start-day 1 ; Начнём неделю с понедельника
  create-lockfiles nil ; Не надо создавать lock-файлы, от них одни проблемы
  cursor-type 'bar ; Курсор в виде вертикальной черты
  delete-old-versions t ; Удалять старые версии файлов
  gc-cons-threshold (* 50 1000 1000) ; Увеличим лимит для сборщика мусора с 800 000 до 50 000 000
  indent-line-function (quote insert-tab)
  indent-tabs-mode nil ; Использовать для выравнивания по нажатию TAB пробелы вместо табуляций
  inhibit-splash-screen t ; Не надо показывать загрузочный экран
  inhibit-startup-message t ; Не надо показывать приветственное сообщение
  initial-scratch-message nil ; В новых буферах не нужно ничего писать
  large-file-warning-threshold (* 100 1024 1024) ; Предупреждение при открытии файлов больше 100 МБ (по умолчанию — 10 МБ)
  load-prefer-newer t ; Если есть файл elc, но el новее, загрузить el-файл
  make-backup-files nil                 ;; Резервные копии не нужны, у нас есть undo-tree
  overwrite-mode-binary nil             ;; Выключить режим перезаписи текста под курсором для бинарных файлов
  overwrite-mode-textual nil            ;; Выключить режим перезаписи текста под курсором для текстовых файлов
  package-user-dir (expand-file-name "elpa" user-emacs-directory) ; Хранить все пакеты в каталоге ~/.emacs.d/elpa/
  require-final-newline t               ;; Автоматически вставлять в конец файла пустую строку, если её там нет
  ring-bell-function #'ignore           ;; Заблокировать пищание
  save-abbrevs 'silently                ;; Сохранять аббревиатуры без лишних вопросов
  save-place-file (expand-file-name ".emacs-places" emacs-config-dir) ; Хранить данные о позициях в открытых файлах в .emacs-places
  save-place-forget-unreadable-files 1  ;; Если файл нельзя открыть, то и помнить о нём ничего не надо
  scroll-conservatively 100000          ;; TODO: проверить, что это такое
  scroll-margin 5                       ;; При прокрутке помещать курсор на 5 строк выше / ниже верхней / нижней границы окна
  scroll-preserve-screen-position 1     ;; TODO: проверить, что это такое
  show-trailing-whitespace t                    ;; Показывать висячие пробелы
  source-directory "/usr/share/emacs/27.1/src/" ;; Путь к исходному коду EMACS
  suggest-key-bindings t               ;; Показывать подсказку клавиатурной комбинации для команды
  tab-always-indent 'complete          ;; Невыровненную строку — выровнять, в противном случае — предложить автозавершение
  tab-width 4                          ;; Обменный курс на TAB — 4 SPACES
  text-scale-mode-step 1.1             ;; Шаг увеличения масштаба
  truncate-lines 1                     ;; Обрезать длинные строки
  uniquify-buffer-name-style 'forward  ;; Показывать директорию перед именем файла, если буферы одинаковые (по умолчанию имя<директория>)
  uniquify-separator "/"               ;; Разделять буферы с похожими именами, используя /
  use-dialog-box nil                   ;; Диалоговые окна не нужны, будем использовать текстовый интерфейс
  user-full-name "Dunaevsky Maxim"
  visible-bell t                       ;; Заблокировать пищание
  window-divider-default-places 't     ;; Разделители окон со всех сторон (по умолчанию только справа)
  window-divider-default-right-width 3 ;; Ширина в пикселях для линии-разделителя окон
  x-underline-at-descent-line t)

;; Включение стандартных режимов
(column-number-mode 1)      ;; Показывать номер колонки в статусной строке
(delete-selection-mode t)   ;; Если регион выделен, удалить его, а не последний символ.
(global-font-lock-mode t)   ;; Отображать шрифты красиво, используя Font Face's
(global-auto-revert-mode 1) ;; Автоматически перезагружать буфер при изменении файла на дискею
(global-hl-line-mode 1)     ;; Подсветить активные строки во всех открытых буферах
(global-visual-line-mode 1) ;; Подсвечивать текущую строку
(line-number-mode t)        ;; Показывать номер строки в статусной строке
(menu-bar-mode -1)          ;; Меню не нужно
(save-place-mode 1)         ;; Помнить позицию курсора в открытых когда-либо файлах.
(scroll-bar-mode -1)        ;; Отключить полосы прокрутки
(size-indication-mode 1)    ;; Отображать размер буфера в строке статуса
(show-paren-mode 1)         ;; Подсвечивать парные скобки и текст между ними. Это глобальный режим.
(tooltip-mode 0)            ;; Не надо показывать подсказки в GUI, используй мини-буфер.
(tool-bar-mode 0)           ;; Выключить тулбар с кнопками
(window-divider-mode t)     ;; Визуально разделять окна EMACS


;; КОДИРОВКИ
;; Везде насаждаем UTF-8
(prefer-coding-system 'utf-8)               ;; При попытке определить кодировку файла начинать перебор с UTF-8
(set-default-coding-systems 'utf-8)         ;; Кодировка по умолчанию
(set-keyboard-coding-system 'utf-8)         ;; Кодировка символов при вводе текста в терминале
(set-language-environment 'utf-8)           ;; Кодировка языка по умолчанию
(set-selection-coding-system 'utf-8)        ;; Кодировка символов для передачи скопированных в буфер данных другим приложениям X11
(set-terminal-coding-system 'utf-8)         ;; Кодировка символов для вывода команд, запущенных в терминале
(setq-default locale-coding-system 'utf-8)  ;; UTF-8 по умолчанию


;; Настройка пакетов
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages
  '(
     airline-themes           ;; https://github.com/AnthonyDiGirolamo/airline-themes
     ;; adoc-mode                ;; https://github.com/bbatsov/adoc-mode
     aggressive-indent        ;;
     all-the-icons            ;;
     all-the-icons-dired      ;; https://github.com/wyuenho/all-the-icons-dired
     all-the-icons-ibuffer    ;; https://github.com/seagle0128/all-the-icons-ibuffer
     anaconda-mode            ;;
     ansible                  ;;
     apache-mode              ;;
     apt-sources-list         ;; https://git.korewanetadesu.com/apt-sources-list.git
     centaur-tabs             ;; https://github.com/ema2159/centaur-tabs
     checkdoc                 ;;
     company                  ;;
     company-anaconda         ;;
     company-box              ;;
     company-terraform        ;;
     company-web              ;;
     counsel                  ;;
     csharp-mode              ;; https://github.com/emacs-csharp/csharp-mode
     dash                     ;;
     dashboard                ;; https://github.com/emacs-dashboard/emacs-dashboard
     demap                    ;; https://gitlab.com/sawyerjgardner/demap.el
     diff-hl                  ;; https://github.com/dgutov/diff-hl
     dockerfile-mode          ;;
     doom-modeline            ;; https://github.com/seagle0128/doom-modeline
     doom-themes              ;; https://github.com/doomemacs/themes
     easy-kill                ;; https://github.com/leoliu/easy-kill
     edit-indirect            ;;
     editorconfig             ;; https://github.com/editorconfig/editorconfig-emacs
     eglot                    ;; https://github.com/joaotavora/eglot
     elsa                     ;; https://github.com/emacs-elsa/Elsa
     embark                   ;; https://github.com/oantolin/embark
     flycheck                 ;; https://flycheck.org
     flycheck-clang-tidy      ;;
     flycheck-color-mode-line ;;
     flycheck-elsa            ;; https://github.com/emacs-elsa/flycheck-elsa
     flycheck-indicator       ;;
     flycheck-package         ;; https://github.com/purcell/flycheck-package
     format-all               ;; https://github.com/lassik/emacs-format-all-the-code
     go-mode                  ;; https://github.com/dominikh/go-mode.el
     helm                     ;; https://github.com/emacs-helm/helm
     highlight-indentation    ;; https://github.com/antonj/Highlight-Indentation-for-Emacs
     hl-todo                  ;; https://github.com/tarsius/hl-todo
     js2-mode                 ;; https://github.com/mooz/js2-mode
     lsp-mode                 ;; https://github.com/emacs-lsp
     lsp-ui                   ;; https://github.com/emacs-lsp/lsp-ui
     magit                    ;; https://magit.org/
     markdown-mode            ;; https://github.com/jrblevin/markdown-mode
     multiple-cursors         ;; https://github.com/magnars/multiple-cursors.el
     org                      ;;
     package-lint             ;; https://github.com/purcell/package-lint
     php-mode                 ;;
     projectile               ;; https://docs.projectile.mx/projectile/installation.html
     protobuf-mode            ;;
     pulsar                   ;; https://github.com/protesilaos/pulsar
     pyenv-mode               ;; https://github.com/pythonic-emacs/pyenv-mode
     python                   ;;
     python-mode              ;;
     pyvenv-auto              ;; https://github.com/nryotaro/pyvenv-auto
     rainbow-delimiters       ;; https://github.com/Fanael/rainbow-delimiters
     reverse-im               ;; https://github.com/a13/reverse-im.el
     rg                       ;; https://github.com/dajva/rg.el
     russian-techwriter       ;; https://github.com/dunmaksim/emacs-russian-techwriter-input-method
     scala-mode               ;;
     swiper                   ;; https://github.com/abo-abo/swiper
     tempel                   ;; https://github.com/minad/tempel
     terraform-mode           ;;
     treemacs                 ;;
     treemacs-all-the-icons   ;;
     treemacs-icons-dired     ;;
     treemacs-magit           ;;
     undo-tree                ;; https://gitlab.com/tsc25/undo-tree
     vagrant                  ;; https://github.com/ottbot/vagrant.el
     verb                     ;;
     vertico                  ;; https://github.com/minad/vertico
     web-mode                 ;;
     wgrep                    ;; https://github.com/mhayashi1120/Emacs-wgrep
     which-key                ;; https://github.com/justbur/emacs-which-key
     ws-butler                ;; https://github.com/lewang/ws-butler
     yaml-mode                ;;
     yascroll                 ;; https://github.com/emacsorphanage/yascroll
     yasnippet                ;; https://github.com/joaotavora/yasnippet
     yasnippet-snippets       ;; https://github.com/AndreaCrotti/yasnippet-snippets
     ))

;; Проверка наличия индекса пакетов
(unless package-archive-contents (package-refresh-contents))

;; Установка необходимых пакетов
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents) ; Надо обновить: вдруг имеющаяся версия протухла?
  (dolist (pkg-name package-selected-packages)
    (unless (package-installed-p pkg-name)
      (message (format "Install package %s" pkg-name))
      (package-install pkg-name t))))


;; Изменение размеров окон
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "S-<SPC>") 'just-one-space) ; Заменить пробелы и TAB'ы до и после курсора на один пробел

(global-set-key (kbd "<escape>") 'keyboard-quit) ; ESC работает как и Ctrl+g, т. е. прерывает ввод команды
(global-set-key (kbd "C-z") 'undo) ; Отмена
(global-set-key (kbd "C-x k") 'kill-this-buffer) ; Закрыть буфер
;; (global-set-key (kbd "<C-tab>") 'mode-line-other-buffer) ; Перейти в другой буфер

(global-set-key (kbd "C-v") 'yank) ; Вставить текст из временного буфера
(global-set-key (kbd "C-x o") 'next-multiframe-window) ; Перейти в следующее окно
(global-set-key (kbd "C-x O") 'previous-multiframe-window) ; Перейти в предыдущее окно

(defun install-required-fonts ()
  "Выполняет установку шрифтов или говорит, как это сделать."
  ;; В Linux автоматически вызывается функция (all-the-icons-install-fonts).
  ;; В Windows такое не будет работать, поэтому пользователя просят установить шрифты,
  ;; скачав их с помощью этой же функции.
  (cond
    ((equal system-type "gnu-linux")
      (unless (file-directory-p "~/.local/share/fonts/")
        (all-the-icons-install-fonts)))
    ((equal system-type "windows-nt")
      (message "Скачайте шрифты с помощью команды all-the-icons-install-fonts.\nУстановите их и перезапустите EMACS."))))


(defun setup-gui-settings (frame-name)
  "Настройки, необходимые при запуске EMACS в графической среде.

  FRAME-NAME — имя фрейма, который настраивается."
  (when (display-graphic-p frame-name)
    (defvar availiable-fonts (font-family-list)) ;; Какие есть семейства шрифтов?
    (defvar default-font-family nil "Шрифт по умолчанию.")

    ;; Перебор шрифтов
    (cond
      ((member "Fira Code" availiable-fonts) (setq default-font-family "Fira Code"))
      ((member "DejaVu Sans Mono" availiable-fonts) (setq default-font-family "DejaVu Sans Mono"))
      ((member "Source Code Pro" availiable-fonts) (setq default-font-family "Source Code Pro"))
      ((member "Consolas" availiable-fonts) (setq default-font-family "Consolas")))

    (when default-font-family
      (message "Выбран шрифт по умолчанию.")
      ;;       (set-frame-font (format "-*-%s-normal-normal-normal-*-%d-*-*-*-m-0-iso10646-1" default-font-family default-font-height))
      ;;       (add-to-list 'default-frame-alist `(font . ,(format "%s-%d" default-font-family default-font-height)))
      ;;       (set-face-font 'markdown-inline-code-face default-font-family)
      ;;       (set-face-font 'markdown-table-face default-font-family)
      ;;       (set-face-font 'markup-meta-face default-font-family)
      ;;       (set-face-font 'markup-meta-hide-face default-font-family)
      ;;       (set-face-font 'markup-value-face default-font-family)
      ;; (set-face-font 'rainbow-delimiters-base-face default-font-family)
      (set-face-attribute 'default nil
        :family default-font-family)
      )

    (set-face-attribute 'default nil :height (* default-font-height 10))

    ;; Настройка иконочных шрифров и немножко GUI.
    (require 'all-the-icons)
    (require 'all-the-icons-dired)
    (require 'all-the-icons-ibuffer)
    (setq-default
      all-the-icons-ibuffer-human-readable-size t ;; Показывать размер файлов в ibuffer в человекочитаемом виде
      all-the-icons-ibuffer-icon t
      dashboard-set-file-icons t ;; Иконки типов файлов в графическом режиме
      dashboard-set-heading-icons t ;; Иконка EMACS в графическом режиме
      )
    (all-the-icons-ibuffer-mode t)

    (with-eval-after-load "centaur-tabs"
      (global-set-key (kbd "C-<prior>") 'centaur-tabs-backward)
      (global-set-key (kbd "C-<next>") 'centaur-tabs-forward))

    (with-eval-after-load "multiple-cursors"
      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))))

;; Правильный способ определить, что EMACS запущен в графическом режиме. Подробнее здесь:
;; https://emacsredux.com/blog/2022/06/03/detecting-whether-emacs-is-running-in-terminal-or-gui-mode/
(add-to-list 'after-make-frame-functions #'setup-gui-settings)


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


;; Save/close/open
(global-set-key (kbd "M-'") 'comment-or-uncomment-region)
(global-set-key (kbd "C-o") 'dired)


(global-set-key (kbd "<f3>") 'replace-string) ;; Поиск и замена
(global-set-key (kbd "<f7>") 'xah-new-empty-buffer) ;; Buffers and windows
(global-set-key (kbd "<f9>") 'sort-lines) ;; Sort lines
(global-set-key (kbd "<esc>") 'keyboard-quit) ;; Execute commands — like [g]

;; Переключение буферов с помощью Ctrl+X и стрелочек
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(global-set-key (kbd "M--") (lambda () (interactive) (insert "—"))) ;; Long dash by Alt+-

(global-unset-key (kbd "<insert>")) ;; Disable overwrite mode
(global-unset-key (kbd "M-,")) ;; Disable M-, as markers

;; Удалить буфер *scratch* после запуска EMACS.
(when (get-buffer "*scratch*") (kill-buffer "*scratch*"))


;; ABBREV-MODE
;; Встроенный режим
;; Аббревиатуры — это фрагменты текста, которые по нажатию [C-x, '] превращаются в другие конструкции
(require 'abbrev)
(setq-default
  abbrev-mode t ; Глобальный режим
  save-abbrevs 'silently) ; Сохранять добавленные аббревиатуры без лишних вопросов


;; ACE-WINDOW
;; https://github.com/abo-abo/ace-window
;; Быстрое переключение между окнами
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)


;; ADOC-MODE
;; https://github.com/bbatsov/adoc-mode
;; Работа с AsciiDoc
(add-to-list 'load-path "~/repo/adoc-mode/")
(require 'adoc-mode)
(defun setup-adoc-mode()
  "Настройки для `adoc-mode'."
  (setq-local completion-at-point-functions
    (cons #'tempel-expand completion-at-point-functions))
  (flycheck-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1)
  (buffer-face-mode t))
(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
(add-hook 'adoc-mode-hook #'setup-adoc-mode)


;; APT SOURCES LIST MODE
;; https://git.korewanetadesu.com/apt-sources-list.git
;; Режим для редактирования файлов настройки репозиториев APT
(require 'apt-sources-list)
(defun setup-apt-sources-list-mode ()
  "Настройки для `apt-sources-list-mode'."
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist (cons "\\.list$" 'apt-sources-list-mode))
(add-hook 'apt-sources-list-mode-hook #'setup-apt-sources-list-mode)


;; CENTAUR-TABS
;; https://github.com/ema2159/centaur-tabs
;; Вкладки с иконками и прочими удобствами
(require 'centaur-tabs)
(setq-default
  centaur-tabs-close-button "×"      ;; Будем использовать вот этот символ вместо X
  centaur-tabs-enable-key-bindings t ;; Включить комбинации клавиш из `centaur-tabs'.
  centaur-tabs-height 36             ;; Высота вкладок
  centaur-tabs-modified-marker t     ;; Показывать маркер, если содержимое вкладки изменилось
  centaur-tabs-set-bar 'under        ;; Доступные значения: over, under
  centaur-tabs-set-icons t           ;; Включить иконки. если это графический режим
  centaur-tabs-style "slant"         ;; Также доступны: bar, alternate, box, chamfer, rounded, slant, wawe, zigzag
  x-underline-at-descent-line t      ;; Если пакет используется вне Spacemacs, необходимо включить это, чтобы подчёркивание отображалось корректно
  )
(centaur-tabs-mode 1)
(add-hook 'dashboard-mode-hook #'centaur-tabs-local-mode)
(add-hook 'dired-mode-hook #'centaur-tabs-local-mode)


;; COMPANY-MODE
;; https://company-mode.github.io/
;; Автодополнение
(require 'company)
(require 'company-dabbrev)
(setq-default
  company-dabbrev-downcase nil
  company-dabbrev-ignore-case nil
  company-dabbrev-ignore-case nil
  company-idle-delay 0.5 ; Задержка вывода подсказки — полсекунды
  company-minimum-prefix-length 2 ;; Минимум 2 знака, чтобы company начала работать
  company-show-quick-access t ; Показывать номера возле потенциальных кандидатов
  company-tooltip-align-annotations t
  company-tooltip-limit 10 ;; Ограничение на число подсказок
  )
(global-set-key (kbd "<tab>") #'company-indent-or-complete-common)


;; COMPANY-WEB
;; Автодополнение для режима web-mode
(require 'company-web)
(require 'company-web-html)
(add-to-list 'company-backends 'company-web-html)


;; CONF MODE
;; Встроенный пакет. Основной режим для редактирования конфигурационных файлов.
(require 'conf-mode)
(add-to-list 'auto-mode-alist '("\\.env$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.flake8$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.ini$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.pylintrc$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.terraformrc$" . conf-mode))


;; CSS-MODE
;; Встроенный режим
(require 'css-mode)
(setq-default css-indent-offset 2)
(defun setup-css-mode ()
  "Настройки для `css-mode'."
  (company-mode 1)
  (display-line-numbers-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-to-list 'auto-mode-alist '("\\.css" . css-mode))
(add-hook 'css-mode-hook #'setup-css-mode)


;; Dashboard
;; https://github.com/emacs-dashboard/emacs-dashboard
;; Отображает дашборд при запуске EMACS
(require 'dashboard)
(setq-default
  dashboard-items
  '(
     (recents . 15)         ;; Последние открытые файлы
     (bookmarks . 10)       ;; Последние закладки
     (projects . 10)        ;; Последние проекты
     (agenda . 0)           ;; Агенда
     (registers . 0))       ;; Регистры
  dashboard-set-footer nil) ;; Скрыть "весёлые" надписи в нижней части дашборда
(dashboard-setup-startup-hook)


;; DEMAP
;; https://gitlab.com/sawyerjgardner/demap.el
;; Мини-карта
(require 'demap)
(global-set-key (kbd "<f4>") #'demap-toggle)
(setq-default demap-minimap-window-width 20) ; Ширина мини-карты


;; DESKTOP-SAVE-MODE
;; Встроенный пакет
;; Позволяет сохранять состояние EMACS между сессиями
(require 'desktop)
(setq-default
  desktop-modes-not-to-save '(dired-mode Info-mode info-lookup-mode) ; А вот эти не сохранять
  desktop-save t) ; Сохранять список открытых буферов, файлов и т. д. без лишних вопросов
(desktop-save-mode t)


;; DIFF-HL
;; https://github.com/dgutov/diff-hl
;; Показывает небольшие маркеры рядом с незафиксированными изменениями. Дополняет функциональность git-gutter,
;; которые показывает изменения только в обычных буферах. Этот пакет умеет работать с dired и другими режимами.
(require 'diff-hl)
(global-diff-hl-mode 1)


;; DIRED
;; Встроенный пакет для работы с файлами и каталогами.
(require 'dired)
(when (string-equal system-type "gnu/linux")
  ;; Это может не работать в Windows, надо проверить
  (setq-default dired-listing-switches "-lahX --group-directories-first"))
(add-hook 'dired-mode-hook #'auto-revert-mode)


;; DISPLAY-LINE-NUMBERS-MODE
;; Встроенный пакет
;; Показывает номера строк
(require 'display-line-numbers)
(setq-default display-line-numbers t)
(global-display-line-numbers-mode 1)


;; DOCKERFILE-MODE
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("^Dockerfile$" . dockerfile-mode))


;; DOOM-MODELINE
;; https://github.com/seagle0128/doom-modeline
;; Красивая и многофункциональная статусная панель
(require 'doom-modeline)
(setq-default
  doom-modeline-buffer-encoding t ; Кодировка
  doom-modeline-buffer-modification-icon t ; Наличие изменений
  doom-modeline-buffer-name t ; Имя буфера
  doom-modeline-buffer-state-icon t ; Состояние буфера
  doom-modeline-env-enable-go t ; Показывать версию Golang
  doom-modeline-env-enable-python t ; Показывать версию Python
  doom-modeline-env-enable-ruby t ; Показывать версию Ruby
  doom-modeline-hud t ;
  doom-modeline-icon t ; Иконки
  doom-modeline-indent-info t ; Информация об отступах
  doom-modeline-lsp t ; Статус LSP
  doom-modeline-major-mode-color-icon t ; Иконка основного режима вместо текста
  doom-modeline-major-mode-icon t
  doom-modeline-project-detection 'auto
  doom-modeline-vcs-max-length 0)
(doom-modeline-mode 1)


;; LOAD THEME
(require 'doom-themes)
(load-theme 'doom-monokai-classic t)
(doom-themes-org-config)
(doom-themes-visual-bell-config)


;; EASY KILL
;; https://github.com/leoliu/easy-kill
;; Удобнее работать с удалением текстовых блоков
(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)


;; EDITORCONFIG EMACS
;; Поддержка https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(require 'editorconfig)
(setq-default editorconfig-trim-whitespaces-mode 'ws-butler-mode)
(editorconfig-mode 1)


;; ELEC-PAIR MODE
;; Встроенный пакет.
;; Автоматически вставляет при вводе одной скобки парную ей. Если
;; выделен регион, то в скобки обрамляется он.
(require 'elec-pair)
(add-to-list 'electric-pair-pairs '(?« . ?»))
(add-to-list 'electric-pair-pairs '(?{ . ?}))
(electric-pair-mode t) ;; Глобальный режим


;; EMACS LISP MODE
;; IT IS NOT A ELISP-MODE!
;; Встроенный пакет для EMACS Lisp
(defun setup-emacs-lisp-mode ()
  "Настройки для `emacs-lisp-mode'."
  (aggressive-indent-mode 1)
  (checkdoc-minor-mode 1)
  (electric-indent-mode 1)
  (flymake-mode 1)
  (flycheck-elsa-setup)
  (highlight-indentation-mode 1)
  (highlight-indentation-set-offset 2)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.el$'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\abbrev_defs$" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook #'setup-emacs-lisp-mode)


;; FLYCHECK
;; https://flycheck.org
;; Проверка синтаксиса на лету с помощью статических анализаторов
(require 'flycheck)
(require 'flycheck-color-mode-line) ;; https://github.com/flycheck/flycheck-color-mode-line
(setq-default
  flycheck-check-syntax-automatically '(mode-enabled save new-line)
  flycheck-locate-config-file-functions '(
                                           flycheck-locate-config-file-by-path
                                           flycheck-locate-config-file-ancestor-directories
                                           flycheck-locate-config-file-home)
  flycheck-highlighting-mode 'lines
  flycheck-indication-mode 'left-fringe
  flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc")
(add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode)
(global-flycheck-mode 1)


;; FLYCHECK ELSA
;; https://github.com/emacs-elsa/flycheck-elsa
(require 'flycheck-elsa)
(setq-default flycheck-elsa-backend 'cask)
(unless (executable-find "cask")
  (push (format "/home/%s/.local/bin/cask" (user-login-name)) exec-path))


;; FLYCHECK-PACKAGE
;; https://github.com/purcell/flycheck-package
;; Проверка пакетов с помощью Flycheck
(require 'flycheck-package)
(eval-after-load 'flycheck '(flycheck-package-setup))


;; FLYSPELL-MODE
;; Проверка орфографии с помощью словарей
(require 'flyspell)
(when
  (and
    (string-equal system-type "gnu/linux") ;; Aspell для Linux, в Windows без проверки орфографии
    (file-exists-p "/usr/bin/aspell") ;; Надо убедиться, что программа установлена в ОС
    )
  (setq-default ispell-program-name "/usr/bin/aspell"))


;; FORMAT-ALL
;; https://github.com/lassik/emacs-format-all-the-code
;; Форматирование кода по нажатию [F12]
(require 'format-all)
(global-set-key (kbd "<f12>") 'format-all-buffer)


;; GO-MODE
;; https://github.com/dominikh/go-mode.el
;; Поддержка Golang
(require 'go-mode)
(defun setup-go-mode ()
  "Настройки `go-mode'."
  (lsp-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.go$'" . go-mode))
(add-hook 'go-mode-hook #'setup-go-mode)


;; HELM
;; https://github.com/emacs-helm/helm
;; Подсказки в минибуфере, и не только
(require 'helm)
(setq-default completion-styles '(flex))
(global-set-key (kbd "C-S-p") 'helm-M-x)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(helm-mode 1)


;; HIGHLIGHT-INDENTATION-MODE
;; https://github.com/antonj/Highlight-Indentation-for-Emacs
;; Показывает направляющие для отступов
(require 'highlight-indentation)
;;(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-face "#4d4d4d")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
(set-minor-mode
  'highlight-indentation-mode
  '(
     python-mode
     terraform-mode
     ))

;; HL-TODO
;; https://github.com/tarsius/hl-todo
;; Подсветка TODO, FIXME и т. п.
(require 'hl-todo)
(global-hl-todo-mode t)


;; IBUFFER
;; Встроенный пакет для удобной работы с буферами.
;; По нажатию F2 выводит список открытых буферов.
(defalias 'list-buffers 'ibuffer)
(setq-default
  ibuffer-expert 1 ; Расширенный  режим для ibuffer
  ibuffer-hidden-filter-groups (list "Helm" "*Internal*")
  ibuffer-show-empty-filter-groups nil ; Если группа пустая, ibuffer не должен её отображать.
  ibuffer-sorting-mode 'filename/process ; Сортировать файлы в ibuffer по имени / процессу.
  ibuffer-truncate-lines nil ; Не обкусывать строки в ibuffer
  ibuffer-use-other-window nil ; Не надо открывать ibuffer в другом окне, пусть открывается в текущем
  ibuffer-saved-filter-groups
  '(
     ("default"
       ("Dired" (mode . dired-mode))
       ("Org" (mode . org-mode))
       ("Markdown" (mode . markdown-mode))
       ("AsciiDoc" (mode . adoc-mode))
       ("ReStructured Text" (mode . rst-mode))
       ("EMACS Lisp" (mode . emacs-lisp-mode))
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
           (mode . elpy-mode)
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
  "Настройки `ibuffer-mode'."
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))
(add-hook 'ibuffer-mode-hook #'setup-ibuffer-mode)
(global-set-key (kbd "<f2>") 'ibuffer)


;; JAVA-MODE
;; Встроенный режим из пакета `cc-mode'.
(require 'cc-mode)
(defun setup-java-mode ()
  "Настройки для `java-mode'."
  (aggressive-indent-mode 1)
  (company-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'java-mode-hook #'setup-java-mode)



;; JS2-MODE
;; https://github.com/mooz/js2-mode
(require 'js2-mode)
(defun setup-js2-mode ()
  "Настройки для `js2-mode'."
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook #'setup-js2-mode)


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
(require 'lsp-mode)
(require 'lsp-ui)
(setq-default
  lsp-headerline-breadcrumb-enable t ; Показывать "хлебные крошки" в заголовке
  lsp-modeline-diagnostics-enable t ; Показывать ошибки LSP в статусной строке
  lsp-ui-doc-enable t
  lsp-ui-peek-always-show t
  lsp-ui-peek-enable t
  lsp-ui-sideline-enable t)


;; MAGIT
;; https://magit.vc/
;; Magic + Git + Git-gutter. Лучшее средство для управления Git.
(require 'magit)
(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'magit-checkout)


;; MAKEFILE
;; Встроенный пакет для работы с Makefile
(defun setup-makefile-mode ()
  "Настройка режима `makefile-mode'."
  (setq indent-tabs-mode t)
  (highlight-indentation-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'makefile-gmake-mode-hook #'setup-makefile-mode)
(add-hook 'makefile-mode-hook #'setup-makefile-mode)


;; MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
;; Режим для работы с файлами в формате Markdown
(require 'markdown-mode)
(setq
  header-line-format " "
  ;; left-margin-width 4
  markdown-fontify-code-blocks-natively t ; Подсвечивать синтаксис в примерах кода
  markdown-header-scaling-values '(1.0 1.0 1.0 1.0 1.0 1.0) ; Все заголовки одной высоты
  markdown-list-indent-width 4 ;; Размер отступа для выравнивания вложенных списков
  word-wrap t ; Перенос по словам
  )
(defun setup-markdown-mode()
  "Settings for `markdown-mode'."
  (interactive)
  (setq left-margin-width 4)
  (highlight-indentation-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(define-key markdown-mode-map (kbd "M-.") 'markdown-follow-thing-at-point)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook #'setup-markdown-mode)


;; MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
;; Позволяет использовать мультикурсорность.
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; NXML-MODE
;; Встроенный пакет
;; Почти как xml-mode, только лучше и новее
(require 'nxml-mode)
(add-to-list 'auto-mode-alist '("\\.xml$'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.pom$'" . nxml-mode))
(defun setup-nxml-mode ()
  "Настройки `nxml-mode'."
  (setq-local
    nxml-attribute-indent 4 ; Выравнивание атрибутов
    nxml-auto-insert-xml-declaration-flag nil ; Не вставлять декларацию
    nxml-bind-meta-tab-to-complete-flag t ; Использовать TAB для завершения ввода
    nxml-child-indent 4 ; Выравнивание дочерних элементов
    nxml-slash-auto-complete-flag t) ; Закрывать теги по вводу /
  (aggressive-indent-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'nxml-mode-hook #'setup-nxml-mode)


;; ORG-MODE
;; https://orgmode.org/
;; Органайзер, и не только
(require 'org)
(defun setup-org-mode ()
  "Настройки для `org-mode'."
  (setq
    truncate-lines nil
    left-margin-width 4
    org-todo-keywords '((
                          sequence
                          "НОВАЯ"
                          "|"
                          "ВЫПОЛНЕНА"))
    right-margin-width 4
    word-wrap t)
  (rainbow-delimiters-mode 1))
(add-to-list 'auto-mode-alist '("\\.org$'" . org-mode))


;; PHP-MODE
(require 'php)
(defun setup-php-mode ()
  "Настройки для `php-mode'."
  (aggressive-indent-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-hook 'php-mode-hook #'setup-php-mode)


;; PIXEL-SCROLL-PRECISION-MODE
;; TODO: что это?
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))


;; PROG-MODE
;; Фундаментальный режим, на основе которого сделаны все режимы программирования.
(require 'prog-mode)
(add-hook 'prog-mode-hook #'company-mode)
(add-hook 'prog-mode-hook #'flycheck-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'ws-butler-mode)


;; PROJECTILE
;; https://docs.projectile.mx/projectile/installation.html
;; Управление проектами. Чтобы каталог считался проектом, он должен быть
;; под контролем любой системы версионирования, либо содержать специальные
;; файлы. В крайнем случае сгодится пустой файл .projectile
;; Подробнее здесь: https://docs.projectile.mx/projectile/projects.html
(require 'projectile)
(setq projectile-completion-system 'company)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; PROTOBUF-MODE
;; https://github.com/emacsmirror/protobuf-mode
;; Работа с файлами Protobuf: подсветка синтаксиса, переход по ссылками и т. д.
(require 'protobuf-mode)
(defun setup-protobuf-mode ()
  "Настройки `protobuf-mode'."
  (aggressive-indent-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
(add-hook 'protobuf-mode-hook #'setup-protobuf-mode)


;; PULSAR-MODE
;; https://github.com/protesilaos/pulsar
;; Подсвечивать курсор при его перемещении на несколько строк
(require 'pulsar)
(setq pulsar-pulse t)
(add-hook 'next-error-hook #'pulsar-pulse-line)
(pulsar-global-mode 1)


;; PYTHON-MODE
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
  (lsp-mode 1)
  (pyvenv-mode 1)
  (rainbow-delimiters-mode 1)
  (setq-local python-indent-offset 4)
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (define-key python-mode-map (kbd "M-/") 'jedi:show-doc)
  (define-key python-mode-map (kbd "M-?") 'helm-jedi-related-names))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-hook 'python-mode-hook #'setup-python-mode)
(add-hook 'python-mode-hook #'pyvenv-mode)


;; RUSSIAN-TECHWRITER
;; Метод ввода для технических писателей
;; https://github.com/dunmaksim/emacs-russian-techwriter-input-method
(require 'russian-techwriter)
(setq-default default-input-method 'russian-techwriter)


;; REVERSE-IM
;; https://github.com/a13/reverse-im.el
;; Чтобы сочетания клавиш работали в любой раскладке.
(require 'reverse-im)
(setq-default reverse-im-input-methods '(
                                          "russian-computer"
                                          "russian-techwriter"))
(reverse-im-mode t)


;; RST-MODE
;; Основной режим для редактирования reStructutedText
;; Больше здесь: https://www.writethedocs.org/guide/writing/reStructuredText/
(require 'rst)
(defun setup-rst-mode ()
  "Настройки для `rst-mode'."
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1)
  )
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
(add-hook 'rst-mode-hook #'setup-rst-mode)


;; RUBY-MODE
;; Это встроенный пакет
;;Поддержка Ruby on Rails
(require 'ruby-mode)
(defun setup-ruby-mode ()
  "Настройки для `ruby-mode'."
  (highlight-indentation-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.rb$" .ruby-mode))


;; RG (ripgrep)
;; https://github.com/dajva/rg.el
;; Для работы пакета требуется наличие в системе утилиты ripgrep
(require 'rg)
(rg-enable-default-bindings)


;; SCALA MODE
;; https://github.com/hvesalai/emacs-scala-mode
(require 'scala-mode)
(defun setup-scala-mode ()
  "Настройки `scala-mode'."
  (aggressive-indent-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sc$" . scala-mode))
(add-hook 'scala-mode-hook #'setup-scala-mode)


;; SHELL-SCRIPT-MODE
(require 'sh-script)
(defun setup-shell-script-mode ()
  "Настройки для `sh-script'."
  (aggressive-indent-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.bashrc$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.profile$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.sh$" . shell-script-mode))


;; SQL MODE
;; Это встроенный режим
(require 'sql)
(defun setup-sql-mode ()
  "Настройки `sql-mode'."
  (aggressive-indent-mode 1)
  (flycheck-mode 1)
  (lsp-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))
(add-hook 'sql-mode-hook #'setup-sql-mode)


;; SWIPER MODE
;; https://github.com/abo-abo/swiper
;; Пакет для быстрого поиска.
;; По кажатию C-7 можно выполнить быстрое редактирование найденных фрагментов, но чтобы
;; оно сработало правильно, нужно добавить команду swiper-mc в список mc/cmds-to-run-once.
(require 'swiper)
(add-to-list 'mc/cmds-to-run-once 'swiper-mc)
(global-set-key (kbd "C-s") 'swiper-isearch)


;; TEMPEL
;; https://github.com/minad/tempel
;; Система шаблонов, более новая, чем `tempo.el'. Основные функции:
;; * `tempel-complete' — завершить ввод шаблона и раскрыть его
;; * `tempel-expand' — раскрыть введенный шаблон
;; * `tempel-insert' — выбрать шаблон из списка и вставить в позицию под курсором
(require 'tempel)
(global-set-key (kbd "M-+") #'tempel-complete)
(global-set-key (kbd "M-*") #'tempel-insert)


;; TERRAFORM-MODE
;; https://github.com/emacsorphanage/terraform-mode
;; Работа с файлами конфигурации Terraform
(require 'terraform-mode)
(defun setup-terraform-mode ()
  "Настройка `terraform-mode'."
  (setq-default flycheck-checker 'terraform)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist (cons "\\.tf$" 'terraform-mode))
(add-hook 'terraform-mode-hook #'setup-terraform-mode)


;; TEXT-MODE
;; Фундаментальный режим, активный во всех буферах с "простым" текстом.
(require 'text-mode)
(defun setup-text-mode ()
  "Настройки для всех режимов на базе `text-mode'."
  (ispell-minor-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'text-mode-hook #'setup-text-mode)


;; TOOLBAR-MODE
;; Встроенная панель с кнопками. Не нужна. В EMACS NOX вовсе отсутствует.
;; Перед отключением нужно проверить, что такая функциональность поддерживается.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))


;; TOOLTIP-MODE
;; Встроенный режим. Показывает подсказки, используя возможности GUI.
;; Не нужен, поэтому лучше выключить (всё будет отображаться в мини-буфере).
(when (fboundp 'tooltip-mode)
  (tooltip-mode nil))


;; TREEMACS — awesome file manager (instead NeoTree)
;; https://github.com/Alexander-Miller/treemacs
;; Дерево файлов и каталогов
(require 'treemacs)
(setq-default treemacs-width 35)
(defun treemacs-get-ignore-files (filename absolute-path)
  "Не показывать в дереве имена указанных файлов и каталогов.

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
(require 'treemacs-icons)
(add-hook 'dired-mode-hook 'treemacs-icons-dired-enable-once)


;; UNDO-TREE
;; Не только предоставляет привычное поведение при отмене команд, но и даёт мощные возможности по
;; ведению дерева правок.
(require 'undo-tree)
(setq undo-tree-auto-save-history nil) ; Отключить создание резервных копий файлов
(global-undo-tree-mode 1)


;; VAGRANT MODE
;; Работа с файлами Vagrant
;; https://github.com/ottbot/vagrant.el
(require 'vagrant)
(add-to-list 'auto-mode-alist (cons "Vagrantfile$\\'" 'vagrant-mode))


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
(vertico-mode 1)


;; WEB-MODE
;; https://web-mode.org/
(require 'web-mode)
(setq-default
  initial-major-mode 'web-mode
  web-mode-attr-indent-offset 4
  web-mode-css-indent-offset 2 ;; CSS
  web-mode-enable-block-face t
  web-mode-enable-css-colorization t ;; Код или имя цвета при редактировании CSS будут отмечены фоном этого цвета
  web-mode-enable-current-element-highlight t
  web-mode-markup-indent-offset 2)
(defun setup-web-mode ()
  "Настройки `web-mode'."
  (highlight-indentation-mode 1)
  (highlight-indentation-set-offset 2)
  (rainbow-delimiters-mode 1)
  (whitespace-mode t)
  (ws-butler-mode t))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-hook 'web-mode-hook #'setup-web-mode)


;; WHICH-KEY MODE
;; https://github.com/justbur/emacs-which-key
;; Показывает подсказки к командам.
(require 'which-key)
(which-key-setup-side-window-right)
(which-key-mode 1)


;; WHITESPACE MODE
;; Встроенный пакет для отображения невидимых символов.
(require 'whitespace)
(setq-default
  whitespace-display-mappings
  '(
     (space-mark   ?\    [?\xB7]     [?.])      ;; Пробел
     (space-mark   ?\xA0 [?\xA4]     [?_])      ;; Неразрывный пробел
     (newline-mark ?\n   [?¶ ?\n]    [?$ ?\n])  ;; Конец строки
     (tab-mark     ?\t   [?\xBB ?\t] [?\\ ?\t]) ;; TAB
     )
  whitespace-line-column 1000 ;; По умолчанию подсвечиваются длинные строки
  )
(set-face-attribute 'whitespace-space nil :foreground "#75715E")
(set-face-attribute 'whitespace-indentation nil :foreground "#E6DB74")


;; YAML-MODE
;; https://github.com/yoshiki/yaml-mode
;; Работа с YAML-файлами
(require 'yaml-mode)
(defun setup-yaml-mode ()
  "Настройки для `yaml-mode'."
  (highlight-indentation-mode 1)
  (rainbow-delimiters-mode 1))
(add-to-list 'auto-mode-alist '("\\.ansible\\-lint" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.pre\\-commit\\-config\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yamllint$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yamllint\\-config\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yfm$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook #'setup-yaml-mode)


;; YASCROLL-MODE
;; https://github.com/emacsorphanage/yascroll
;; Альтернативная полоса прокрутки
(require 'yascroll)
(global-yascroll-bar-mode 1)


;; YASNIPPET
;; http://github.com/joaotavora/yasnippet
;; Предоставляет функциональность сниппетов — блоков кода, в которые всего-лишь нужно подставить значения.
(require 'yasnippet)
;; Если директории для сниппектов нет, её нужно создать.
(defvar yas-snippet-root-dir (concat emacs-config-dir "snippets"))
(unless (file-directory-p yas-snippet-root-dir)
  (mkdir yas-snippet-root-dir))
(yas-global-mode 1)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setup-gui-settings (selected-frame))


;; Загрузка CUSTOM FILE
(setq custom-file (expand-file-name "custom.el" emacs-config-dir))
(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init.el)
;;; init.el ends here
