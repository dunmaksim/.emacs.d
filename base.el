;;; base.el --- настройки, которые должны быть выполнены несмотря ни на что
;;; Commentary:
;;; Здесь находятся настройки базовой функциональности Emacs.
;;; Даже если будут какие-то проблемы со сторонними пакетами, этот код всё
;;; равно будет выполнен.
;;; По этой же причине здесь нет ничего, что могло бы сломаться.
;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p) ;; Использовать y и n вместо yes и no (сокращает объём вводимого текста для подтверждения команд)

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

;; Если используется старая версия EMACS, нужно указать параметры протокола TLS.
;; В противном случае будут проблемы при загрузке архива пакетов.
(when (< emacs-major-version 27)
  (require 'gnutls)
  (customize-set-variable
   'gnutls-algorithm-priority
   "NORMAL:-VERS-TLS1.3"
   "Необходимо для старых версий Emacs."))


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
          (customize-set-variable 'source-directory init-el-emacs-source-path)
          (message (format "Исходный код обнаружен в каталоге %s" init-el-emacs-source-path))))
    ;; Каталог не существует
    (message (format "Каталог %s не существует." init-el-emacs-source-path))))

(custom-set-variables
 '(compilation-scroll-output t "Автоматическая прокрутка буфера *compilation*")
 '(create-lockfiles nil "Не создавать lock-файлы")
 '(cursor-type 'bar "Курсор в виде вертикальной черты")
 '(custom-safe-themes t "Считать все темы безопасными")
 '(default-input-method "russian-computer" "Метод ввода по умолчанию")
 '(default-transient-input-method "russian-computer")
 '(delete-by-moving-to-trash t "Удалять файлы в Корзину")
 '(gc-cons-threshold (* 50 1000 1000) "Увеличить размер памяти для сборщика мусора")
 '(indent-tabs-mode nil "Отключить `indent-tabs-mode'.")
 '(inhibit-startup-screen t "Не показывать приветственный экран")
 '(initial-scratch-message nil "Пустой буфер *scratch*")
 '(load-prefer-newer t "Если есть файл elc, но el новее, загрузить el-файл.")
 '(read-file-name-completion-ignore-case t "Игнорировать регистр при вводе имён файлов")
 '(ring-bell-function 'ignore "Отключить звуковое сопровождение событий")
 '(save-place-file (expand-file-name ".emacs-places" user-emacs-directory) "Хранить данные о позициях в открытых файлах в .emacs-places")
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


(when (fboundp 'menu-bar-mode)
  (customize-set-variable 'menu-bar-mode nil "Выключить отображение меню"))

(when (fboundp 'tool-bar-mode)
  (customize-set-variable 'tool-bar-mode nil "Выключить отображение панели инструментов"))

(require 'keymap)

(keymap-global-unset "<insert>") ;; Режим перезаписи не нужен
(keymap-global-unset "M-,")      ;; Такие маркеры не нужны
(keymap-global-unset "C-z")      ;; Такой Ctrl+Z нам не нужен
(keymap-global-set "C-x k"       ;; Закрыть буфер по нажатию [C-x k]
                   (lambda()
                     (interactive)
                     (kill-buffer (current-buffer))))
(keymap-global-set "M--"         ;; Вставка длинного тире
                   (lambda()
                     (interactive)
                     (insert "—")))


;; 📦 ABBREV-MODE
;; Встроенный пакет.
;; Использование аббревиатур -- фрагментов текста, которые при вводе
;; определённой последовательности символов заменяются на другую,
;; например:
;; tf → Terraform
;; yc → Yandex Cloud
;; Это встроенный пакет
(require 'abbrev)


;; 📦 AUTOREVERT
;; Встроенный пакет.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
;; Автоматическое обновление буферов.
;; По умолчанию `global-auto-revert-mode' работает только с файловыми
;; буферами.
(require 'autorevert)
(customize-set-variable 'auto-revert-check-vc-info t "Автоматически обновлять статусную строку")
;; Автоматически перезагружать файловый буфер при изменении файла на диске.
(global-auto-revert-mode 1)
;; Включить автообновление буферов с `dired-mode'.
(add-hook 'dired-mode-hook 'auto-revert-mode)


;; 📦 CALENDAR
;; Встроенный пакет
(require 'calendar)
(customize-set-variable 'calendar-week-start-day 1 "Начнём неделю с понедельника.")


;; 📦 CHECKDOC
;; Встроенный пакет для проверки строк документации.
(require 'checkdoc)
(customize-set-variable 'checkdoc-minor-mode-string " CheckDoc")
(add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)


;; 📦 CONF-MODE
;; Встроенный пакет.
;; Основной режим для редактирования конфигурационных файлов INI/CONF
(require 'conf-mode)
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.flake8\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.ini\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.pylintrc\\'" . conf-mode))


;; 📦 CSS-MODE
;; Встроенный пакет.
;; Поддержка CSS.
(require 'css-mode)
(customize-set-variable 'css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.css" . css-mode))


;; 📦 DELSEL
;; Встроенный пакет.
;; Используется для управления удалением выделенного текста.
(require 'delsel)
(delete-selection-mode t) ;; Удалять выделенный фрагмент при вводе текста


;; 📦 DESKTOP
;; Встроенный пакет.
;; Сохранение состояния Emacs между сессиями.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(require 'desktop)
(custom-set-variables
 '(desktop-auto-save-timeout 20 "Автосохранение каждые 20 секунд.")
 '(desktop-dirname user-emacs-directory "Каталог для хранения файла .desktop.")
 '(desktop-load-locked-desktop t "Загрузка файла .desktop даже если он заблокирован.")
 '(desktop-modes-not-to-save '(dired-mode Info-mode info-lookup-mode)) ; А вот эти не сохранять
 '(desktop-restore-frames t "Восстанавливать фреймы.")
 '(desktop-save t "Сохранять список открытых буферов, файлов и т. д. без лишних вопросов."))
(desktop-save-mode 1)
(add-hook 'server-after-make-frame-hook 'desktop-read)


;; 📦 DIRED
;; Встроенный пакет для работы с файлами и каталогами.
;; Клавиши:
;; [+] - создание каталога.
;; [C-x C-f] - создание файла с последующим открытием буфера.
(require 'dired)
(custom-set-variables
 '(dired-kill-when-opening-new-dired-buffer t "Удалять буфер при переходе в другой каталог.")
 '(dired-listing-switches "-l --human-readable --all --group-directories-first"))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)


;; 📦 DISPLAY-LINE-NUMBERS-MODE
;; Встроенный пакет
;; Показывает номера строк
(require 'display-line-numbers)
(add-hook 'adoc-mode-hook 'display-line-numbers-mode)
(add-hook 'c-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'css-mode-hook 'display-line-numbers-mode)
(add-hook 'csv-mode-hook 'display-line-numbers-mode)
(add-hook 'dockerfile-mode-hook 'display-line-numbers-mode)
(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)
(add-hook 'html-mode-hook 'display-line-numbers-mode)
(add-hook 'javascript-mode-hook 'display-line-numbers-mode)
(add-hook 'json-mode-hook 'display-line-numbers-mode)
(add-hook 'latex-mode-hook 'display-line-numbers-mode)
(add-hook 'lisp-data-mode-hook 'display-line-numbers-mode)
(add-hook 'makefile-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)
(add-hook 'nxml-mode-hook 'display-line-numbers-mode)
(add-hook 'po-mode-hook 'display-line-numbers-mode)
(add-hook 'python-mode-hook 'display-line-numbers-mode)
(add-hook 'rst-mode-hook 'display-line-numbers-mode)
(add-hook 'ruby-mode-hook 'display-line-numbers-mode)
(add-hook 'sh-mode-hook 'display-line-numbers-mode)
(add-hook 'shell-script-mode-hook 'display-line-numbers-mode)
(add-hook 'terraform-mode-hook 'display-line-numbers-mode)
(add-hook 'tex-mode-hook 'display-line-numbers-mode)
(add-hook 'web-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)


;; 📦 ELECTRIC-INDENT MODE
;; Встроенный пакет.
;; Автоматический отступ. В основном только мешает, лучше выключить.
(require 'electric)
(customize-set-variable 'electric-indent-inhibit t "Не выравнивать предыдущую строку по нажатию RET.")
(add-hook 'emacs-lisp-mode-hook 'electric-indent-local-mode)


;; 📦 ELEC-PAIR MODE
;; Встроенный пакет.
;; Автоматически вставляет при вводе одной скобки или кавычки парную ей. Если
;; выделен регион, то в скобки обрамляется он.
(require 'elec-pair)
(add-to-list 'electric-pair-pairs '(?\( . ?\))) ;; ()
(add-to-list 'electric-pair-pairs '(?\[ . ?\])) ;; []
(add-to-list 'electric-pair-pairs '(?{ . ?}))   ;; {}
(add-to-list 'electric-pair-pairs '(?« . ?»))   ;; «»
(add-to-list 'electric-pair-pairs '(?‘ . ’?))   ;; ‘’
(add-to-list 'electric-pair-pairs '(?‚ . ‘?))   ;; ‚‘
(add-to-list 'electric-pair-pairs '(?“ . ”?))   ;; “”

(add-hook 'adoc-mode-hook 'electric-pair-local-mode)
(add-hook 'conf-mode-hook 'electric-pair-local-mode)
(add-hook 'emacs-lisp-data-mode-hook 'electric-pair-local-mode)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-local-mode)
(add-hook 'lisp-data-mode-hook 'electric-pair-local-mode)
(add-hook 'markdown-mode-hook 'electric-pair-local-mode)
(add-hook 'python-mode-hook 'electric-pair-local-mode)
(add-hook 'ruby-mode-hook 'electric-pair-local-mode)


;; 📦 EMACS-LISP-MODE
;; IT IS NOT A ELISP-MODE!
;; Встроенный пакет для EMACS Lisp
(require 'elisp-mode)
(add-to-list 'auto-mode-alist '("\\.abbrev_defs\\'" . lisp-data-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))


;; 📦 FACE-REMAP
;; Встроенный пакет.
;; Отображение шрифтов в графическом режиме.
(require 'face-remap)
(customize-set-variable 'text-scale-mode-step 1.1 "Шаг увеличения масштаба")


;; 📦 FILES
;; Это встроенный пакет для управления файлами
(require 'files)
(custom-set-variables
 '(auto-save-file-name-transforms `((".*" , init-el-autosave-dir) t))
 '(delete-old-versions t "Удалять старые резервные копии файлов без лишних вопросов")
 '(enable-local-eval t "Разрешить инструкцию вызов `eval' в `.dir-locals.el'")
 '(enable-local-variables t "Считать все переменные из файлов `.dir-locals.el' безопасными")
 '(large-file-warning-threshold (* 100 1024 1024) "Предупреждение при открытии файлов больше 100 МБ (по умолчанию — 10 МБ)")
 '(make-backup-files nil "Резервные копии не нужны, у нас есть undo-tree")
 '(save-abbrevs 'silently "Сохранять аббревиатуры без лишних вопросов"))
(add-to-list 'safe-local-variable-values '(buffer-env-script-name . ".venv/bin/activate"))
(add-to-list 'safe-local-variable-values '(electric-pair-preserve-balance . t))
(add-to-list 'safe-local-variable-values '(emacs-lisp-docstring-fill-column . 80))
(add-to-list 'safe-local-variable-values '(fill-column . 120))
(add-to-list 'safe-local-variable-values '(fill-column . 80))
(add-to-list 'safe-local-variable-values '(frozen_string_literal . true))
(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(projectile-project-compilation-cmd . "make dirhtml"))
(add-to-list 'safe-local-variable-values '(projectile-project-test-cmd . "pre-commit run --all"))


;; 📦 FILL-COLUMN
;; Встроенный пакет.
;; Отображение рекомендуемой границы символов.
(require 'display-fill-column-indicator)
(add-hook 'emacs-lisp-mode-hook 'display-fill-column-indicator-mode)


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
      ;; Программа для проверки орфографии найдена
      (progn
        (message (format "Для проверки орфографии используется %s" text-spell-program))
        (require 'flyspell)
        (customize-set-variable 'ispell-program-name text-spell-program)
        (add-hook 'adoc-mode-hook 'flyspell-mode)
        (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
        (add-hook 'markdown-mode-hook 'flyspell-mode)
        (add-hook 'rst-mode-hook 'flyspell-mode)
        (add-hook 'text-mode-hook 'flyspell-mode))
    ;; Не найдено программ для проверки орфографии
    (message "Не найдено программ для проверки орфографии.")))


;; 📦 FRAME
;; Встроенный пакет.
;; Управление фреймами.
(require 'frame)
(custom-set-variables
 '(window-divider-default-places 't "Разделители окон со всех сторон (по умолчанию только справа)")
 '(window-divider-default-right-width 3  "Ширина в пикселях для линии-разделителя окон"))
(window-divider-mode t) ;; Отображать разделитель между окнами
(keymap-global-set "C-x O" 'previous-multiframe-window) ;; Перейти в предыдущее окно
(keymap-global-set "C-x o" 'next-multiframe-window)     ;; Перейти в следующее окно


;; 📦 GOTO-ADDRESS-MODE
;; Встроенный пакет.
;; Подсвечивает ссылки и позволяет переходить по ним с помощью [C-c RET].
;; Возможны варианты (зависит от основного режима).
(require 'goto-addr)
(add-hook 'adoc-mode-hook 'goto-address-mode)
(add-hook 'emacs-lisp-mode-hook 'goto-address-mode)
(add-hook 'markdown-mode-hook 'goto-address-mode)
(add-hook 'rst-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'web-mode-hook 'goto-address-mode)


;; 📦 GREP
;; Встроенный пакет.
;; Поиск с помощью `grep'.
(require 'grep)
(keymap-global-set "<f6>" 'find-grep) ;; Запуск `find-grep' по нажатию [F6].


;; 📦 HL-LINE
;; Встроенный пакет.
;; Подсветка текущей строки.
(require 'hl-line)
(global-hl-line-mode 1)


;; 📦 IBUFFER
;; Встроенный пакет для удобной работы с буферами.
;; По нажатию F2 выводит список открытых буферов.
;; Взято из конфига автора пакета
;; https://github.com/jwiegley/dot-emacs/blob/master/init.org
(require 'ibuffer)
(custom-set-variables
 '(ibuffer-formats ;; Форматирование вывода
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
 '(ibuffer-default-sorting-mode 'filename/process "Сортировать файлы по имени / процессу")
 '(ibuffer-expert 1 "Не запрашивать подтверждение для опасных операций")
 '(ibuffer-truncate-lines nil "Не обкусывать длинные строки")
 '(ibuffer-use-other-window t "Открывать буфер *Ibuffer* в отдельном окне"))
(defalias 'list-buffers 'ibuffer "Замена стандартной функции на ibuffer.")
(keymap-global-set "<f2>" 'ibuffer)


;; 📦 IBUF-EXT
;; Встроенный пакет.
;; Дополнительные настройки `ibuffer'.
(require 'ibuf-ext)
(custom-set-variables
 '(ibuffer-saved-filter-groups                    ;; Группы по умолчанию
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
        (mode . editorconfig-conf-mode)
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
 '(ibuffer-hidden-filter-groups (list "*Internal*" ) "Не показывать эти буферы")
 '(ibuffer-show-empty-filter-groups nil "Не показывать пустые группы"))
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(add-hook 'ibuffer-mode-hook #'(lambda ()(ibuffer-switch-to-saved-filter-groups "default")))


;; 📦 JS-MODE
;; Встроенный пакет.
;; Базовые настройки при работе с JavaScript.
(require 'js)
(custom-set-variables
 '(js-indent-level 2 "Отступ в 2 пробела, а не 4 (по умолчанию).")
 '(js-chain-indent t "Выравнивание при цепочке вызовов через точку."))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))


;; 📦 MAKEFILE
;; Встроенный пакет.
;; Поддержка Makefile.
(require 'make-mode)
(add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-gmake-mode))


;; 📦 NEW-COMMENT
;; Встроенный пакет.
;; Работа с комментариями.
(require 'newcomment)
(keymap-global-set "M-'" 'comment-or-uncomment-region)


;; 📦 NXML-MODE
;; Встроенный пакет.
;; Почти как `xml-mode', только лучше и новее (ну вы поняли...)
(require 'nxml-mode)
(custom-set-variables
 '(nxml-attribute-indent 4 "Выравнивание атрибутов")
 '(nxml-auto-insert-xml-declaration-flag nil "Не вставлять декларацию")
 '(nxml-bind-meta-tab-to-complete-flag t "Использовать TAB для завершения ввода")
 '(nxml-child-indent 4 "Выравнивание дочерних элементов")
 '(nxml-slash-auto-complete-flag t "Закрывать теги по вводу /"))
(add-to-list 'auto-mode-alist '("\\.pom\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))


;; 📦 PACKAGE
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(customize-set-variable 'package-archive-priorities
                        '(("gnu" . 40)
                          ("nongnu" . 30)
                          ("melpa-stable" . 20)
                          ("melpa" . 10)))


;; 📦 PAREN
;; Встроенный режим
;; Управление парными скобками.
(require 'paren)
(show-paren-mode 1) ;; Подсвечивать парные скобки


;; 📦 REPLACE
;; Встроенный пакет.
;; Функции поиска и замены текста.
(require 'replace)
(keymap-global-set "<f3>" 'replace-string)
(keymap-global-set "<f4>" 'replace-regexp)


;; 📦 SAVEPLACE
;; Встроенный пакет.
;; Запоминание позиции курсора в посещённых файлах.
(require 'saveplace)
(custom-set-variables
 '(save-place-forget-unreadable-files t "Не запоминать положение в нечитаемых файлах.")
 '(save-place-file '(expand-file-name ".emacs-places" user-emacs-directory)))
(save-place-mode 1)


;; 📦 RST-MODE
;; Встроенный пакет.
;; Основной режим для редактирования reStructutedText
;; https://www.writethedocs.org/guide/writing/reStructuredText/
(require 'rst)
(custom-set-variables
 '(rst-default-indent 3)
 '(rst-indent-comment 3)
 '(rst-indent-field 3)
 '(rst-indent-literal-minimized 3)
 '(rst-indent-width 3)
 '(rst-toc-indent 3))
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . rst-mode))


;; 📦 SAVE-HIST
;; Встроенный пакет.
;; Запоминает историю введенных команд
(require 'savehist)
(savehist-mode 1)


;; 📦 SHELL-SCRIPT-MODE
;; Встроенный пакет.
;; Работа со скриптами Shell.
(require 'sh-script)
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.profile\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))


;; 📦 SIMPLE
;; Встроенный пакет.
;; Разные настройки управления элементарным редактированием текста.
(require 'simple)
(custom-set-variables
 '(backward-delete-char-untabify-method 'hungry "Удалять все символы выравнивания при нажатии [Backspace]")
 '(blink-matching-paren t "Мигать, когда скобки парные")
 '(blink-matching-paren-on-screen t " TODO: ???")
 '(suggest-key-bindings t "Показывать подсказку клавиатурной комбинации для команды"))
(column-number-mode 1)      ;; Показывать номер колонки в статусной строке
(global-visual-line-mode 1) ;; Деление логических строк на видимые
(line-number-mode t)        ;; Показывать номер строки в статусной строке
(overwrite-mode -1)        ;; Отключить режим перезаписи текста
(size-indication-mode nil)  ;; Отображать размер буфера в строке статуса
(keymap-global-set "C-z" 'undo)               ;; Отмена
(keymap-global-set "S-<SPC>" 'just-one-space) ;; Заменить пробелы и TAB'ы до и после курсора на один пробел


;; 📦 SORT
;; Встроенный пакет.
(require 'sort)
(keymap-global-set "<f9>" 'sort-lines)


;; 📦 TOOLBAR
;; Встроенный пакет, недоступный в Emacs NOX
(when (fboundp 'tool-bar-mode)
  (require 'tool-bar)
  (customize-set-variable 'tool-bar-mode nil))


;; 📦 TOOLTIP
;; Встроенный пакет.
;; Вывод подсказок в графической среде.
(when (fboundp 'tooltip-mode)
  (require 'tooltip)
  (customize-set-variable 'tooltip-mode nil "Отключить показ подсказок с помощью GUI")
  (tooltip-mode -1))


;; 📦 UNIQUIFY
;; Встроенный пакет.
;; Используется для поддержания уникальности названий буферов, путей и т. д.
(require 'uniquify)
(custom-set-variables
 '(uniquify-buffer-name-style 'forward "Показывать каталог перед именем файла, если буферы одинаковые (по умолчанию имя<каталог>)")
 '(uniquify-separator "/" "Разделять буферы с похожими именами, используя /"))


;; 📦 WHITESPACE MODE
;; Встроенный пакет.
;; Отображение невидимых символов.
(require 'whitespace)
(custom-set-variables
 '(whitespace-display-mappings ;; Отображение нечитаемых символов
   '((space-mark   ?\    [?\xB7]     [?.])        ;; Пробел
     (space-mark   ?\xA0 [?\xA4]     [?_])        ;; Неразрывный пробел
     (newline-mark ?\n   [?¶ ?\n]    [?$ ?\n])    ;; Конец строки
     (tab-mark     ?\t   [?\xBB ?\t] [?\\ ?\t]))) ;; TAB
 '(whitespace-line-column 1000 "По умолчанию подсвечиваются длинные строки. Не надо этого делать."))
(add-hook 'adoc-mode-hook 'whitespace-mode)
(add-hook 'conf-mode-hook 'whitespace-mode)
(add-hook 'css-mode-hook 'whitespace-mode)
(add-hook 'dockerfile-mode-hook 'whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'html-mode-hook 'whitespace-mode)
(add-hook 'json-mode-hook 'whitespace-mode)
(add-hook 'latex-mode-hook 'whitespace-mode)
(add-hook 'lisp-data-mode-hook 'whitespace-mode)
(add-hook 'makefile-gmake-mode-hook 'whitespace-mode)
(add-hook 'makefile-mode-hook 'whitespace-mode)
(add-hook 'markdown-mode-hook 'whitespace-mode)
(add-hook 'nxml-mode-hook 'whitespace-mode)
(add-hook 'org-mode-hook 'whitespace-mode)
(add-hook 'po-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'rst-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'sh-mode-hook 'whitespace-mode)
(add-hook 'sql-mode-hook 'whitespace-mode)
(add-hook 'terraform-mode-hook 'whitespace-mode)
(add-hook 'tex-mode-hook 'whitespace-mode)
(add-hook 'web-mode-hook 'whitespace-mode)
(add-hook 'yaml-mode-hook 'whitespace-mode)


;; 📦 WINDMOVE
;; Встроенный пакет.
;; Перемещение между окнами Emacs.
(require 'windmove)
(keymap-global-set "C-x <up>" 'windmove-up)
(keymap-global-set "C-x <down>" 'windmove-down)


;; 📦 WINNER-MODE
;; Встроенный пакет.
;; Управление окнами.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Convenience.html
;; Для управления конфигурациями окон используются последовательности
;; [C-c <left>] и [C-c <right>]
(require 'winner)
(winner-mode 1)


;; 📦 WINDOW
;; Встроенный пакет, отвечает за управление размерами окон
(require 'window)
(keymap-global-set "C-S-<left>" 'shrink-window-horizontally)   ;; [Ctrl+Shift+←]   Уменьшить размер окна по ширине
(keymap-global-set "C-S-<right>" 'enlarge-window-horizontally) ;; [Ctrl+Shift+→]   Увеличить размер окна по ширине
(keymap-global-set "C-S-<down>" 'enlarge-window)               ;; [Ctrl+Shift+↓]   Увеличить размер окна по ширине
(keymap-global-set "C-S-<up>" 'shrink-window)                  ;; [Ctrl+Shift+↑]   Уменьшить размер окна по высоте
(keymap-global-set "C-S-<iso-lefttab>" 'next-buffer)           ;; [Ctrl+Tab]       Вернуться в предыдущий буфер
(keymap-global-set "C-<tab>" 'previous-buffer)                 ;; [Ctrl+Shift+Tab] Следующий буфер

(provide 'base.el)

;;; base.el ends here
