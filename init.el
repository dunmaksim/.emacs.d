;;; init.el --- Dunaevsky Maxim GNU EMACS config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Настройки EMACS для работы техническим писателем.

;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p) ;; Использовать y и n вместо yes и no (сокращает объём вводимого текста для подтверждения команд)

(defun emacs-version-not-less-than (major minor)
  "True when Emacs version is not less than MAJOR MINOR version."
  (or
   (> emacs-major-version major)
   (and (= emacs-major-version major)
        (>= emacs-minor-version minor))))

(defconst init-el-font-height 16 "Размер шрифта по умолчанию.")
(defvar init-el-theme 'misterioso "Тема по умолчанию.")

(require 'custom)
(customize-set-variable
 'custom-file
 (expand-file-name
  (convert-standard-filename "custom.el")
  user-emacs-directory)
 "Файл для сохранения пользовательских настроек, сделанных в customize.")

(require 'derived) ;; derived-mode-hook-name

;;; Здесь находятся настройки базовой функциональности Emacs.
;;; Даже если будут какие-то проблемы со сторонними пакетами, этот код всё
;;; равно будет выполнен.
;;; По этой же причине здесь нет ничего, что могло бы сломаться.

;; Настройки, специфичные для графического режима
(defun setup-gui-settings (frame-name)
  "Настройки, необходимые при запуске EMACS в графической среде.

  FRAME-NAME — имя фрейма, который настраивается."
  (when (display-graphic-p frame-name)
    (global-font-lock-mode t)  ;; Отображать шрифты красиво, используя Font Face's

    (defvar availiable-fonts (font-family-list)) ;; Какие есть семейства шрифтов?
    (defvar default-font-family nil "Шрифт по умолчанию.")

    ;; Перебор шрифтов11
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

(setup-gui-settings (selected-frame))

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
 '(save-place-forget-unreadable-files t "Если файл нельзя открыть, то и помнить о нём ничего не надо")
 '(scroll-bar-mode nil "Отключить полосы прокрутки")
 ;; '(scroll-conservatively 101 "TODO: проверить что это такое")
 '(scroll-margin 4 "Отступ от верхней и нижней границ буфера")
 '(show-trailing-whitespace t "Подсветка висячих пробелов")
 '(standard-indent 4 "Отступ по умолчанию")
 '(tab-always-indent 'complete "Если можно — выровнять текст, иначе — автодополнение.")
 '(truncate-lines 1 "Обрезать длинные строки")
 '(use-dialog-box nil "Диалоговые окна ОС не нужны")
 '(use-short-answers t "Краткие ответы вместо длинных")
 '(user-full-name "Dunaevsky Maxim" "Имя пользователя")
 '(user-mail-address "dunmaksim@yandex.ru" "Адрес электронной почты")
 '(vc-follow-symlinks t "Переходить по ссылкам без лишних вопросов")
 '(visible-bell t "Мигать буфером при переходе в него"))

(when (fboundp 'menu-bar-mode)
  (customize-set-variable 'menu-bar-mode nil "Выключить отображение меню"))

(when (fboundp 'tool-bar-mode)
  (customize-set-variable 'tool-bar-mode nil "Выключить отображение панели инструментов"))

(require 'keymap)


(keymap-global-unset "<insert>")  ;; Режим перезаписи не нужен
(keymap-global-unset "M-,")       ;; Такие маркеры не нужны
(keymap-global-unset "C-z")       ;; Такой Ctrl+Z нам не нужен
(keymap-global-unset "C-x C-z")   ;; `suspend-emacs' тоже не нужен
(keymap-global-unset "C-x C-p")   ;; `mark-page' не нужна, часто конфликтует с Projectile

;; Включим переключение буферов по Ctrl+PgUp и Ctrl+PgDn
(keymap-global-unset "C-<next>")  ;; Ни разу не видел, что это было нужно
(keymap-global-unset "C-<prior>") ;; Это сочетание тоже не нужно.
(keymap-global-set "C-<next>" 'next-buffer)
(keymap-global-set "C-<prior>" 'previous-buffer)

;; Закрыть буфер по нажатию [C-x k]
(keymap-global-set "C-x k" (lambda() (interactive) (kill-buffer (current-buffer))))

;; Вставка длинного тире по нажатию [M--]
(keymap-global-set "M--" (lambda() (interactive) (insert "—")))


;; 📦 ABBREV-MODE
;; Встроенный пакет.
;; Использование аббревиатур -- фрагментов текста, которые при вводе
;; определённой последовательности символов заменяются на другую,
;; например:
;; tf → Terraform
;; yc → Yandex Cloud
;; Это встроенный пакет
(require 'abbrev)
(add-hook 'asciidoc-mode-hook 'abbrev-mode)
(add-hook 'markdown-mode-hook 'abbrev-mode)
(add-hook 'rst-mode-hook 'abbrev-mode)


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
(dolist (mode '(("\\.env\\'" . conf-mode)
                ("\\.flake8\\'" . conf-mode)
                ("\\.ini\\'" . conf-mode)
                ("\\.pylintrc\\'" . conf-mode)
                ("\\inventory\\'" . conf-mode)))
  (add-to-list 'auto-mode-alist mode))


;; 📦 CSS-MODE
;; Встроенный пакет.
;; Поддержка CSS.
(require 'css-mode)
(customize-set-variable 'css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))


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
 '(desktop-dirname user-emacs-directory "Каталог для хранения файла .desktop.")
 '(desktop-load-locked-desktop t "Загрузка файла .desktop даже если он заблокирован.")
 '(desktop-restore-frames t "Восстанавливать фреймы.")
 '(desktop-save t "Сохранять список открытых буферов, файлов и т. д. без лишних вопросов."))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(desktop-save-mode 1)
(add-hook 'server-after-make-frame-hook 'desktop-read)


;; 📦 DIRED
;; Встроенный пакет для работы с файлами и каталогами.
;; Клавиши:
;; [+] - создание каталога.
;; [C-x C-f] - создание файла с последующим открытием буфера.
(require 'dired)
(custom-set-variables
 '(dired-free-space 'separate "Информация о занятом и свободном месте в отдельной строке")
 '(dired-garbage-files-regexp
   (concat (regexp-opt
            '(".aux"
              ".bak"
              ".dvi"
              ".log"
              ".orig"
              ".rej"
              ".toc"
              ".~undo-tree~")) ;; Добавил файлы UNDO-TREE в список мусора
           "\\'"))
 '(dired-kill-when-opening-new-dired-buffer t "Удалять буфер при переходе в другой каталог")
 '(dired-listing-switches "-l --human-readable --all --group-directories-first")
 '(dired-recursive-deletes 'always "Не задавать лишних вопросов при удалении не-пустых каталогов"))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)


;; 📦 DISPLAY-LINE-NUMBERS-MODE
;; Встроенный пакет
;; Показывает номера строк
(require 'display-line-numbers)
(dolist
    (hook '(adoc-mode
            asciidoc-mode
            c-mode
            conf-mode
            css-mode
            csv-mode
            dockerfile-mode
            emacs-lisp-mode
            html-mode
            js-mode
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
            yaml-mode))
  (add-hook (derived-mode-hook-name hook) #'display-line-numbers-mode))


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
(dolist (pair '((?\( . ?\)) ;; ()
                (?\[ . ?\]) ;; []
                (?{ . ?})   ;; {}
                (?« . ?»)   ;; «»
                (?‘ . ’?)   ;; ‘’
                (?‚ . ‘?)   ;; ‚‘
                (?“ . ”?))) ;; “”))
  (add-to-list 'electric-pair-pairs pair))
(dolist (hook '(adoc-mode
                conf-mode
                emacs-lisp-data-mode
                emacs-lisp-mode
                lisp-data-mode
                markdown-mode
                python-mode
                ruby-mode))
  (add-hook (derived-mode-hook-name hook) #'electric-pair-local-mode))


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
 '(enable-local-variables :all "Считать все переменные из файлов `.dir-locals.el' безопасными")
 '(large-file-warning-threshold (* 100 1024 1024) "Предупреждение при открытии файлов больше 100 МБ (по умолчанию — 10 МБ)")
 '(make-backup-files nil "Резервные копии не нужны, у нас есть undo-tree")
 '(require-final-newline t "Требовать новую строку в конце файлов")
 '(save-abbrevs 'silently "Сохранять аббревиатуры без лишних вопросов"))
(dolist (safe-var '((buffer-env-script-name . ".venv/bin/activate")
                    (electric-pair-preserve-balance . t)
                    (emacs-lisp-docstring-fill-column . 80)
                    (fill-column . 120)
                    (fill-column . 80)
                    (frozen_string_literal . true)
                    (lexical-binding . t)))
  (add-to-list 'safe-local-variable-values safe-var))


;; 📦 FILL-COLUMN
;; Встроенный пакет.
;; Отображение рекомендуемой границы символов.
(require 'display-fill-column-indicator)
(dolist (mode-name '(emacs-lisp-mode
                     yaml-mode))
  (add-hook (derived-mode-hook-name mode-name) 'display-fill-column-indicator-mode))


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
        (dolist (hook '(adoc-mode
                        asciidoc-mode
                        emacs-lisp-mode
                        markdown-mode
                        org-mode
                        rst-mode
                        text-mode))
          (add-hook (derived-mode-hook-name hook) 'flyspell-mode)))
    ;; Не найдено программ для проверки орфографии
    (message "Не найдено программ для проверки орфографии.")))


;; 📦 FRAME
;; Встроенный пакет.
;; Управление фреймами.
(require 'frame)
(custom-set-variables
 '(window-divider-default-places 't "Разделители окон со всех сторон (по умолчанию только справа)")
 '(window-divider-default-right-width 3  "Ширина в пикселях для линии-разделителя окон"))
(keymap-global-set "C-x O" 'previous-multiframe-window) ;; Перейти в предыдущее окно
(keymap-global-set "C-x o" 'next-multiframe-window)     ;; Перейти в следующее окно


;; 📦 GOTO-ADDRESS-MODE
;; Встроенный пакет.
;; Подсвечивает ссылки и позволяет переходить по ним с помощью [C-c RET].
;; Возможны варианты (зависит от основного режима).
(require 'goto-addr)
(dolist (hook '(adoc-mode
                emacs-lisp-mode
                html-mode
                markdown-mode
                rst-mode
                web-mode))
  (add-hook (derived-mode-hook-name hook) 'goto-address-mode))


;; 📦 GREP
;; Встроенный пакет.
;; Поиск с помощью `grep'.
(require 'grep)
(keymap-global-set "<f6>" 'find-grep) ;; Запуск `find-grep' по нажатию [F6].


;; 📦 IBUFFER
;; Встроенный пакет для удобной работы с буферами.
;; По нажатию F2 выводит список открытых буферов.
;; Взято из конфига автора пакета
;; https://github.com/jwiegley/dot-emacs/blob/master/init.org
(require 'ibuffer)
(custom-set-variables
 '(ibuffer-formats ;; Форматирование вывода
   ;; Полный формат
   '((mark      ;; Отметка
      modified  ;; Буфер изменён?
      read-only ;; Только чтение?
      locked    ;; Заблокирован?
      " "
      (name 30 40 :left :elide) ;; Имя буфера: от 30 до 40 знаков
      " "
      (mode 8 -1 :left)         ;; Активный режим: от 8 знаков по умолчанию, при необходимости увеличить
      " "
      filename-and-process)     ;; Имя файла и процесс
     ;; Сокращённый формат
     (mark      ;; Отметка?
      " "
      (name 32 -1) ;; Имя буфера: 32 знака, при неоходимости — расширить на сколько нужно
      " "
      filename)))  ;; Имя файла)
 '(ibuffer-default-sorting-mode 'filename/process "Сортировать файлы по имени / процессу")
 '(ibuffer-expert 1 "Не запрашивать подтверждение для опасных операций")
 '(ibuffer-shrink-to-minimum-size t "Минимальный размер буфера по умолчанию")
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
      ("AsciiDoc" (or
                   (mode . adoc-mode)
                   (mode . asciidoc-mode)))
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
        ;; (mode . anaconda-mode)
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
;; Почти как `xml-mode', только лучше и новее (ну вы поняли…)
(require 'nxml-mode)
(custom-set-variables
 '(nxml-attribute-indent 4 "Выравнивание атрибутов")
 '(nxml-auto-insert-xml-declaration-flag nil "Не вставлять декларацию")
 '(nxml-bind-meta-tab-to-complete-flag t "Использовать TAB для завершения ввода")
 '(nxml-child-indent 4 "Выравнивание дочерних элементов")
 '(nxml-slash-auto-complete-flag t "Закрывать теги по вводу /"))
(add-to-list 'auto-mode-alist '("\\.pom\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))


;; 📦 PAREN
;; Встроенный режим
;; Управление парными скобками.
(require 'paren)
(show-paren-mode 1) ;; Подсвечивать парные скобки


;; 📦 PIXEL-SCROLL
;; Встроенный пакет
;; Позволяет плавно прокручивать текст
(when (package-installed-p 'pixel-scroll)
  (require 'pixel-scroll)
  (pixel-scroll-mode 1)
  (pixel-scroll-precision-mode))


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
(customize-set-variable 'save-place-forget-unreadable-files t "Не запоминать положение в нечитаемых файлах.")
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
(dolist (mode '(("\\.bashrc\\'" . shell-script-mode)
                ("\\.envrc\\'" . shell-script-mode)
                ("\\.profile\\'" . shell-script-mode)
                ("\\.sh\\'" . shell-script-mode)))
  (add-to-list 'auto-mode-alist mode))


;; 📦 SIMPLE
;; Встроенный пакет.
;; Разные настройки управления элементарным редактированием текста.
(require 'simple)
(custom-set-variables
 '(backward-delete-char-untabify-method 'hungry "Удалять все символы выравнивания при нажатии [Backspace]")
 '(blink-matching-paren t "Мигать, когда скобки парные")
 '(suggest-key-bindings t "Показывать подсказку клавиатурной комбинации для команды"))
(column-number-mode 1)      ;; Показывать номер колонки в статусной строке
(global-visual-line-mode 1) ;; Деление логических строк на видимые
(line-number-mode t)        ;; Показывать номер строки в статусной строке
(overwrite-mode -1)         ;; Отключить режим перезаписи текста
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
(dolist (hook '(asciidoc-mode
                adoc-mode
                conf-mode
                css-mode
                dockerfile-mode
                emacs-lisp-mode
                html-mode
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
                snippet-mode ;; Yasnippet
                sql-mode
                terraform-mode
                tex-mode
                web-mode
                yaml-mode))
  (add-hook (derived-mode-hook-name hook) 'whitespace-mode))


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

;;;;;; Здесь заканчиваются настройки встроенных пакетов и начинаются
;;;;;; настройки пакетов, полученных от чертей из интернета.

;; 📦 PACKAGE
(require 'package)
(customize-set-variable 'package-enable-at-startup nil "Prevent double loading of libraries")
(dolist (archive '(("gnu" . "https://elpa.gnu.org/packages/")
                   ("melpa" . "https://melpa.org/packages/")
                   ("melpa-stable" . "https://stable.melpa.org/packages/")
                   ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (add-to-list 'package-archives archive t))
(package-initialize)

(customize-set-variable
 'package-archive-priorities
 '(("gnu" . 40)
   ("nongnu" . 30)
   ("melpa-stable" . 20)
   ("melpa" . 10)))

(unless package-archive-contents
  (message "Обновление списка архивов...")
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (progn
    (message "Пакет `use-package' не установлен.")
    (message "Установка `use-package'...")
    (package-install 'use-package t)))

(when (package-built-in-p 'use-package (version-to-list "2.4.6"))
  (message "Пакет `use-package' встроенный и имеет версию ниже 2.4.6.")
  ;; Сейчас это не работает, потому что в самом пакете `use-package'
  ;; забыли повысить номер версии, и она в 2.4.6 всё ещё 2.4.5.
  (message "Установка новой версии `use-package` из GNU ELPA...")
  (customize-set-variable 'package-install-upgrade-built-in t)
  (package-refresh-contents)
  (package-install 'use-package t)
  (customize-set-variable 'package-install-upgrade-built-in nil))

(require 'use-package)

;; Настройки отладочного режима
(when init-file-debug
  (custom-set-variables
   '(debug-on-error t "Автоматически перейти в режим отладки при ошибках.")
   '(use-package-compute-statistics t "Сбор статистики `use-package'.")
   '(use-package-expand-minimally t "Минимальное раскрытие кода.")
   '(use-package-verbose t "Подробный режим работы `use-package'.")))


;; 📦 DELIGHT
;; https://elpa.gnu.org/packages/delight.html
;; Позволяет спрятать из панели статуса лишние названия режимов.
;; Эти строки находятся здесь потому, что `use-package' активно
;; использует возможности этого пакета далее, поэтому он должен быть
;; загружен как можно раньше.
(unless (package-installed-p 'delight)
  (package-vc-install '(delight
                        :url "https://git.savannah.nongnu.org/git/delight.git"
                        :branch "1.7")))
(use-package delight
  :ensure t
  :config
  (delight '((checkdoc-minor-mode)
             (global-visual-line-mode)
             (global-whitespace-mode))))

(use-package emacs
  :delight
  (whitespace-mode " ¶"))

;; 📦 ACE-WINDOW
;; https://github.com/abo-abo/ace-window
;; Быстрое переключение между окнами по M+o
(unless (package-installed-p 'ace-window)
  (package-vc-install '(ace-window
                        :url "https://github.com/abo-abo/ace-window.git"
                        :branch "0.10.0")))
(use-package ace-window
  :ensure t
  :bind (:map global-map
              ("M-o" . ace-window)))


;; 📦 ACTIVITIES
;; https://elpa.gnu.org/packages/activities.html
;; Управление наборами окон, вкладок, фреймов и буферов
(unless (package-installed-p 'activities)
  (package-vc-install '(activities
                        :url "https://github.com/alphapapa/activities.el.git"
                        :branch "v0.7.2")))
(use-package activities
  :ensure t
  :config
  (activities-mode 1)
  :bind
  (
   ("C-x C-a C-n" . activities-new)
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
  :ensure t
  :hook (emacs-lisp-mode . adjust-parens-mode)
  :bind (:map emacs-lisp-mode-map
              ("<tab>" . lisp-indent-adjust-parens)
              ("<backtab>" . lisp-dedent-adjust-parens)))


;; 📦 ADOC-MODE
;; https://github.com/bbatsov/adoc-mode
;; Работа с AsciiDoc
;; (use-package adoc-mode
;;   :ensure t
;;   :defer t
;;   :custom
;;   (adoc-fontify-code-blocks-natively 10000)
;;   :mode "\\.adoc\\'")

(use-package asciidoc-mode
  :load-path "~/repo/asciidoc-mode/"
  :mode ("\\.adoc\\'" . asciidoc-mode))


;; 📦 AGGRESSIVE-INDENT
;; https://github.com/Malabarba/aggressive-indent-mode
;; Принудительное выравнивание кода
(unless (package-installed-p 'aggressive-indent)
  (package-vc-install '(aggressive-indent
                        :url "https://github.com/Malabarba/aggressive-indent-mode.git"
                        :branch "1.10.0")))
(use-package aggressive-indent
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode
          json-mode
          latex-mode
          lisp-data-mode
          nxml-mode
          sh-mode
          sql-mode
          ) . aggressive-indent-mode))


;; 📦 ALL
;; https://elpa.gnu.org/packages/all.html
;; Это аналог `occur', только все найденные строки помещаются в отдельный буфер,
;; где их можно отредактировать, не прыгая по всему буферу. После изменений
;; достаточно нажать C-c C-c, и изменения отразятся в основном буфере
(use-package all
  :ensure t)


;; 📦 ANSIBLE
;; https://gitlab.com/emacs-ansible/emacs-ansible
;; Дополнительные возможности при работе с YAML-файлами Ansible
(unless (package-installed-p 'ansible)
  (package-vc-install '(ansible
                        :url "https://gitlab.com/emacs-ansible/emacs-ansible.git"
                        :branch "0.3.2")))
(use-package ansible
  :ensure t
  :defer t)


;; 📦 APHELEIA
;; https://github.com/radian-software/apheleia
;; Форматирование содержимого буфера с помощью внешних средств
(unless (package-installed-p 'apheleia)
  (package-vc-install '(apheleia
                        :url "https://github.com/radian-software/apheleia.git"
                        :branch "v4.2")))
(use-package apheleia
  :ensure t
  :delight "")


;; 📦 AVY
;; https://github.com/abo-abo/avy
;; Множество функций для быстрого перехода к нужной строке, слову, символу и
;; так далее.
(unless (package-installed-p 'avy)
  (package-vc-install '(avy
                        :url "https://github.com/abo-abo/avy.git"
                        :branch "0.5.0")))
(use-package avy
  :ensure t
  :delight ""
  :bind (:map global-map
              ("M-g f" . avy-goto-line)
              ("M-g w" . avy-goto-word)
              ("C-:" . avy-goto-char)))


;; 📦 BBCODE-MODE
;; https://github.com/lassik/emacs-bbcode-mode
;; Режим редактирования BB-кодов
(unless (package-installed-p 'bbcode-mode)
  (package-vc-install '(bbcode-mode
                        :url "https://github.com/lassik/emacs-bbcode-mode.git"
                        :branch "v2.3.0")))
(use-package bbcode-mode
  :ensure t
  :defer t)


;; 📦 BREADCRUMB
;; https://elpa.gnu.org/packages/breadcrumb.html
;; Упрощает навигацию по сложным документам: показывает хлебные
;; крошки в заголовках окон и позволяет быстро перейти в нужное место
;; с помощью `breadcrumb-jump'.
;; (use-package breadcrumb
;;   :ensure t
;;   :hook ((emacs-lisp-mode
;;           rst-mode) . breadcrumb-local-mode))


;; 📦 BUFFER-ENV
;; https://github.com/astoff/buffer-env
;; Настройка окружения отдельно для каждого буфера.
;; Настройки загружаются из файла `.env' в каталоге проекта или `.dir-locals.el'.
;; Во первом случае в файле должна быть указана команда для активации окружения, например:
;; source .venv/bin/activate
;; Во втором достаточно задать значение переменной `buffer-env-script-name'.
;; (when (emacs-version-not-less-than 27 1)
;;   (use-package buffer-env
;;     :ensure t
;;     :hook ((hack-local-variables
;;             comint-mode
;;             ) . buffer-env-update)))


;; 📦 COLORFUL-MODE
;; https://github.com/DevelopmentCool2449/colorful-mode
;; Отображение цветов прямо в буфере. Наследник `raibow-mode.el'.
(unless (package-installed-p 'colorful-mode)
  (package-vc-install '(colorful-mode
                        :url "https://github.com/DevelopmentCool2449/colorful-mode.git"
                        :branch "v1.0.4")))
(use-package colorful-mode
  :ensure t
  :hook ((css-mode
          emacs-lisp-mode
          web-mode) . colorful-mode))


;; 📦 COMPANY-MODE
;; https://company-mode.github.io/
;; Автодополнение
(unless (package-installed-p 'company)
  (package-vc-install '(company
                        :url "https://github.com/company-mode/company-mode.git"
                        :branch "1.0.2")))
(use-package company
  :ensure t
  :delight ""
  :demand t
  :custom
  (company-idle-delay 0.5 "Задержка вывода подсказки — полсекунды")
  (company-minimum-prefix-length 2 "Минимум 2 знака, чтобы company начала работать")
  (company-show-quick-access t "Показывать номера возле потенциальных кандидатов")
  (company-tooltip-align-annotations t "Выровнять текст подсказки по правому краю")
  (company-tooltip-limit 15 "Ограничение на число подсказок")
  :hook ((adoc-mode
          css-mode
          dockerfile-mode
          emacs-lisp-mode
          html-mode
          latex-mode
          lisp-data-mode
          minibufer-mode
          nxml-mode
          org-mode
          python-mode
          rst-mode
          ruby-mode
          web-mode
          ) . company-mode)
  :bind
  (:map company-active-map
        ("TAB" . company-complete-common-or-cycle)
        ("M-/" . company-complete)
        ("M-." . company-show-location)))


;; 📦 COUNSEL
;; https://elpa.gnu.org/packages/counsel.html
;; Автодополнение на основе Ivy
(use-package counsel
  :ensure t
  :bind
  (:map global-map
        ("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("M-y" . counsel-yank-pop)
        ("C-h f" . counsel-describe-function)
        ("C-h v" . counsel-describe-variable)
        ("C-h l" . counsel-find-library)
        ("C-c c" . counsel-compile)
        ("C-c g" . counsel-git)
        ("C-x 8 RET" . counsel-unicode-char)))


;; 📦 CSV-MODE
;; https://elpa.gnu.org/packages/csv-mode.html
;; Поддержка CSV
(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")


;; 📦 DENOTE
;; https://protesilaos.com/emacs/denote
;; Режим для управления заметками
(when (emacs-version-not-less-than 28 1)
  (unless (package-installed-p 'denote)
    (package-vc-install '(denote
                          :url "https://github.com/protesilaos/denote.git"
                          :branch "3.1.0")))
  (use-package denote
    :ensure t
    :custom
    (denote-directory "~/Документы/Notes/" "Каталог для хранения заметок.")))


;; 📦 DIRENV
;; https://github.com/wbolster/emacs-direnv
;; Поддержка `direnv' в Emacs. Для корректной работы нужно выполнить несколько
;; дополнительных действий:
;; 1. Установить в систему direnv
;; 2. Для поддержки Ruby установить ruby-install: https://direnv.net/docs/ruby.html
;; 3. Создать в каталоге ~/.config/direnv/ файл direnvrc:
;;
;; # Usage: use ruby <version>
;; #
;; # Loads the specified ruby version into the environment
;; #
;; use_ruby() {
;;   local ruby_dir=$HOME/.rubies/$1
;;   load_prefix $ruby_dir
;;   layout ruby
;; }
;; 4. Создать в каталоге проекта файл `.envrc':
;;
;; source .venv/bin/activate
;; use ruby 3.3.5
;;
;; 5. Разрешить использование этого файла:
;;
;; cd  ~/<project>
;; direnv allow
;; (use-package direnv
;;   :ensure t
;;   :config
;;   (direnv-mode))


;; 📦 DOCKERFILE-MODE
;; https://github.com/spotify/dockerfile-mode
;; Работа с файлами `Dockerfile'.
(unless (package-installed-p 'dockerfile-mode)
  (package-vc-install '(dockerfile-mode
                        :url "https://github.com/spotify/dockerfile-mode.git"
                        :branch "v1.9")))
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "\\Dockerfile\\'")


;; 📦 DOOM-THEMES
;; https://github.com/doomemacs/themes
;; Темы из DOOM Emacs
;; (use-package doom-themes
;;   :ensure t
;;   :custom
;;   (doom-themes-enable-bold t "Включить поддержку полужирного начертания.")
;;   (doom-themes-enable-italic t "Включить поддержку наклонного начертания."))
(unless (package-installed-p 'doom-themes)
  (package-vc-install '(doom-themes
                        :url "https://github.com/doomemacs/themes.git"
                        :branch "v2.3.0")))
(use-package doom-themes
  :ensure t)


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
(unless (package-installed-p 'edit-indirect)
  (package-vc-install '(edit-indirect
                        :url "https://github.com/Fanael/edit-indirect.git"
                        :branch "0.1.13")))
(use-package edit-indirect
  :ensure t
  :bind (:map global-map
              ("C-c '" . edit-indirect-region)))


;; 📦 EDITORCONFIG
;; Поддержка https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(unless (package-installed-p 'editorconfig)
  (package-vc-install '(editorconfig
                        :url "https://github.com/editorconfig/editorconfig-emacs.git"
                        :branch "v0.11.0"
                        :doc "doc")))
(use-package editorconfig
  :ensure t
  :delight ""
  :config
  (editorconfig-mode 1)
  (add-hook 'before-save-hook 'editorconfig-format-buffer)
  :mode
  ("\\.editorconfig\\'" . editorconfig-conf-mode))


;; 📦 EF-THEMES
;; https://github.com/protesilaos/ef-themes.git
(unless (package-installed-p 'ef-themes)
  (package-vc-install '(ef-themes
                        :url "https://github.com/protesilaos/ef-themes.git"
                        :branch "1.9.0")))
(use-package ef-themes
  :ensure t
  :config
  (setq init-el-theme 'ef-autumn))


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
(when (emacs-version-not-less-than 26 3)
  (unless (package-installed-p 'eglot)
    (package-vc-install '(eglot
                          :url "https://github.com/joaotavora/eglot.git"
                          :branch "1.17")))
  (use-package eglot
    :ensure t
    :defer t
    :config
    (add-to-list 'eglot-server-programs '(ansible-mode . ("ansible-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs '(dockerfile-mode . ("docker-langserver" "--stdio")))
    (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
    (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
    (add-to-list 'eglot-server-programs '(ruby-mode . ("bundle" "exec" "rubocop" "--lsp")))
    (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
    :bind (:map eglot-mode-map
                ("C-c C-d" . eldoc)
                ("C-c C-r" . eglot-rename)
                ("C-c C-f" . eglot-format-buffer))
    :hook ((ansible-mode
            dockerfile-mode
            markdown-mode
            python-mode
            rst-mode
            ruby-mode
            yaml-mode
            ) . eglot-ensure)))


;; 📦 EL-PATCH
;; https://github.com/radian-software/el-patch
;; Зависимость Magit
(unless (package-installed-p 'el-patch)
  (package-vc-install '(el-patch
                        :url "https://github.com/radian-software/el-patch.git"
                        :branch "3.1")))


;; 📦 ELDOC-MODE
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Programming-Language-Doc.html
;; Отображение подсказок при работе с Emacs Lisp
(use-package eldoc
  :config
  (global-eldoc-mode nil) ;; Глобально этот режим не нужен
  :delight ""             ;; Выводить в статус-баре тоже не нужно
  :hook ((emacs-lisp-mode ;; Включаем только там, где это действительно необходимо
          python-mode) . eldoc-mode))


;; 📦 ENVRC
;; https://github.com/purcell/envrc
;; Загрузка переменных окружения из `.envrc'.
(unless (package-installed-p 'envrc)
  (package-vc-install '(envrc
                        :url "https://github.com/purcell/envrc.git"
                        :branch "0.12")))
(use-package envrc
  :hook (after-init . envrc-global-mode))


;; 📦 FLYCHECK
;; https://www.flycheck.org/
;; Проверка синтаксиса на лету с помощью статических анализаторов
(unless (package-installed-p 'flycheck)
  (package-vc-install '(flycheck
                        :url "https://github.com/flycheck/flycheck.git"
                        :branch "34.1"
                        :doc "doc")))
(use-package flycheck
  :ensure t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save new-line))
  (flycheck-highlighting-mode 'lines "Стиль отображения проблемных мест — вся строка")
  (flycheck-indication-mode 'left-fringe "Место размещения маркера ошибки — левая граница")
  (flycheck-locate-config-file-functions '(flycheck-locate-config-file-by-path
                                           flycheck-locate-config-file-ancestor-directories
                                           flycheck-locate-config-file-home))
  (flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc" "Файл настроек Markdownlint")
  (flycheck-sphinx-warn-on-missing-references t "Предупреждать о некорректных ссылках в Sphinx")
  (flycheck-textlint-config ".textlintrc.yaml" "Файл настроек Textlint")
  :hook ((adoc-mode
          asciidoc-mode
          conf-mode
          css-mode
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
          sql-mode
          terraform-mode
          web-mode
          yaml-mode
          ) . flycheck-mode))


;; 📦 FLYLISP
;; https://elpa.gnu.org/packages/flylisp.html
;; Подсвекта непарных или неправильно выровненных скобок
(use-package flylisp
  :ensure t
  :hook (emacs-lisp-mode . flylisp-mode))


;; 📦 FORMAT-ALL
;; https://github.com/lassik/emacs-format-all-the-code
;; Форматирование кода с помощью разных внешних средств.
(unless (package-installed-p 'format-all)
  (package-vc-install '(format-all
                        :url "https://github.com/lassik/emacs-format-all-the-code.git"
                        :branch "0.6.0")))
(use-package format-all
  :ensure t
  :defer t
  :bind (:map global-map
              ([f12] . format-all-buffer)))

(hl-line-mode)


;; 📦 HELM
;; https://github.com/emacs-helm/helm
;; Подсказки и автодополнение ввода.
;; [C-o] — переключение между источниками подсказок (история и полный список команд)
(unless (package-installed-p 'helm)
  (package-vc-install '(helm
                        :url "https://github.com/emacs-helm/helm.git"
                        :branch "v4.0")))
(use-package helm
  :ensure t
  :delight ""
  :config
  (helm-mode 1)
  :bind (:map global-map
              ("M-y" . helm-show-kill-ring)))


;; 📦 HL-TODO
;; https://github.com/tarsius/hl-todo
;; Подсветка TODO, FIXME и т. п.
(unless (package-installed-p 'hl-todo)
  (package-vc-install
   '(hl-todo
     :url "https://github.com/tarsius/hl-todo.git"
     branch: "v3.8.1")))
(use-package hl-todo
  :ensure t
  :config (global-hl-todo-mode t))


;; 📦 HYPERBOLE
;; https://www.gnu.org/software/hyperbole/
;; Распознаёт текст в буферах и автоматически превращает в кнопки и ссылки.
(use-package hyperbole
  :ensure t)



;; 📦 INDENT-BARS
;; https://github.com/jdtsmith/indent-bars
;; Красивая подсветка отступов
(unless (package-installed-p 'indent-bars)
  (package-vc-install
   '(indent-bars
     :url "https://github.com/jdtsmith/indent-bars.git"
     :branch "v0.8.2")))
(use-package indent-bars
  :ensure t
  :hook ((emacs-lisp-mode
          makefile-mode
          markdown-mode
          python-mode
          rst-mode
          ruby-mode
          yaml-mode
          ) . indent-bars-mode))


;; 📦 IVY
;; https://elpa.gnu.org/packages/ivy.html
;; https://elpa.gnu.org/packages/doc/ivy.html
;; Функции фильтрации и выбора элементов. Как Helm, но теперь в составе Emacs.
;; При переименовании файлов рекомендуется использовать `ivy-immediate-done',
;; Это последовательность [C-M-j].
(use-package ivy
  :ensure t
  :demand t
  :delight 'ivy-mode
  :config
  (ivy-mode 1)
  :bind
  (:map global-map
        ("C-x b" . ivy-switch-buffer)
        ("C-c v" . ivy-push-view)
        ("C-c V" . ivy-pop-view)))


;; 📦 IVY-HYDRA
;; https://elpa.gnu.org/packages/ivy-hydra.html
;; Дополнительные сочетания клавиш для IVY.
(use-package ivy-hydra
  :ensure t
  :demand t
  :after ivy
  :requires ivy)


;; 📦 JINX
;; https://github.com/minad/jinx
;; Проверка орфографии не только для слова под курсором, как во `flyspell',
;; а вообще во всём буфере.
;; В Debian требует для работы `libenchant2-dev' и `pkgconf'.
(unless (package-installed-p 'jinx)
  (package-vc-install '(jinx
                        :url "https://github.com/minad/jinx.git"
                        :branch "1.10")))
(use-package jinx
  :hook ((adoc-mode
          asciidoc-mode
          markdown-mode
          org-mode
          rst-mode
          text-mode). jinx-mode))


;; 📦 JSON-MODE
;; https://github.com/json-emacs/json-mode
;; Поддержка JSON
(unless (package-installed-p 'json-mode)
  (package-vc-install '(json-mode
                        :url "https://github.com/json-emacs/json-mode.git"
                        :branch "v1.9.2")))
(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json\\'")


;; 📦 DASH
;; https://github.com/magnars/dash.el
;; Зависимость MAGIT
(unless (package-installed-p 'dash)
  (package-vc-install '(dash
                        :url "https://github.com/magnars/dash.el.git"
                        :branch "2.19.1"
                        :doc "doc")))


;; 📦 WITH-EDITOR
;; https://github.com/magit/with-editor
;; Зависимость MAGIT
(unless (package-installed-p 'with-editor)
  (package-vc-install '(with-editor
                         :url "https://github.com/magit/with-editor.git"
                         :branch "v3.4.2"
                         :lisp-dir "lisp"
                         :doc "docs")))


;; 📦 MAGIT
;; https://magit.vc/
;; Magic + Git + Diff-HL.
;; Лучшее средство для работы с Git.
(unless (package-installed-p 'magit)
  (package-vc-install
   '(transient
     :url "https://github.com/magit/transient.git"
     :branch "v0.8.1"
     :lisp-dir "lisp"
     :doc "docs"))
  (package-vc-install
   '(magit
     :url "https://github.com/magit/magit.git"
     :branch "v4.1.1"
     :lisp-dir "lisp"
     :doc "docs")))
(use-package magit
  :ensure t
  :demand t
  :custom
  (magit-auto-revert-mode t "Автоматически обновлять файлы в буферах при изменениях на диске.")
  (magit-define-global-key-bindings t "Включить глобальные сочетания Magit.")
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :hook
  (after-save . magit-after-save-refresh-status))


;; 📦 MAGIT-FILE-ICONS
;; https://github.com/gekoke/magit-file-icons
;; Иконки в буферах Magit
(unless (package-installed-p 'magit-file-icons)
  (package-vc-install '(magit-file-icons
                        :url "https://github.com/gekoke/magit-file-icons.git"
                        :branch "v2.0.0")))
(use-package magit-file-icons
  :config
  (magit-file-icons-mode 1))


;; 📦 DIFF-HL
;; https://github.com/dgutov/diff-hl
;; Показывает небольшие маркеры рядом с незафиксированными изменениями. Дополняет функциональность git-gutter,
;; которые показывает изменения только в обычных буферах. Этот пакет умеет работать с dired и другими режимами.
(unless (package-installed-p 'diff-hl)
  (package-vc-install '(diff-hl
                        :url "https://github.com/dgutov/diff-hl.git"
                        :branch "1.10.0")))
(use-package diff-hl
  :requires magit
  :after magit
  :ensure t
  :commands (diff-hl-mode diff-hl-dired-mode)
  :config
  (global-diff-hl-mode 1)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :hook
  ((adoc-mode
    asciidoc-mode
    emacs-lisp-mode
    markdown-mode
    python-mode
    rst-mode
    yaml-mode). diff-hl-margin-mode)
  ((dired-mode . diff-hl-dired-mode)))


;; 📦 MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
;; Режим для работы с файлами в формате Markdown
(when (emacs-version-not-less-than 27 1)
  (unless (package-installed-p 'markdown-mode)
    (package-vc-install '(markdown-mode
                          :url "https://github.com/jrblevin/markdown-mode.git"
                          branch "v2.6")))
  (use-package markdown-mode
    :ensure t
    :defer t
    :custom
    (markdown-fontify-code-blocks-natively t "Подсвечивать синтаксис в примерах кода")
    (markdown-header-scaling-values '(1.0 1.0 1.0 1.0 1.0 1.0) "Все заголовки одной высоты")
    (markdown-list-indent-width 4 "Размер отступа для выравнивания вложенных списков")
    :config (setq-local word-wrap t)
    :bind (:map markdown-mode-map
                ("M-." . markdown-follow-thing-at-point))
    :mode "\\.md\\'"))


;; 📦 MODUS-THEMES
;; https://www.gnu.org/software/emacs/manual/html_node/modus-themes/index.html
(use-package modus-themes
  :ensure t)


;; 📦 MONOKAI-EMACS
;; https://github.com/oneKelvinSmith/monokai-emacs
;; Тема из редактора Sublime Text
(use-package monokai-theme
  :ensure t)
;; (setq init-el-theme 'monokai)


;; 📦 MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
;; Позволяет использовать мультикурсорность.
(use-package multiple-cursors
  :ensure t
  :custom (mc/always-run-for-all t "Не задавать лишних вопросов.")
  :init
  (keymap-global-unset "M-<down-mouse-1>")
  :bind (:map global-map
              ("C-S-c C-S-c" . mc/edit-lines)
              ("C->" . mc/mark-next-like-this)
              ("C-<" . mc/mark-previous-like-this)
              ("C-c C-<" . mc/mark-all-like-this)
              ("M-<mouse-1>" . mc/add-cursor-on-click)))


;; 📦 NANO-MODELINE
;; https://elpa.gnu.org/packages/nano-modeline.html
;; Статусная строка маленькая вообще жестб
(use-package nano-modeline
  :ensure t
  :custom
  (nano-modeline-position 'nano-modeline-footer "Показывать внизу")
  :hook
  (messages-buffer-mode . nano-modeline-message-mode)
  (org-agenda-mode . nano-modeline-org-agenda-mode)
  (org-capture-mode . nano-modeline-org-capture-mode)
  (org-mode . nano-modeline-org-mode)
  (prog-mode . nano-modeline-prog-mode)
  (term-mode . nano-modeline-term-mode)
  (text-mode . nano-modeline-text-mode))


;; 📦 NERD-ICONS
;; https://github.com/rainstormstudio/nerd-icons.el
;; Требуется для корректной работы `doom-modeline'.
;; Начиная с версии 4.0.0 пакет `all-the-icons' не поддерживается.
;;
;; Для установки самих шрифтов следует использовать команду `nerd-icons-install-fonts'.
;; В Debian Linux шрифты будут загружены в каталог `~/.local/share/fonts'. Рекомендуется
;; скопировать их в `/usr/local/share/fonts/'.
(use-package nerd-icons
  :ensure t
  :delight ""
  :custom
  (nerd-icons-color-icons t "Использовать цветные иконки."))


;; 📦 NERD-ICONS-DIRED
;; https://github.com/rainstormstudio/nerd-icons-dired
;; Иконки в `dired'.
(use-package nerd-icons-dired
  :ensure t
  :delight ""
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))


;; 📦 NERD-ICONS-IBUFFER
;; https://github.com/seagle0128/nerd-icons-ibuffer
;; Отображение иконок в ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :after (ibuffer nerd-icons)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; 📦 ORG-MODE
;; https://orgmode.org/
;; Органайзер, заметки и так далее
(unless (and (package-installed-p 'org)
             (package-built-in-p 'org '(9 7 18)))
  (customize-set-variable 'package-install-upgrade-built-in t)
  (package-install 'org)
  (customize-set-variable 'package-install-upgrade-built-in nil))
(use-package org
  :defer t
  :config
  (setq-local
   truncate-lines nil ;; Не обрезать строки
   word-wrap t))      ;; Перенос длинных строк


;; 📦 PACKAGE-LINT
;; https://github.com/purcell/package-lint
;; Проверка пакетов Emacs
(unless (package-installed-p 'package-lint)
  (package-vc-install '(package-lint
                        :url "https://github.com/purcell/package-lint.git"
                        :branch "0.23")))
(use-package package-lint
  :ensure t
  :defer t)


;; 📦 PHP-MODE
;; https://github.com/emacs-php/php-mode
;; Работа с файлами PHP
(unless (package-installed-p 'php-mode)
  (package-vc-install '(php-mode
                        :url "https://github.com/emacs-php/php-mode.git"
                        :branch "v1.26.1"
                        :lisp-dir "lisp")))
(use-package php-mode
  :ensure t
  :mode "\\.php\\'")


;; 📦 PO-MODE
;; https://www.gnu.org/software/gettext/manual/html_node/Installation.html
;; Работа с файлами локализации.
;; Необходимо установить в систему эти пакеты:
;; * gettext
;; * gettext-el: если po-mode из архивов не работает
(use-package po-mode
  :pin "melpa"
  :ensure t
  :mode "\\.po\\'\\|\\.po\\.")


;; 📦 PROJECTILE
;; https://docs.projectile.mx/projectile/installation.html
;; Управление проектами. Чтобы каталог считался проектом, он должен быть
;; под контролем любой системы версионирования, либо содержать специальные
;; файлы. В крайнем случае сгодится пустой файл .projectile
;; Подробнее здесь: https://docs.projectile.mx/projectile/projects.html
(unless (package-installed-p 'projectile)
  (package-install 'buttercup)
  (package-vc-install '(projectile
                        :url "https://github.com/bbatsov/projectile.git"
                        :branch "v2.8.0"
                        :doc "doc")))
(use-package projectile
  :ensure t
  :delight ""
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :bind
  ([f7] . projectile-compile-project)
  :init
  (add-to-list 'safe-local-variable-values '(projectile-project-compilation-cmd . "make dirhtml"))
  (add-to-list 'safe-local-variable-values '(projectile-project-test-cmd . "pre-commit run --all"))
  :config
  (projectile-mode 1))


;; 📦 PULSAR
;; Вспыхивание строки, к которой переместился курсор
;; https://github.com/protesilaos/pulsar
;; Этот пакет требует Emacs версии 27.1 или новее
(when (emacs-version-not-less-than 27 1)
  (unless (package-installed-p 'pulsar)
    (package-vc-install '(pulsar
                          :url "https://github.com/protesilaos/pulsar.git"
                          :branch "1.2.0")))
  (use-package pulsar
    :ensure t
    :custom (pulsar-pulse t)
    :hook
    (after-init . pulsar-global-mode)
    (next-error . pulsar-pulse-line)
    :config
    (add-to-list 'pulsar-pulse-functions 'ace-window)
    (add-to-list 'pulsar-pulse-functions 'flycheck-next-error)
    (add-to-list 'pulsar-pulse-functions 'flyspell-goto-next-error)
    (add-to-list 'pulsar-pulse-functions 'recenter-top-bottom)))


;; 📦 PYTHON-MODE
;; Встроенный пакет для работы с Python
(use-package python-mode
  :ensure t
  :custom
  (py-pylint-command-args "--max-line-length 120" "Дополнительные параметры, передаваемые pylint")
  (python-indent-guess-indent-offset-verbose nil "Выключить уведомления")
  (python-indent-offset 4 "Отсуп по умолчанию — 4 пробела"))


;; 📦 RAINBOW-DELIMITERS-MODE
;; https://github.com/Fanael/rainbow-delimiters
;; Подсветка парных скобок одним и тем же цветом
(unless (package-installed-p 'rainbow-delimiters)
  (package-vc-install '(rainbow-delimiters
                        :url "https://github.com/Fanael/rainbow-delimiters"
                        :branch "2.1.5")))
(use-package rainbow-delimiters
  :ensure t
  :delight ""
  :hook
  ((asciidoc-mode
    adoc-mode
    conf-mode
    css-mode
    emacs-lisp-mode
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


;; 📦 RUSSIAN-TECHWRITER
;; Метод ввода для технических писателей
;; https://github.com/dunmaksim/emacs-russian-techwriter-input-method
(use-package russian-techwriter
  :ensure t
  :custom
  (default-input-method 'russian-techwriter))


;; 📦 REVERSE-IM
;; https://github.com/a13/reverse-im.el
;; Чтобы сочетания клавиш работали в любой раскладке.
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"
                              "russian-techwriter"))
  :config
  (reverse-im-mode 1))


;; 📦 RG
;; https://github.com/dajva/rg.el
;; Фронтенд для `ripgrep', утилиты для поиска (должна быть установлена).
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))



;; 📦 RUBY-MODE
;; Встроенный пакет
(use-package ruby-mode
  :defer t
  :init
  (defvar ruby-indent-offset 2 "Ширина TAB'а в `ruby-mode'.")
  :mode
  ("\\Vagrantfile\\'"
   "\\.rb\\'"))


;; 📦 STANDARD-THEME
;; https://github.com/protesilaos/standard-themes
;; Почти как встроенные темы, только немного доработанные
(use-package standard-themes
  :ensure t)


;; 📦 SYMBOLS-OUTLINE
;; https://github.com/liushihao456/symbols-outline.el
;; Показывает переменные, функции, заголовки и другие части файла на панели
;; Требует наличия в системе `ctags'. В Debian рекомендуется использовать
;; пакет `universal-ctags'
(use-package symbols-outline
  :ensure t
  :custom
  (symbols-outline-window-width 40 "Ширина окна")
  :bind (:map global-map
              ("C-c i" . symbols-outline-show))
  :hook
  ((asciidoc-mode
    adoc-mode
    emacs-lisp-mode
    python-mode
    rst-mode
    ) . symbols-outline-follow-mode))



;; 📦 SWIPER
;; https://elpa.gnu.org/packages/swiper.html
;; Умный поиск и отличная (в некоторых случаях) замена `isearch-forward' и
;; `isearch-backward'.
(use-package swiper
  :ensure t
  :bind (:map global-map
              ("C-s" . swiper-isearch)
              ("C-r" . swiper-isearch-backward)))


;; 📦 TEMPEL
;; https://github.com/minad/tempel
;; Система шаблонов.
(unless (package-installed-p 'tempel)
  (package-vc-install '(tempel
                        :url "https://github.com/minad/tempel.git"
                        :branch "1.2")))
(use-package tempel
  :ensure t)


;; 📦 TERRAFORM-MODE
;; https://github.com/hcl-emacs/terraform-mode
;; Работа с файлами конфигурации Terraform и OpenTofu
(unless (package-installed-p 'terraform-mode)
  (package-vc-install '(terraform-mode
                        :url "https://github.com/hcl-emacs/terraform-mode.git"
                        :branch "1.0.1")))
(use-package terraform-mode
  :ensure t
  :defer t
  :mode ("\\.terraformrc\\'"
         "\\.tf\\'"
         "\\.tofurc\\'"
         "tofu\\.rc\\'"))


;; 📦 WEB-MODE
;; https://web-mode.org/
;; Режим для редактирования HTML и не только.
(unless (package-installed-p 'web-mode)
  (package-vc-install '(web-mode
                        :url "https://github.com/fxbois/web-mode.git"
                        :branch "v17.3.13")))
(use-package web-mode
  :ensure t
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
  :ensure t
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


;; 📦 YAML-MODE
;; https://github.com/yoshiki/yaml-mode
;; Работа с YAML-файлами
(unless (package-installed-p 'yaml-mode)
  (package-vc-install '(yaml-mode
                        :url "https://github.com/yoshiki/yaml-mode.git"
                        :branch "0.0.16")))
(use-package yaml-mode
  :ensure t
  :defer t
  :mode
  ("\\.ansible\\-lint\\'"
   "\\.clang\\-tidy\\'"
   "\\.pre\\-commit\\-config\\.yaml\\'"
   "\\.yaml\\'"
   "\\.yamllint\\'"
   "\\.yamllint\\-config\\.yaml\\'"
   "\\.yfm\\'"
   "\\.yml\\'"))


;; 📦 YASNIPPET
;; https://elpa.gnu.org/packages/yasnippet.html
;; Библиотека для управления сниппетами. Требуется для расширения функций Eglot.
(use-package yasnippet
  :ensure t
  :init
  (progn
    (defvar init-el-yasnippet-snippets-dir (expand-file-name "snippets" user-emacs-directory))
    (unless (file-directory-p init-el-yasnippet-snippets-dir)
      (make-directory init-el-yasnippet-snippets-dir))
    (unless (file-directory-p init-el-autosave-dir)
      (make-directory init-el-autosave-dir)))
  :config (yas-global-mode 1))


;; 📦 YASNIPPET-SNIPPETS
;; https://github.com/AndreaCrotti/yasnippet-snippets
;; Набор сниппетов для `yasnippet'
(use-package yasnippet-snippets
  :ensure t)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(load-theme init-el-theme t)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init.el)
;;; init.el ends here
