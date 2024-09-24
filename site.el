;;; site.el --- Настройки сторонних пакетов
;;; Commentary:
;;; Этот код может выполняться, а может и упасть.
;;; Code:

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

;; ;; Настройки отладочного режима
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
  :ensure t
  :vc (
       :url "https://git.savannah.nongnu.org/git/delight.git"
       :rev "1.7"))


;; 📦 ACE-WINDOW
;; https://github.com/abo-abo/ace-window
;; Быстрое переключение между окнами по M+o
(use-package ace-window
  :ensure t
  :vc (
       :url "https://github.com/abo-abo/ace-window.git"
       :rev "0.10.0")
  :bind (:map global-map
              ("M-o" . ace-window)))


;; 📦 ACTIVITIES
;; https://elpa.gnu.org/packages/activities.html
;; Управление наборами окон, вкладок, фреймов и буферов
(use-package activities
  :ensure t
  :vc (
       :url "https://github.com/alphapapa/activities.el.git"
       :rev "v0.7.1")
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
(use-package adoc-mode
  :ensure t
  :defer t
  :custom
  (adoc-fontify-code-blocks-natively 10000)
  :mode ("\\.adoc\\'" . adoc-mode))


;; 📦 AGGRESSIVE-INDENT
;; https://github.com/Malabarba/aggressive-indent-mode
;; Принудительное выравнивание кода
(use-package aggressive-indent
  :ensure t
  :vc (
       :url "https://github.com/Malabarba/aggressive-indent-mode.git"
       :rev "1.10.0")
  :defer t
  :hook
  ((emacs-lisp-mode
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
  :ensure t
  :vc (
       :url "https://github.com/pythonic-emacs/anaconda-mode.git"
       :rev "v0.1.16"
       )
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))


;; 📦 ANSIBLE
;; https://gitlab.com/emacs-ansible/emacs-ansible
;; Дополнительные возможности при работе с YAML-файлами Ansible
(use-package ansible
  :ensure t
  :vc (
       :url "https://gitlab.com/emacs-ansible/emacs-ansible.git"
       :rev "0.3.2"
       )
  :defer t)


;; 📦 APHELEIA
;; https://github.com/radian-software/apheleia
;; Форматирование содержимого буфера с помощью внешних средств
(use-package apheleia
  :ensure t
  :vc (
       :url "https://github.com/radian-software/apheleia.git"
       :rev "v4.2")
  :delight "")


;; 📦 BBCODE-MODE
;; https://github.com/lassik/emacs-bbcode-mode
;; Режим редактирования BB-кодов
(use-package bbcode-mode
  :ensure t
  :vc (
       :url "https://github.com/lassik/emacs-bbcode-mode.git"
       :rev "v2.3.0")
  :defer t)


;; 📦 BREADCRUMB
;; https://elpa.gnu.org/packages/breadcrumb.html
;; Упрощает навигацию по сложным документам: показывает хлебные
;; крошки в заголовках окон и позволяет быстро перейти в нужное место
;; с помощью `breadcrumb-jump'.
(use-package breadcrumb
  :ensure t
  :hook
  ((emacs-lisp-mode
    rst-mode) . breadcrumb-local-mode))


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
    :hook
    ((
      hack-local-variables
      comint-mode
      ) . buffer-env-update)))


;; 📦 COLORFUL-MODE
;; https://github.com/DevelopmentCool2449/colorful-mode
;; Отображение цветов прямо в буфере. Наследник `raibow-mode.el'.
(use-package colorful-mode
  :ensure t
  :vc (
       :url "https://github.com/DevelopmentCool2449/colorful-mode.git"
       :rev "v1.0.4")
  :hook (css-mode
         web-mode))


;; 📦 COMPANY-MODE
;; https://company-mode.github.io/
;; Автодополнение
(use-package company
  :ensure t
  :vc (
       :url "https://github.com/company-mode/company-mode.git"
       :rev "1.0.2"
       )
  :delight ""
  :demand t
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
  (use-package denote
    :ensure t
    :vc (
         :url "https://github.com/protesilaos/denote.git"
         :rev "3.1.0")
    :custom
    (denote-directory "~/Документы/Notes/" "Каталог для хранения заметок.")))



;; 📦 DIFF-HL
;; https://github.com/dgutov/diff-hl
;; Показывает небольшие маркеры рядом с незафиксированными изменениями. Дополняет функциональность git-gutter,
;; которые показывает изменения только в обычных буферах. Этот пакет умеет работать с dired и другими режимами.
(use-package diff-hl
  :ensure t
  :vc (
       :url "https://github.com/dgutov/diff-hl.git"
       :rev "1.10.0")
  :commands (diff-hl-mode diff-hl-dired-mode)
  :config
  (global-diff-hl-mode 1)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :hook
  ((adoc-mode
    emacs-lisp-mode
    markdown-mode
    python-mode
    rst-mode
    yaml-mode). diff-hl-mode))


;; 📦 DOCKERFILE-MODE
;; https://github.com/spotify/dockerfile-mode
;; Работа с файлами `Dockerfile'.
(use-package dockerfile-mode
  :ensure t
  :vc (
       :url "https://github.com/spotify/dockerfile-mode.git"
       :rev "v1.9")
  :defer t
  :mode
  ("\\Dockerfile\\'" . dockerfile-mode))


;; 📦 DOOM-THEMES
;; https://github.com/doomemacs/themes
;; Темы из DOOM Emacs
;; (use-package doom-themes
;;   :ensure t
;;   :custom
;;   (doom-themes-enable-bold t "Включить поддержку полужирного начертания.")
;;   (doom-themes-enable-italic t "Включить поддержку наклонного начертания."))
(use-package doom-themes
  :ensure t
  :vc (
       :url "https://github.com/doomemacs/themes.git"
       :rev "v2.3.0"))


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
  :ensure t
  :vc (
       :url "https://github.com/Fanael/edit-indirect.git"
       :rev "0.1.13")
  :defer t
  :bind (:map global-map
              ("C-c '" . edit-indirect-region)))


;; 📦 EDITORCONFIG
;; Поддержка https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :ensure t
  :vc (
       :url "https://github.com/editorconfig/editorconfig-emacs.git"
       :rev "v0.11.0")
  :delight ""
  :config
  (editorconfig-mode 1)
  :mode
  ("\\.editorconfig\\'" . editorconfig-conf-mode))


;; 📦 EF-THEMES
;; https://github.com/protesilaos/ef-themes.git
(use-package ef-themes
  :ensure t
  :vc (
       :url "https://github.com/protesilaos/ef-themes.git"
       :rev "1.8.0"))
(setq init-el-theme 'ef-elea-dark)


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
  (use-package eglot
    :ensure t
    :vc (
         :url "https://github.com/joaotavora/eglot.git"
         :rev "1.17")
    :defer t
    :config
    (add-to-list 'eglot-server-programs '(ansible-mode . ("ansible-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs '(dockerfile-mode . ("docker-langserver" "--stdio")))
    (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
    (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
    (add-to-list 'eglot-server-programs '(ruby-mode . ("bundle" "exec" "rubocop" "--lsp")))
    (add-to-list 'eglot-server-programs '(rst-mode . ("esbonio")))
    (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
    :hook
    ((ansible-mode
      dockerfile-mode
      markdown-mode
      python-mode
      rst-mode
      ruby-mode
      yaml-mode
      ) . eglot-ensure)))


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
  (python-mode . eldoc-mode))


;; 📦 FLYCHECK
;; https://www.flycheck.org/
;; Проверка синтаксиса на лету с помощью статических анализаторов
(defconst flycheck-default-margin-str "⮾")
(use-package flycheck
  :ensure t
  :vc (
       :url "https://github.com/flycheck/flycheck.git"
       :rev "34.1")
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save new-line))
  (flycheck-highlighting-mode 'lines "Стиль отображения проблемных мест — вся строка")
  (setq flycheck-indication-mode 'left-fringe "Место размещения маркера ошибки — левая граница")
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
  :hook
  (emacs-lisp-mode . flylisp-mode))


;; 📦 FLYMAKE
;; Более свежая версия встроенного пакета из репозитория gnu
;; Используется для проверки `init.el'.
;; https://elpa.gnu.org/packages/flymake.html
(use-package flymake
  :ensure t
  :hook
  ((emacs-lisp-mode
    lisp-data-mode
    ) . flymake-mode))


;; 📦 FORMAT-ALL
;; https://github.com/lassik/emacs-format-all-the-code
;; Форматирование кода с помощью разных внешних средств.
(use-package format-all
  :ensure t
  :vc (
       :url "https://github.com/lassik/emacs-format-all-the-code.git"
       :rev "0.6.0")
  :defer t
  :bind (:map global-map
              ([f12] . format-all-buffer)))


;; 📦 HELM
;; https://github.com/emacs-helm/helm
;; Подсказки и автодополнение ввода.
;; [C-o] — переключение между источниками подсказок (история и полный список команд)
(use-package helm
  :ensure t
  :vc (
       :url "https://github.com/emacs-helm/helm.git"
       :rev "v4.0")
  :delight ""
  :config
  (helm-mode 1)
  :bind (:map global-map
              ;; ("C-x C-f" . helm-find-files)
              ;; ("C-x b" . helm-buffers-list)
              ;; ("M-x" . helm-M-x)
              ("M-y" . helm-show-kill-ring)))


;; 📦 HELM-PROJECTILE
;; https://github.com/bbatsov/helm-projectile
;; Интеграция HELM с PROJECTILE
(use-package helm-projectile
  :ensure t
  :vc (
       :url "https://github.com/bbatsov/helm-projectile.git"
       :rev "v1.0.0")
  :delight ""
  :requires (helm projectile)
  :after (helm projectile)
  :config
  (helm-projectile-on))


;; 📦 HL-TODO
;; https://github.com/tarsius/hl-todo
;; Подсветка TODO, FIXME и т. п.
(use-package hl-todo
  :ensure t
  :vc (
       :url "https://github.com/tarsius/hl-todo.git"
       :rev "v3.8.1")
  :config (global-hl-todo-mode t))


;; 📦 IVY
;; https://elpa.gnu.org/packages/ivy.html
;; https://elpa.gnu.org/packages/doc/ivy.html
;; Функции фильтрации и выбора элементов. Как Helm, но теперь в
;; составе Emacs
(use-package ivy
  :ensure t
  :demand t
  :config
  (ivy-mode 1)
  :bind
  (:map global-map
        ("C-x b" . ivy-switch-buffer)
        ("C-c v" . ivy-push-view)
        ("C-c V" . ivy-pup-view)))


;; 📦 JSON-MODE
;; https://github.com/json-emacs/json-mode
;; Поддержка JSON
(use-package json-mode
  :ensure t
  :vc (
       :url "https://github.com/json-emacs/json-mode.git"
       :rev "v1.9.2")
  :defer t
  :mode ("\\.json\\'" . json-mode))


;; 📦 MAGIT
;; https://magit.vc/
;; Magic + Git + Diff-HL.
;; Лучшее средство для работы с Git.
(use-package magit
  :ensure t
  :vc (
       :url "https://github.com/magit/magit.git"
       :rev "v4.1.0"
       :lisp "lisp")
  :demand t
  :custom
  (magit-auto-revert-mode t "Автоматически обновлять файлы в буферах при изменениях на диске.")
  (magit-define-global-key-bindings t "Включить глобальные сочетания Magit.")
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))


;; 📦 MAGIT-FILE-ICONS
;; https://github.com/gekoke/magit-file-icons
;; Иконки в буферах Magit
(use-package magit-file-icons
  :after magit
  :vc (
       :url "https://github.com/gekoke/magit-file-icons.git"
       :rev "v2.0.0")
  :config
  (magit-file-icons-mode 1))


;; 📦 MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
;; Режим для работы с файлами в формате Markdown
(when (emacs-version-not-less-than 27 1)
  (use-package markdown-mode
    :ensure t
    :vc (
         :url "https://github.com/jrblevin/markdown-mode.git"
         :rev "v2.6")
    :defer t
    :custom
    (markdown-fontify-code-blocks-natively t "Подсвечивать синтаксис в примерах кода")
    (markdown-header-scaling-values '(1.0 1.0 1.0 1.0 1.0 1.0) "Все заголовки одной высоты")
    (markdown-list-indent-width 4 "Размер отступа для выравнивания вложенных списков")
    :config (setq-local word-wrap t)
    :bind (:map markdown-mode-map
                ("M-." . markdown-follow-thing-at-point))
    :mode ("\\.md\\'" . markdown-mode)))


;; 📦 MODUS-THEMES
;; https://www.gnu.org/software/emacs/manual/html_node/modus-themes/index.html
(use-package modus-themes
  :ensure t)


;; 📦 MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
;; Позволяет использовать мультикурсорность.
;; (use-package multiple-cursors
;;   :ensure t
;;   :bind (:map global-map
;;               ("C-S-c C-S-c" . mc/edit-lines)
;;               ("C->" . mc/mark-next-like-this)
;;               ("C-<" . mc/mark-previous-like-this)
;;               ("C-c C-<" . mc/mark-all-like-this))
;;   :config
;;   (global-unset-key (kbd "M-<down-mouse-1>"))
;;   (global-set-key (kbd "M-<mouse-1>" #'mc/add-cursor-on-click)))


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
(use-package org
  :ensure t
  :defer t
  :config
  (setq-local
   truncate-lines nil ;; Не обрезать строки
   word-wrap t))      ;; Перенос длинных строк


;; 📦 PACKAGE-LINT
;; https://github.com/purcell/package-lint
;; Проверка пакетов Emacs
(use-package package-lint
  :ensure t
  :vc (
       :url "https://github.com/purcell/package-lint.git"
       :rev "0.23")
  :defer t)


;; 📦 PHP-MODE
;; https://github.com/emacs-php/php-mode
;; Работа с файлами PHP
(use-package php-mode
  :ensure t
  :vc (
       :url "https://github.com/emacs-php/php-mode.git"
       :rev "v1.25.1")
  :mode("\\.php\\'" . php-mode))


;; 📦 PO-MODE
;; https://www.gnu.org/software/gettext/manual/html_node/Installation.html
;; Работа с файлами локализации.
;; Необходимо установить в систему эти пакеты:
;; * gettext
;; * gettext-el
(use-package po-mode
  :ensure t
  :mode
  ("\\.po\\'\\|\\.po\\." . po-mode))


;; 📦 PROJECTILE
;; https://docs.projectile.mx/projectile/installation.html
;; Управление проектами. Чтобы каталог считался проектом, он должен быть
;; под контролем любой системы версионирования, либо содержать специальные
;; файлы. В крайнем случае сгодится пустой файл .projectile
;; Подробнее здесь: https://docs.projectile.mx/projectile/projects.html
(use-package projectile
  :ensure t
  :vc (
       :url "https://github.com/bbatsov/projectile.git"
       :rev "v2.8.0")
  :delight ""
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :bind
  ([f7] . projectile-compile-project)
  :config
  (projectile-mode 1))


;; 📦 PULSAR
;; Вспыхивание строки, к которой переместился курсор
;; https://github.com/protesilaos/pulsar
;; Этот пакет требует Emacs версии 27.1 или новее
(when (emacs-version-not-less-than 27 1)
  (use-package pulsar
    :ensure t
    :vc (
         :url "https://github.com/protesilaos/pulsar.git"
         :rev "1.1.0")
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
  :ensure t
  :custom
  (py-pylint-command-args "--max-line-length 120" "Дополнительные параметры, передаваемые pylint")
  (python-indent-guess-indent-offset-verbose nil "Выключить уведомления")
  (python-indent-offset 4 "Отсуп по умолчанию — 4 пробела"))


;; 📦 RAINBOW-DELIMITERS-MODE
;; https://github.com/Fanael/rainbow-delimiters
;; Подсветка парных скобок одним и тем же цветом
(use-package rainbow-delimiters
  :ensure t
  :vc (
       :url "https://github.com/Fanael/rainbow-delimiters"
       :rev "2.1.5")
  :delight ""
  :hook
  ((
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


;; 📦 REVERSE-IM
;; https://github.com/a13/reverse-im.el
;; Чтобы сочетания клавиш работали в любой раскладке.
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode 1))


;; 📦 RUSSIAN-TECHWRITER
;; Метод ввода для технических писателей
;; https://github.com/dunmaksim/emacs-russian-techwriter-input-method
(use-package russian-techwriter
  :ensure t
  :custom
  (default-input-method 'russian-techwriter))


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


;; 📦 SWIPER
;; https://elpa.gnu.org/packages/swiper.html
;; Умный поиск и отличная замена `isearch-forward' и
;; `isearch-backward'.
(use-package swiper
  :ensure t
  :bind (:map global-map
              ("C-s" . swiper-isearch)))


;; 📦 TEMPEL
;; https://github.com/minad/tempel
;; Система шаблонов.
(use-package tempel
  :ensure t
  :vc (
       :url "https://github.com/minad/tempel.git"
       :rev "1.2"))


;; 📦 TERRAFORM-MODE
;; https://github.com/hcl-emacs/terraform-mode
;; Работа с файлами конфигурации Terraform
(use-package terraform-mode
  :ensure t
  :vc (
       :url "https://github.com/hcl-emacs/terraform-mode.git"
       :rev "1.0.1")
  :defer t
  :mode
  ("\\.terraformrc\\'" . terraform-mode)
  ("\\.tf\\'" . terraform-mode))


;; 📦 WEB-MODE
;; https://web-mode.org/
;; Режим для редактирования HTML и не только.
(use-package web-mode
  :ensure t
  :vc (
       :url "https://github.com/fxbois/web-mode.git"
       :rev "v17.3.13")
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
(use-package yaml-mode
  :ensure t
  :vc (
       :url "https://github.com/yoshiki/yaml-mode.git"
       :rev "0.0.16")
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



(provide 'site.el)
;;; site.el ends here
