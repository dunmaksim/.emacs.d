# Конфигурация EMACS для технических писателей

Я не использую пакетные менеджеры вроде `straight.el` или `use-package`. Вместо них для установки используется нативный `package.el`. Все необходимые пакеты устанавливаются автоматически при запуске редактора.

В этой конфигурации используются (в алфавитном порядке):

* `abbrev-mode`

    Встроенный пакет для использования аббревиатур — последовательностей, автоматически заменяемых на более длинные слова и предложения.

* [ace-window](https://github.com/abo-abo/ace-window)

    Пакет для управления окнами Emacs.

* [adoc-mode](https://github.com/bbatsov/adoc-mode)

    Версия пакета от `bbatsov`. Подробнее [здесь](https://github.com/bbatsov/adoc-mode).

    Предоставляет поддержку [AsciiDoc](https://docs.asciidoctor.org/asciidoc/).

* [aggressive-indent](https://github.com/Malabarba/aggressive-indent-mode)

    Принудительное выравнивание кода. Особенно полезен этот пакет для работы с `init.el`.

* [ansible](https://github.com/k1LoW/emacs-ansible)

    Поддержка синтаксиса для конфигураций Ansible. Дополнительный режим, расширяющий возможности `yaml-mode`.

* [bind-key](https://github.com/jwiegley/use-package)

    Обёртки над функциями Emacs, используемыми для привязки клавиш.

* `checkdoc`

    Встроенный пакет, используемый для проверки документации пакетов Emacs Lisp.

* [company](https://company-mode.github.io/)

    Автодополнение. Используется в основном при работе с текстами программ.

* `conf-mode`

    Встроенный пакет для работы с конфигурационными файлами формата INI.

* `css-mode`

    Встроенный пакет для работы с файлами CSS.

* [dashboard](https://github.com/emacs-dashboard/emacs-dashboard)

    "Рабочий стол" со списком закладок, последних и закрепленных файлов и проектов.

* `desktop`

    Сохранение состояния Emacs между сессиями.

* [diff-hl](https://github.com/dgutov/diff-hl)

    Подсветка незафиксированных изменений.

* `dired`

    Встроенный пакет для управления файлами.

* `display-line-numbers`

    Встроенный пакет для отображения номеров строк.

* [dockerfile-mode](https://github.com/spotify/dockerfile-mode)

    Поддержка конфигурационных файлов [Docker](https://docker.io/).

* [doom-modeline](https://github.com/seagle0128/doom-modeline)

    Строка статуса, используемая проектом [DooM EMACS](https://github.com/doomemacs/doomemacs). Выглядит отлично, поддерживает иконки и тонкую настройку внешнего вида.

* [doom-themes](https://github.com/doomemacs/themes)

    Темы из проекта [DooM EMACS](https://github.com/doomemacs/doomemacs). Мне больше всего нравится `monokai-pro`.

* [edit-indirect](https://github.com/Fanael/edit-indirect)

    Позволяет открыть буфер с фрагментом кода и нужным основным режимом во время работы с другим буфером. Например, код на Python, вставленный в Markdown-файл, откроется с основным режимом `anaconda-mode` и всеми его преимуществами: автодополнение, подсветка синтаксиса и т. д. Чтобы открыть фрагмент кода в другом буфере, нужно нажать **[C-c ']**, чтобы сохранить изменения — **[C-c C-c]**, закрыть буфер без сохранения изменений — **[C-c C-k]**.

* [editorconfig](https://github.com/editorconfig/editorconfig-emacs#readme)

    Поддержка [EditoConfig](https://editorconfig.org). Использование этой штуки задаёт правила форматирования текста на уровне проекта: TAB vs SPACES, типы концов строк и т. д.

* `elec-pair`

    Автоматическая вставка парной скобки.

* [flycheck](https://www.flycheck.org)

    Автоматическая проверка синтаксиса с помощью статических анализаторов. Flymake давно устарел, [flycheck](https://www.flycheck.org/) обходит его по всем параметрам. Используйте его для статического анализа кода и текстов.

* [flycheck-clang-tidy](https://github.com/ch1bo/flycheck-clang-tidy)

    Поддержка статического анализатора проекта Clang для проверки файлов на языках C и C++.

* [flycheck-color-mode-line](https://github.com/flycheck/flycheck-color-mode-line)

    Отображение результатов работы Flycheck с помощью цвета в статусной строке.

* [format-all](https://github.com/lassik/emacs-format-all-the-code)

    Форматирование кода с помощью внешних средств, например, `standard` для JavaScript, `black` для Python и т. д.

* [git-gutter](https://github.com/emacsorphanage/git-gutter)

    Статус Git на боковой панели. Добавленные строки будут отмечены зеленым, измененные — синим, удаленные — красным, конфликты — оранжевым.

* [go-mode](https://github.com/dominikh/go-mode.el)

    Поддержка языка программирования Golang.

* [highlight-indentation](https://github.com/antonj/Highlight-Indentation-for-Emacs)

    Отображает направляющие для отступов.

* [js2-mode](https://github.com/mooz/js2-mode/)

    Расширенная поддержка языка программирования JavaScript. Обладает значительно большими возможностями, чем встроенный режим `javascript-mode`.

* [lsp-mode](https://github.com/emacs-lsp/lsp-mode)

    Поддержка Language Server Protocol в EMACS.

* [lsp-ui](https://github.com/emacs-lsp/lsp-ui)

    Поддержка графического интерфейса при работе с LSP.

* [magit](https://github.com/magit/magit)

    Текстово-графичекий интерфейс для работы с системой контроля версий Git.

* [multiple-cursors](https://github.com/magnars/multiple-cursors.el)

    Поддержка мультикурсорности. Для редактирования выделенных строк нужно нажать **[C-S-c C-Sc]**.

* [php-mode](https://github.com/emacs-php/php-mode)

    Поддержка языка программирования PHP.

* [projectile](https://github.com/bbatsov/projectile)

    Работа с проектами.

* protobuf-mode

    Режим для работы с файлами Protobuf.

* [pyenv-mode](https://github.com/proofit404/pyenv-mode)

    Интеграция с [pyenv](https://github.com/pyenv/pyenv) — менеджером версий Python.

* [python-mode](https://gitlab.com/groups/python-mode-devs)

    Базовая поддержка языка программирования Python.

* [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)

    Парные скобки отображаются одним цветом.

* [scala-mode](https://github.com/hvesalai/emacs-scala-mode)

    Поддержка языка программирования Scala.

* [swiper](https://github.com/abo-abo/swiper)

    Продвинутый поиск по файлу. То же самое, что встроенный пакет `isearch`, только лучше.

* [terraform-mode](https://github.com/syohex/emacs-terraform-mode)

    Поддержка синтаксиса конфигурационных файлов Terraform.

* [treemacs](https://github.com/Alexander-Miller/treemacs)

    Дерево файлов в отдельном окне. Отличная замена устаревшему `neotree`.

* [treemacs-all-the-icons](https://github.com/Alexander-Miller/treemacs)

    Поддержка отображения иконок в Treemacs.

* [treemacs-icons-for-dired](https://github.com/Alexander-Miller/treemacs)

    Отображение иконок в `dired-mode`.

* [treemacs-magit](https://github.com/Alexander-Miller/treemacs)

    Интеграция Treemacs с Magit. Цвет файла в дереве показывает его состояние в Magit.

* [undo-tree](https://www.dr-qubit.org/undo-tree.html)

    Замена стандартной системы Undo/Redo, используемой в EMACS.

* [vagrant](https://github.com/ottbot/vagrant.el)

    Поддержка конфигурационных файлов Vagrant.

* [verb](https://github.com/federicotdn/verb)

    Работа с REST API путем отправки HTTP-запросов к нужным эндпоинтам. Конфигурация пишется в org-файлах. Возможно, лучше использовать специализированное решение типа [Insomnia](https://insomnia.rest).

* [web-mode](https://web-mode.org)

    Режим работы с Web-файлами: HTML, CSS.

* [wgrep](http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el)

    Аналог `grep`, позволяющий выполнять массовую замену текста средствами EMACS в нескольких файлах.

* [which-key](https://github.com/justbur/emacs-which-key)

    Показывает подсказки с комбинациями клавиш, привязанным к командам.

* `whitespace`

    Встроенный режим, отображающий невидимые символы: пробелы, табуляции, переходы на новую строку и т. п.

* [ws-butler](https://github.com/lewang/ws-butler)

    Старый, но очень полезный пакет: позволяет работать с большими файлами так, будто включен режим удаления висячих пробелов, но затрагивает только изменённые строки.

* [yaml-mode](https://github.com/yoshiki/yaml-mode)

    Поддержка синтаксиса языка разметки [YAML](https://yaml.org/).

* [yascroll](https://github.com/emacsorphanage/yascroll)

    Альтернативные полосы прокрутки: показываются только когда это необходимо, в остальное время не видны.

* [yasnippet](https://github.com/joaotavora/yasnippet)

    Поддержка сниппетов: развёртывания небольших фрагментов текста в заранее подготовленные большие блоки.

* [yasnippet-snippets](https://github.com/AndreaCrotti/yasnippet-snippets)

    Набор сниппетов для `yasnippet`.
