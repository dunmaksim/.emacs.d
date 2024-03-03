# Конфигурация EMACS для технических писателей

Все необходимые пакеты устанавливаются автоматически при запуске редактора.
Для управления конфигурацией используются возможности пакета [use-package](https://www.gnu.org/software/emacs/manual/html_mono/use-package.html).

В этой конфигурации используются (в алфавитном порядке):


## abbrev-mode

Встроенный пакет для использования аббревиатур — последовательностей, автоматически заменяемых на более длинные слова и предложения.


## ace-window

Пакет для управления окнами Emacs. Я использую его для быстрого перехода между окнами с помощью сочетания **Alt+o**.

[Страница проекта](https://github.com/abo-abo/ace-window)


## adoc-mode

Пакет для поддержки [AsciiDoc](https://docs.asciidoctor.org/asciidoc/).

[Страница проекта](https://github.com/bbatsov/adoc-mode)


## aggressive-indent

Принудительное выравнивание кода. Особенно полезен этот пакет для работы с `init.el`.

[Страница проекта](https://github.com/Malabarba/aggressive-indent-mode)


## ansible

Поддержка синтаксиса для конфигураций Ansible. Дополнительный режим, расширяющий возможности `yaml-mode`.

[Страница проекта](https://github.com/k1LoW/emacs-ansible)


## bind-key

Обёртки над функциями Emacs, используемыми для привязки клавиш.

[Страница проекта](https://github.com/jwiegley/use-package)


## checkdoc

Встроенный пакет, используемый для проверки документации пакетов Emacs Lisp.


## company

Автодополнение. Используется в основном при работе с текстами программ.

[Страница проекта](https://company-mode.github.io/)


## conf-mode

Встроенный пакет для работы с конфигурационными файлами формата INI.


## css-mode

Встроенный пакет для работы с файлами CSS.


## dashboard

"Рабочий стол" со списком закладок, последних и закрепленных файлов и проектов.

[Страница проекта](https://github.com/emacs-dashboard/emacs-dashboard)


## desktop

Встроенный пакет для сохранение состояния Emacs между сессиями.

Документация Emacs: [Saving Emacs Sessions](https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html)


## diff-hl

Подсветка незафиксированных изменений.

[Страница проекта](https://github.com/dgutov/diff-hl)


## dired

Встроенный пакет для управления файлами.

Документация Emacs: [Dired, the Directory Editor](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html)


## display-line-numbers

Встроенный пакет для отображения номеров строк.


## dockerfile-mode

Поддержка конфигурационных файлов [Docker](https://docker.io/).

[Страница проекта](https://github.com/spotify/dockerfile-mode)


## doom-modeline

Строка статуса, используемая проектом [DooM EMACS][doom-emacs]. Выглядит отлично, поддерживает иконки и тонкую настройку внешнего вида.

[Страница проекта](https://github.com/seagle0128/doom-modeline)


## doom-themes

Темы из проекта [DooM EMACS][doom-emacs]. Мне больше всего нравится `monokai-pro`.

[Страница проекта](https://github.com/doomemacs/themes)


## edit-indirect

Позволяет открыть буфер с фрагментом кода и нужным основным режимом во время работы с другим буфером. Например, код на Python, вставленный в Markdown-файл, откроется с основным режимом `anaconda-mode` и всеми его преимуществами: автодополнение, подсветка синтаксиса и т. д. Чтобы открыть фрагмент кода в другом буфере, нужно нажать **[C-c ']**, чтобы сохранить изменения — **[C-c C-c]**, закрыть буфер без сохранения изменений — **[C-c C-k]**.

[Страница проекта](https://github.com/Fanael/edit-indirect)


## editorconfig

Поддержка [EditoConfig](https://editorconfig.org). Использование этой штуки задаёт правила форматирования текста на уровне проекта: TAB vs SPACES, типы концов строк и т. д.

[Страница проекта](https://github.com/editorconfig/editorconfig-emacs#readme)


## elec-pair

Встроенный пакет для автоматической вставки парной скобки.


## Flycheck

Автоматическая проверка синтаксиса с помощью статических анализаторов. Отличная замена Flymake.

[Страница проекта](https://www.flycheck.org)


## format-all

Форматирование кода с помощью внешних средств, например, `standard` для JavaScript, `black` для Python и т. д.

[Страница проекта](https://github.com/lassik/emacs-format-all-the-code)


## js2-mode

Расширенная поддержка языка программирования JavaScript. Обладает значительно большими возможностями, чем встроенный режим `javascript-mode`.

[Страница проекта](https://github.com/mooz/js2-mode/)


## Magit

Текстово-графичекий интерфейс для работы с системой контроля версий Git.

[Страница проекта](https://github.com/magit/magit)


## multiple-cursors

Поддержка мультикурсорности. Для редактирования выделенных строк нужно нажать **[C-S-c C-Sc]**.

[Страница проекта](https://github.com/magnars/multiple-cursors.el)


## Projectile

Работа с проектами. Аналог встроенного пакета `project.el`, предоставляющий дополнительные возможности.

[Страница проекта](https://github.com/bbatsov/projectile)


## Pulsar

Вспыхивание строки, к которой перемещён курсор. Помогает лучше ориентироваться среди множества открытых фреймов и окон.

[Страница проекта](https://git.sr.ht/~protesilaos/pulsar)


## python-mode

Базовая поддержка языка программирования Python.

[Страница проекта](https://gitlab.com/groups/python-mode-devs)


## rainbow-delimiters

Парные скобки отображаются одним цветом.

[Страница проекта](https://github.com/Fanael/rainbow-delimiters)


## terraform-mode

Поддержка синтаксиса конфигурационных файлов Terraform.

[Страница проекта](https://github.com/syohex/emacs-terraform-mode)


## treemacs

Дерево файлов в отдельном окне. Отличная замена устаревшему `neotree`.

[Страница проекта](https://github.com/Alexander-Miller/treemacs)


## undo-tree

Замена стандартной системы Undo/Redo, используемой в EMACS, на более удобную.

[Страница проекта](https://www.dr-qubit.org/undo-tree.html)


## vagrant

Поддержка конфигурационных файлов Vagrant.

[Страница проекта](https://github.com/ottbot/vagrant.el)


## web-mode

Режим работы с Web-файлами: HTML, CSS.

[Страница проекта](https://web-mode.org)


## which-key

Показывает подсказки с комбинациями клавиш, привязанным к командам.

[Страница проекта](https://github.com/justbur/emacs-which-key)


## whitespace

Встроенный пакет, отображающий невидимые символы: пробелы, табуляции, переходы на новую строку и т. п.


## ws-butler

Старый, но очень полезный пакет: позволяет работать с большими файлами так, будто включен режим удаления висячих пробелов, но затрагивает только изменённые строки.

[Страница проекта](https://github.com/lewang/ws-butler)


## yaml-mode

Поддержка синтаксиса языка разметки [YAML](https://yaml.org/).

[Страница проекта](https://github.com/yoshiki/yaml-mode)


## yasnippet

Поддержка сниппетов: развёртывания небольших фрагментов текста в заранее подготовленные большие блоки.

[Страница проекта](https://github.com/joaotavora/yasnippet)


## yasnippet-snippets

Набор сниппетов для `yasnippet`.

[Страница проекта](https://github.com/AndreaCrotti/yasnippet-snippets)


[doom-emacs]: https://github.com/doomemacs/doomemacs
