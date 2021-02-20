# Конфигурация EMACS для технических писателей

В качестве пакетного менеджера используется [straight.el](https://github.com/raxod502/straight.el) — форк [use-package](https://github.com/jwiegley/use-package), избавленный от его проблем.

В этой конфигурации используются (в алфавитном порядке):

* [flycheck](#flycheck)
* [markdown-mode](#markdown-mode)
* [whitespace-mode](#whitespace-mode)
* [ws-butler](#ws-butler)

## Flycheck {#flycheck}

Flymake давно устарел, [flycheck](https://www.flycheck.org/) обходит его по всем параметрам. Используйте его для статического анализа кода и текстов.

## markdown-mode {#markdown-mode}

Пакет [markdown-mode](https://github.com/jrblevin/markdown-mode) используется как основной способ редактирования файлов `markdown`. Этот `README.md` тоже набран с использованием режима `markdown`. Предоставляет множество возможностей, например, подсветку синтаксиса для примеров кода, переход по ссылкам, проверку ссылок и многое другое.

## whitespace-mode {#whitespace-mode}

Пакет [whitespace-mode](https://www.emacswiki.org/emacs/WhiteSpace) используется для отображения пробелов, переходов на новую строку, TAB'ов и так далее.

## ws-butler {#ws-butler}

Для удаления висящих пробелов некоторые рекомендуют использовать конструкцию:

```lisp
(add-to-list 'write-file-functions 'delete-trailing-whitespace)
```

Делать этого ни в коем случае не стоит, потому что удаляются висящие пробелы во всём файле. Это плохо, если вы работаете совместно с другими людьми над одним проектом, в котором много унаследованного кода. Всякая строка, из которой будут удалены лишние пробелы в конце или начале, попадёт в историю изменений, и коммиты будут слишком «шумными». Используйте пакет [ws-butler](https://github.com/lewang/ws-butler) — он удаляет висящие пробелы только в тех строках, которые вы меняли.
