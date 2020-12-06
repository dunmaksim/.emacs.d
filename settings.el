;;; settings.el --- Summary
;;; Commentary:
;;; Custom settings for Emacs.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(column-number-mode 1)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes t)
 '(delete-by-moving-to-trash t)
 '(desktop-load-locked-desktop t)
 '(desktop-save-mode t)
 '(electric-pair-mode t)
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-virtualenv-path (quote current))
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoctor asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cwl d-dmd dockerfile-hadolint emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-megacheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json jsonnet less less-stylelint llvm-llc lua-luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc pug puppet-parser puppet-lint python-pylint python-pycompile r-lintr racket rpm-rpmlint markdown-markdownlint-cli markdown-mdl nix rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar tex-chktex tex-lacheck texinfo typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(ibuffer-default-display-maybe-show-predicates t)
 '(ibuffer-expert t)
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 16 -1)
           " "
           (size 6 -1 :right)
           " "
           (mode 16 16)
           " " filename)
     (mark " "
           (name 16 -1)
           " " filename))))
 '(ibuffer-maybe-show-regexps nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message "")
 '(make-backup-files nil)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location "right")
 '(mode-icons-change-mode-name nil)
 '(neo-smart-open 0)
 '(neo-theme (if (display-graphic-p) (quote icons) (quote arrow)))
 '(neo-window-fixed-size nil)
 '(neo-window-width 30)
 '(next-line-add-newlines nil)
 '(package-hidden-regexps
   (quote
    ("\\`2048-game" "\\`0blayout" "\\flymake*" "\\ac-*" "\\aa-edit-mode" "\\abc-mode" "\\abgaben" "\\ada-*" "\\aes" "\\adoc-mode" "\\ahk-mode" "\\alchemist" "\\alda-mode" "\\anaphora" "\\android-mode" "\\angular-mode" "\\annotate*" "\\ansi" "\\ant" "\\anx-api")))
 '(package-menu-hide-packages 1 t)
 '(prettier-js-args (quote ("--tab-width" "4")))
 '(py-isort-options (quote ("-sl")))
 '(python-indent-guess-indent-offset 4)
 '(python-indent-offset 4)
 '(query-replace-highlight t)
 '(ring-bell-function (quote ignore))
 '(save-place t)
 '(scroll-conservatively 1000000)
 '(scroll-margin 10)
 '(scroll-step 1)
 '(search-highlight t)
 '(select-enable-clipboard t)
 '(tide-format-before-save t)
 '(undo-limit 800000)
 '(undo-tree-mode-lighter "")
 '(user-full-name "Maxim Dunaevsky")
 '(user-mail-address "dunaevsky@outlook.com")
 '(version-control t)
 '(warning-minimum-log-level :warning)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-css-colorization t)
 '(web-mode-markup-indent-offset 2)
 '(wg-morph-on nil)
 '(whitespace-auto-cleanup t t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; settings.el ends here
