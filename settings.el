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
 '(centaur-tabs-height 32)
 '(centaur-tabs-set-bar (quote over))
 '(centaur-tabs-set-icons 1)
 '(centaur-tabs-style "rounded")
 '(column-number-mode 1)
 '(company-dabbrev-code-ignore-case nil)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-delay 1)
 '(company-tooltip-align-annotations t)
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
 '(global-hl-line-mode t)
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
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
      ("Lisp"
       (mode . emacs-lisp-mode))
      ("Dired"
       (mode . dired-mode))
      ("Python"
       (or
        (mode . python-mode)
        (mode . elpy-mode)))
      ("Web"
       (or
        (mode . web-mode)
        (mode . js2-mode)
        (mode . typescript-mode)
        (mode . javascript-mode)))
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
      ("TypeScript"
       (or
        (mode . typescript-mode)))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*\\(Customize\\|Help\\)")
        (name . "\\*\\(Echo\\|Minibuf\\)")))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-shrink-to-minimum-size t t)
 '(ibufffer-use-other-window t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message "")
 '(js2-highlight-undeclared-vars t)
 '(js2-highlight-unused-variables t)
 '(js2-strict-trailing-comma-warning t)
 '(line-number-mode t)
 '(linum-mode t t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
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
 '(package-menu--hide-packages 1 t)
 '(package-selected-packages
   (quote
    (all-the-icons-dired tide tide-mode typescript-mode centaur-tabs winum treemacs-magit treemacs-icons-dired treemacs-projectile treemacs rust-mode tabbar-mode tabbar dired-sidebar zones pydoc pyenv-mode darkokai-theme flycheck-clang-analyzer flycheck-pos-tip flycheck-pycheckers yasnippet night-owl-theme prettier-js ace-window lsp-javascript-typescript lsp-python lsp-rust lsp-ui srcery-theme tern racer flycheck-rust cargo zerodark-theme chyla-theme overcast-theme 2048-game panda-theme pipenv clang-format monokai-alt-theme company-anaconda anaconda-mode helm typescript persp-mode python-mode py-isort py-autopep8 isearch web-mode web-beautify magit yasnippet-snippets airline-themes powerline monokai-theme highlight-numbers rainbow-delimiters use-package neotree flycheck company all-the-icons)))
 '(persp-auto-resume-time 0)
 '(persp-auto-save-opt 0)
 '(persp-autokill-buffer-on-remove (quote kill-weak))
 '(persp-keymap-prefix (kbd "C-x p"))
 '(prettier-js-args (quote ("--tab-width" "4")))
 '(py-isort-options (quote ("-sl")))
 '(python-indent-guess-indent-offset 4)
 '(python-indent-offset 4)
 '(query-replace-highlight t)
 '(ring-bell-function (quote ignore))
 '(save-place t)
 '(scroll-bar-mode -1)
 '(scroll-conservatively 1000000)
 '(scroll-margin 10)
 '(scroll-step 1)
 '(search-highlight t)
 '(select-enable-clipboard t)
 '(tide-format-before-save t)
 '(tool-bar-mode nil)
 '(undo-limit 800000)
 '(undo-tree-mode-lighter "")
 '(use-package-always-ensure t)
 '(use-package-compute-statistics t)
 '(use-package-enable-imenu-support t)
 '(user-full-name "Maxim Dunaevsky")
 '(user-mail-address "dunaevsky@outlook.com")
 '(version-control t)
 '(warning-minimum-log-level :warning)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-css-colorization t)
 '(web-mode-markup-indent-offset 2)
 '(wg-morph-on nil)
 '(whitespace-auto-cleanup t t)
 '(workon-home "~/.virtualenvs"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; settings.el ends here
