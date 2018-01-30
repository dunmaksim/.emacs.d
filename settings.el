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
 '(desktop-save-mode t)
 '(desktop-load-locked-desktop t)
 '(electric-pair-mode t)
 '(elpy-rpc-backend "jedi")
 '(flycheck-display-errors-delay 0.0)
 '(flycheck-standard-error-navigation nil)
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
        (mode . tide-mode)
        (mode . typescript-mode)))
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
 '(line-number-mode t)
 '(linum-mode t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(neo-smart-open 0)
 '(neo-theme (if (display-graphic-p) (quote icons) (quote arrow)))
 '(next-line-add-newlines nil)
 '(package-selected-packages
   (quote
    (monokai-alt-theme company-anaconda anaconda-mode helm typescript persp-mode elpy python-mode py-isort py-autopep8 isearch web-mode web-beautify tide magit yasnippet-snippets airline-themes powerline monokai-theme highlight-numbers rainbow-delimiters use-package neotree flycheck company all-the-icons)))
 '(persp-auto-resume-time 0)
 '(persp-auto-save-opt 0)
 '(persp-autokill-buffer-on-remove (quote kill-weak))
 '(persp-keymap-prefix (kbd "C-x p"))
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
 '(tide-format-options
   (quote
    (:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil)))
 '(tide-hl-identifier-mode 1 t)
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
 '(whitespace-auto-cleanup t t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; settings.el ends here
