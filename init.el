;;; init.el --- Summary
;;; Commentary:
;;; Main EMACS settings file, load settings from parts.

;;; Code:


(setq-default
 buffer-file-coding-system 'utf-8-auto-unix ;; UTF-8 everywhere
 create-lockfiles nil ;; Don't create lock-files
 inhibit-startup-message t ;; No startup message
 initial-scratch-message "" ;; No scratch message
 initial-major-mode 'fundamental-mode ;; fundamental-mode by default
 inhibit-splash-screen t ;; disable splash screen
 make-backup-files nil ;; Don't create backup files
 truncate-lines 1 ;; Wrap lines everywhere
 )


;; Shift+arrow for moving to another window
(windmove-default-keybindings)

(fset 'yes-or-no-p 'y-or-n-p) ;;; Shortcuts for yes and no

(global-hl-line-mode t) ;; Highlight current line
(overwrite-mode nil) ;; Disable overwrite mode

;; Resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Exit on Ctrl+Q
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)


;; Settings for window (not only a Windows!) system.
(when window-system
  (blink-cursor-mode 0)
  (fringe-mode 2)
  (scroll-bar-mode 0) ;; Off scrollbars
  (menu-bar-mode 0) ;; Off menu
  (tool-bar-mode 0) ;; Off toolbar
  (tooltip-mode 0) ;; No windows for tooltip
  (window-divider-mode 0)
  (set-face-attribute 'default nil :height 110)

  ;; Font settings for Linux and Windows
  (cond
   (
    (string-equal system-type "windows-nt")
    (when (member "Consolas" (font-family-list))
      (set-face-attribute 'default nil :font "Consolas")))
   (
    (string-equal system-type "gnu/linux")
    (when (member "DejaVu Sans Mono" (font-family-list))
      (set-face-attribute 'default nil :font "DejaVu Sans Mono")))))

;;; Save user settings in dedicated file
(setq custom-file "~/.emacs.d/settings.el")
(load-file "~/.emacs.d/settings.el")


;; Auto-revert mode
(global-auto-revert-mode 1)


;; ENCODING
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
;; (setq-default buffer-file-coding-system 'utf-8-auto-unix)


;; AUTO INSTALL STRAIGHT BOOTSTRAP
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;; Settings for hotkeys on any layout
(defun cfg:reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(cfg:reverse-input-method 'russian-computer)


(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))


;; Save/close/open
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)
(global-set-key (kbd "C-r") 'revert-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-'") 'comment-or-uncomment-region)
(global-set-key (kbd "C-o") 'dired)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Buffers and windows
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "<f7>") 'xah-new-empty-buffer)


(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "M-4") 'split-window-horizontally)
(global-set-key (kbd "M-5") 'split-window-vertically)
(global-set-key (kbd "M-6") 'balance-windows)

;; Search and replace
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-h") 'query-replace)
(global-set-key (kbd "C-S-h") 'query-replace-regexp)

;; Execute commands
(global-set-key (kbd "M-a") 'execute-extended-command)
(global-set-key (kbd "M-x") 'kill-whole-line)
(global-set-key (kbd "<esc>") 'keyboard-quit)


(global-set-key (kbd "M--")(lambda()(interactive)(insert "—"))) ;; Long dash by Alt+-

(global-unset-key (kbd "<insert>")) ;; Disable overwrite mode
(global-unset-key (kbd "M-,")) ;; Disable M-, as markers

(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))


;; AIRLINE THEMES
(straight-use-package 'airline-themes)


;; ALL THE ICONS
(when window-system
  (straight-use-package 'all-the-icons)
  (cond
   ;; Install fonts in GNU / Linux
   ((string-equal system-type "gnu/linux")(unless (file-directory-p "~/.local/share/fonts/") (all-the-icons-install-fonts)))
   ((string-equal system-type "windows-nt")(progn (message "Download and install fonts with all-the-icons-install-fonts command.")))))


;; ALL THE ICONS DIRED
;; https://github.com/jtbm37/all-the-icons-dired
(when window-system
  (straight-use-package 'all-the-icons-dired)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


;; ALL THE ICONS IBUFFER
;; https://github.com/seagle0128/all-the-icons-ibuffer
(when window-system
  (straight-use-package 'all-the-icons-ibuffer))


;; AUTO VIRTUALENVWRAPPER
(straight-use-package 'auto-virtualenvwrapper)
(add-hook 'python-mode-hook 'auto-virtualenvwrapper-activate)


;; BEACON
(straight-use-package 'beacon)
(beacon-mode 1)


;;;; COMPANY
(straight-use-package 'company)
(defun customize-company-mode-hook()
  "Settings for company-mode."
  (defvar company-dabbrev-code-ignore-case nil)
  (defvar company-dabbrev-downcase nil)
  (defvar company-dabbrev-ignore-case nil)
  (defvar company-idle-delay 0)
  (defvar company-minimum-prefix-length 2)
  (defvar company-quickhelp-delay 1)
  (defvar company-tooltip-align-annotations t)
  (setq
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case nil
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-quickhelp-delay 1
   company-tooltip-align-annotations t))


;; CONF MODE FOR INI / CONF / LIST
(straight-use-package 'conf-mode)
(add-to-list 'auto-mode-alist '("\\.pylintrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.list\\'" . conf-mode))


;; CUA-MODE
;; Ctrl+X, Ctrl+V, Ctrl+Z and other Windows-like shortcuts.
(cua-mode 1)


;; ELECTRIC-PAIR-MODE
(electric-pair-mode 1)
(setq-default electric-pair-pairs
              '(
                (?\« . ?\»)
                (?\„ . ?\“)))


;; ELISP MODE
(defun setup-elisp-mode ()
  "Settings for EMACS Lisp Mode."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'emacs-lisp-mode-hook #'setup-elisp-mode)
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))


;; ELPY
;; https://elpy.readthedocs.io/
(straight-use-package 'elpy)
(setq elpy-rpc-python-command "python3")
(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'elpy-format-code nil t)))
(elpy-enable)
(remove-hook 'elpy-modules 'elpy-module-flymake)
;; Отключить старый flymake, включить flycheck
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(defalias 'workon 'pyvenv-workon)


;; EMMET
(straight-use-package 'emmet-mode)


;; FLYCHECK
(straight-use-package `flycheck)
(defun setup-flycheck-mode ()
  "Settings for 'flycheck-mode'."
  (interactive)
  (setq-default
   flycheck-check-syntax-automatically '(mode-enabled save)
   flycheck-indication-mode 'left-margin
   flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc"))
(add-hook 'flycheck-mode-hook #'setup-flycheck-mode)


;; FLYCHECK-COLOR-MODE-LINE: highlight line by flycheck state
;; https://github.com/flycheck/flycheck-color-mode-line
(straight-use-package 'flycheck-color-mode-line)
(with-eval-after-load "flycheck"
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;; FLYCHECK INDICATOR
(straight-use-package 'flycheck-indicator)
(with-eval-after-load "flycheck"
  (add-hook 'flycheck-mode-hook 'flycheck-indicator-mode))


;; FLYCHECK-POS-TIP
;; https://github.com/flycheck/flycheck-pos-tip
(when window-system
  (straight-use-package 'flycheck-pos-tip)
  (with-eval-after-load "flycheck"
    (flycheck-pos-tip-mode 1)))


;; FORMAT ALL
;; https://github.com/lassik/emacs-format-all-the-code
(straight-use-package 'format-all)
(global-set-key (kbd "<f12>") 'format-all-buffer)


;; HELM
(straight-use-package 'helm)
(global-set-key (kbd "<f10>") 'helm-buffers-list)
(helm-mode 1)


;; HELM-COMPANY
(straight-use-package 'helm-company)
(defvar company-active-map)
(define-key company-active-map (kbd "C-:") 'helm-company)


;; IBUFFER
(straight-use-package 'ibuffer)
(defalias 'list-buffers 'ibuffer)
(require 'ibuf-ext)
(defun setup-ibuffer ()
  "Settings for 'ibuffer-mode'."
  (interactive)
  (defvar ibuffer-maybe-show-regexps)
  (setq
   ibuffer-hidden-filter-groups (list "Helm" "*Internal*")
   ibuffer-maybe-show-regexps nil
   ibuffer-saved-filter-groups (quote
                                (("default"
                                  ("Dired"
                                   (mode . dired-mode))
                                  ("Org"
                                   (mode . org-mode))
                                  ("Markdown"
                                   (mode . markdown-mode))
                                  ("YAML"
                                   (mode . yaml-mode))
                                  ("Protobuf"
                                   (mode . protobuf-mode))
                                  ("Lisp"
                                   (mode . emacs-lisp-mode))
                                  ("Python"
                                   (or
                                    (mode . python-mode)
                                    (mode . elpy-mode)))
                                  ("Shell-script"
                                   (or
                                    (mode . shell-script-mode)))
                                  ("Terraform"
                                   (or
                                    (mode . terraform-mode)))
                                  ("Web"
                                   (or
                                    (mode . web-mode)))
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
                                    (name . "\\*\\(Echo\\|Minibuf\\)"))))))
   ;;   ibuffer-shrink-to-minimum-size t
   ibuffer-formats
   '((mark modified read-only " "
           (name 60 60 :left :elide)
           (size 10 10 :right)
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 60 60)
           " " filename))
   ibuffer-use-other-window nil)
  (ibuffer-switch-to-saved-filter-groups "default")
  (ibuffer-update nil)
  (when window-system
    (all-the-icons-ibuffer-mode 1)
    ))
(global-set-key (kbd "<f2>") 'ibuffer)
(add-to-list 'ibuffer-never-show-predicates "^\\*")
(add-hook 'ibuffer-mode-hook #'setup-ibuffer)


;; JSON-MODE
;; https://github.com/joshwnj/json-mode
(straight-use-package 'json-mode)
(defun setup-json-mode()
  "Settings for json-mode."
  (company-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.\\(?:json\\|bowerrc\\|jshintrc\\)\\'" . json-mode))
(add-hook 'json-mode-hook #'setup-json-mode)


;; LSP MODE
(straight-use-package 'lsp-mode)
(add-hook 'python-mode-hook 'lsp)
(add-hook 'javacript-mode-hook 'lsp)


;; LSP-JEDI
(straight-use-package 'lsp-jedi)
(defvar lsp-disabled-clients)
(defvar lsp-enabled-clients)
(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-enabled-clients 'jedi))


;; MAGIT
;; https://magit.vc/
(straight-use-package 'magit)
(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'magit-checkout)


;; MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
(straight-use-package 'markdown-mode)
(defun setup-markdown-mode()
  "Settings for editing markdown documents."
  (interactive)
  ;; Настройки отступов и всякое такое
  (setq
   global-hl-line-mode nil
   header-line-format " "
   left-margin-width 4
   line-spacing 3
   right-margin-width 4
   word-wrap t)

  ;; Additional modes
  (abbrev-mode 1)
  (buffer-face-mode 1)
  (company-mode 1)
  (flycheck-mode 1) ;; Turn on linters
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1) ;; Highlight brackets
  (visual-line-mode 1) ;; Highlight current line
  (whitespace-mode 1) ;; Show spaces, tabs and other
  (ws-butler-mode 1) ;; Delete trailing spaces on changed lines
  (cond ;; Turn on spell-checking only in Linux
   ((string-equal system-type "gnu/linux")(flyspell-mode 1)))
  )
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'setup-markdown-mode)


;; MODE ICONS
;; https://github.com/ryuslash/mode-icons
(when window-system
  (straight-use-package 'mode-icons)
  (mode-icons-mode 1))


;; MONOKAI THEME
(straight-use-package 'monokai-theme)
(load-theme 'monokai t)


;; MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
(straight-use-package 'multiple-cursors)


;; ORG-MODE
;; https://orgmode.org/
(defun setup-org-mode ()
  "Settings for 'org-mode'."
  (interactive)
  (setq
   left-margin-width 4
   right-margin-width 4
   word-wrap t))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook #'setup-org-mode)


;; POWERLINE
;; https://github.com/milkypostman/powerline
(straight-use-package 'powerline)
(powerline-default-theme)


;; PROJECTILE-MODE
;; https://docs.projectile.mx/projectile/index.html
(straight-use-package 'projectile)
(defvar projectile-project-search-path)
(defvar projectile-mode-map)
(setq projectile-project-search-path '("~/repo/"))
(projectile-mode 1)
(define-key projectile-mode-map (kbd "S-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; PROTOBUF-MODE
(straight-use-package 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))


;; PYENV-MODE
(straight-use-package 'pyenv-mode)


;; PYTHON-MODE
(straight-use-package 'python-mode)
(defun setup-python-mode ()
  "Settings for 'python-mode'."
  (interactive)
  (company-mode 1)
  (elpy-mode 1)
  (flycheck-mode 1)
  (linum-mode 1)
  (pyenv-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'python-mode-hook #'setup-python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


;; RAINBOW DELIMITERS
;; https://github.com/Fanael/rainbow-delimiters
(straight-use-package 'rainbow-delimiters)
(rainbow-delimiters-mode 1)


;; RAINBOW MODE
;; http://elpa.gnu.org/packages/rainbow-mode.html
(straight-use-package 'rainbow-mode)


;; PAREN-MODE
(show-paren-mode 1)


;; SHELL-SCRIPT-MODE
(defun setup-shell-script-mode ()
  "Settings for 'shell-script-mode'."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (linum-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'shell-script-mode #'setup-shell-script-mode)


;; SQL MODE
(defun setup-sql-mode ()
  "Settings for SQL-mode."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (linum-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
(add-hook 'sql-mode-hook #'setup-sql-mode)


;; TERRAFORM-MODE
;; https://github.com/emacsorphanage/terraform-mode
;; !!! DO NOT USE ABANDONED HCL-MODE: https://github.com/syohex/emacs-hcl-mode !!!
(straight-use-package 'terraform-mode)
(defun setup-terraform-mode ()
  "Settings for terraform-mode."
  (interactive)
  (defvar flycheck-checker 'terraform)
  (setq flycheck-checker 'terraform)
  (company-mode 1)
  (flycheck-mode 1)
  (linum-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'setup-terraform-mode)


;; TIDE
;; https://github.com/ananthakumaran/tide
(straight-use-package 'tide)
(defun setup-tide-mode()
  "Settings for tide-mode."
  (interactive)
  (defvar tide-format-before-save)
  (setq
   tide-format-before-save t)

  (company-mode +1)
  (flycheck-mode +1)
  (linum-mode 1)
  (rainbow-delimiters-mode +1)
  (rainbow-mode +1)
  (tide-hl-identifier-mode +1)
  (tide-restart-server)
  (tide-setup))
(add-hook 'typescript-mode #'setup-tide-mode)
(add-hook 'js2-mode #'setup-tide-mode)


;; TREEMACS
(straight-use-package 'treemacs)
(global-set-key (kbd "<f8>") 'treemacs)
(with-eval-after-load 'treemacs
  (defvar treemacs-ignored-file-predicates)
  (defun treemacs-ignore-example (filename absolute-path)
    (or (string-equal filename "*\\__pycache__\\/\\'")
        (string-prefix-p "/x/y/z/" absolute-path)))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-example))


;; TREEMACS-ALL-THE-ICONS
(when window-system
  (straight-use-package 'treemacs-all-the-icons))


;; TREEMACS-DIRED
(when window-system
  (straight-use-package 'treemacs-icons-dired))


;;; TYPESCRIPT MODE
(straight-use-package 'typescript-mode)
(defun setup-typescript-mode ()
  "Settings for 'typescript-mode'."
  (interactive)
  (flycheck-mode 1)
  (linum-mode 1))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.d.ts\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'setup-typescript-mode)


;; WEB-BEAUTIFY
(straight-use-package 'web-beautify)


;; WEB-MODE
;; https://web-mode.org/
(straight-use-package 'web-mode)
(defun setup-web-mode()
  "Settings for web-mode."
  (defvar web-mode-attr-indent-offset)
  (defvar web-mode-css-indent-offset)
  (defvar web-mode-enable-block-face)
  (defvar web-mode-enable-css-colorization)
  (setq
   web-mode-attr-indent-offset 4
   web-mode-css-indent-offset 2 ;; CSS
   web-mode-enable-block-face t
   web-mode-enable-css-colorization t
   web-mode-enable-current-element-highlight t
   web-mode-markup-indent-offset 2)

  (company-mode)
  (emmet-mode 1)
  (flycheck-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1)) ;; HTML
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-hook 'web-mode-hook #'setup-web-mode)


;; WHITESPACE MODE
;; https://www.emacswiki.org/emacs/WhiteSpace
(straight-use-package 'whitespace-mode)
(defun setup-whitespace-mode ()
  "Settings for 'whitespace-mode'."
  (interactive)
  (setq-default whitespace-display-mappings
                '(
                  (space-mark   ?\    [?\xB7]  [?.]) ; space
                  (space-mark   ?\xA0 [?\xA4]  [?_]) ; hard space
                  (newline-mark ?\n   [?¶ ?\n] [?$ ?\n]) ; end of line
                  )
                ;; Highlight lines with length bigger than 1000 chars
                whitespace-line-column 1000
                whitespace-fill-column 1000
                ))
(add-hook 'whitespace-mode-hook #'setup-whitespace-mode)


;; WS-BUTLER-MODE (instead (add-to-list 'write-file-functions 'delete-trailing-whitespace))
;; https://github.com/lewang/ws-butler
(straight-use-package 'ws-butler)


;; YAML-MODE
;; https://github.com/yoshiki/yaml-mode
(straight-use-package 'yaml-mode)
(defun setup-yaml-mode ()
  "Settings for yaml-mode."
  (interactive)

  (company-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook #'setup-yaml-mode)

;;; init.el ends here
