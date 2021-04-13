;;; init.el --- Summary
;;; Commentary:
;;; Main EMACS settings file, load settings from parts.

;;; Code:

(defvar buffer-file-coding-system)
(defvar create-lockfiles)
(defvar emacs-config-dir
  (file-name-directory load-file-name) "Root directory with settings.")
(defvar inhibit-splash-screen)
(defvar inhibit-startup-message)
(defvar initial-major-mode)
(defvar initial-scratch-message)
(defvar make-backup-files)
(defvar savehist-file)
(defvar savehist-save-minibuffer-history)
(defvar savehistory-delete-duplicates)
(defvar savehistory-length)
(defvar truncate-lines)
(defvar user-full-name)

(setq buffer-file-coding-system 'utf8-auto-unix
      create-lockfiles nil
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message ""
      make-backup-files nil
      savehist-save-minibuffer-history 1
      savehistory-delete-duplicates t
      savehistory-length t
      truncate-lines 1
      user-full-name "Dunaevsky Maxim")


(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)


(fset 'yes-or-no-p 'y-or-n-p) ;;; Shortcuts for yes and no


(global-hl-line-mode 1) ;; Highlight current line


;; Resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Exit on Ctrl+Q
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)


(global-set-key (kbd "C-x o") 'next-multiframe-window)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)


;; Settings for window (not only a Windows!) system.
(defvar default-font-family nil "Default font family.")
(when (display-graphic-p)
  (fringe-mode 2)
  (scroll-bar-mode 0) ;; Off scrollbars
  (tool-bar-mode 0) ;; Off toolbar
  (tooltip-mode 0) ;; No windows for tooltip
  (window-divider-mode 0)
  (set-face-attribute 'default nil :height 110)

  ;; Font settings for Linux and Windows
  (cond
   (
    (string-equal system-type "windows-nt")
    (when (member "Consolas" (font-family-list))
      (setq default-font-family "Consolas")))
   (
    (string-equal system-type "gnu/linux")
    (when (member "DejaVu Sans Mono" (font-family-list))
      (setq default-font-family "DejaVu Sans Mono"))))

  (set-face-attribute 'default nil :family default-font-family))


;;; Save user settings in dedicated file
(setq custom-file (expand-file-name "settings.el" emacs-config-dir))
(load-file "~/.emacs.d/settings.el")


;; Auto-revert mode
(global-auto-revert-mode 1)


;; ENCODING
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8-auto-unix)


;; AUTO INSTALL STRAIGHT BOOTSTRAP
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
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


;; Buffers and windows
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "<f7>") 'xah-new-empty-buffer)


(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "M-4") 'split-window-horizontally)
(global-set-key (kbd "M-5") 'split-window-vertically)
(global-set-key (kbd "M-6") 'balance-windows)

;; Search and replace
;;(global-set-key (kbd "C-f") 'isearch-forward) → replaced with swiper
(global-set-key (kbd "C-h") 'query-replace)
(global-set-key (kbd "C-S-h") 'query-replace-regexp)

;; Sort lines
(global-set-key (kbd "<f9>") 'sort-lines)

;; Execute commands
(global-set-key (kbd "<esc>") 'keyboard-quit)


;; Switch windows with C-x and arrow keys
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)


(global-set-key (kbd "M--")
                (lambda ()
                  (interactive)
                  (insert "—"))) ;; Long dash by Alt+-

(global-unset-key (kbd "<insert>")) ;; Disable overwrite mode
(global-unset-key (kbd "M-,")) ;; Disable M-, as markers

(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))


;; AIRLINE THEMES
(straight-use-package 'airline-themes)


;; ALL THE ICONS
(when (display-graphic-p)
  (progn
    (straight-use-package 'all-the-icons)
    (cond
     ;; Install fonts in GNU / Linux
     (
      (string-equal system-type "gnu/linux")
      (unless
          (file-directory-p "~/.local/share/fonts/")
        (all-the-icons-install-fonts)))
     (
      ;; Not install fonts in Windows, but print message
      (string-equal system-type "windows-nt")
      (progn (message "Download and install fonts with all-the-icons-install-fonts command."))))))


;; ALL THE ICONS DIRED
;; https://github.com/jtbm37/all-the-icons-dired
(when (display-graphic-p)
  (defun setup-all-the-icons-dired-mode()
    "Settings for 'all-the-icons-dired-mode'."
    (interactive)
    (all-the-icons-dired-mode 1))
  (progn
    (straight-use-package 'all-the-icons-dired)
    (add-hook 'dired-mode-hook #'setup-all-the-icons-dired-mode)))


;; ALL THE ICONS IBUFFER
;; https://github.com/seagle0128/all-the-icons-ibuffer
(when (display-graphic-p)
  (progn
    (straight-use-package 'all-the-icons-ibuffer)
    (add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)))


;; ALL THE ICONS IVY
;; https://github.com/asok/all-the-icons-ivy
(straight-use-package 'all-the-icons-ivy)
(all-the-icons-ivy-setup)


;; ALL THE ICONS IVY RICH
;; https://github.com/seagle0128/all-the-icons-ivy-rich
(straight-use-package 'all-the-icons-ivy-rich)
(all-the-icons-ivy-rich-mode 1)


;; AUTO VIRTUALENVWRAPPER
(straight-use-package 'auto-virtualenvwrapper)
(add-hook 'python-mode-hook 'auto-virtualenvwrapper-activate)


;; BEACON
(straight-use-package 'beacon)
(beacon-mode 1)


;; COMPANY-MODE
;;https://company-mode.github.io/
(straight-use-package 'company)
(defun setup-company-mode ()
  "Settings for company-mode."
  (interactive)
  (defvar company-dabbrev-code-ignore-case nil)
  (defvar company-dabbrev-downcase nil)
  (defvar company-dabbrev-ignore-case nil)
  (defvar company-idle-delay 0)
  (defvar company-minimum-prefix-length 2)
  (defvar company-quickhelp-delay 1)
  (defvar company-tooltip-align-annotations t)

  (setq company-dabbrev-code-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-idle-delay 0
        company-minimum-prefix-length 2
        company-quickhelp-delay 3
        company-tooltip-align-annotations t))
(add-hook 'company-mode-hook #'setup-company-mode)


;; COMPANY-JEDI
;; https://github.com/company-mode/company-mode
(straight-use-package 'company-jedi)
(require 'company)
(add-to-list 'company-backends 'company-jedi)


;; COMPANY-TERRAFORM
(straight-use-package 'company-terraform)


;; COMPANY-QUICKHELP-MODE
;; https://github.com/company-mode/company-quickhelp
(straight-use-package 'company-quickhelp)
(company-quickhelp-mode 1)


;; CONF MODE FOR INI / CONF / LIST
(straight-use-package 'conf-mode)
(add-to-list 'auto-mode-alist '("\\.ini\\'" . conf-mode ))
(add-to-list 'auto-mode-alist '("\\.list\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.pylintrc\\'" . conf-mode))


;; COUNSEL
;; USE TOGETHER WITH IVY-MODE
(straight-use-package 'counsel)
(counsel-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)


;; CUA-MODE
;; Ctrl+X, Ctrl+V, Ctrl+Z and other Windows-like shortcuts.
(cua-mode 1)


;; DESKTOP-SAVE-MODE
(require 'desktop)
(setq desktop-modes-not-to-save '(dired-mode
                                  Info-mode
                                  info-lookup-mode))
(setq desktop-save 1)
(desktop-save-mode 1)


;; DIFF HL: highlight changes
;; https://github.com/dgutov/diff-hl
(straight-use-package 'diff-hl)
(global-diff-hl-mode 1)


;; DOCKERFILE-MODE
(straight-use-package 'dockerfile-mode)
(defun setup-dockerfile-mode ()
  "Settings for 'dockerfile-mode'."
  (company-mode 1)
  (flycheck-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("Dockerfile'" . dockerfile-mode))
(add-hook 'dockerfile-mode-hook #'setup-dockerfile-mode)


;; EDIT-INDIRECT-MODE
;; https://github.com/Fanael/edit-indirect/
;; Use C-c ' for editing source code examples in markdown
(straight-use-package 'edit-indirect)


;; EDITORCONFIG EMACS
;; https://github.com/editorconfig/editorconfig-emacs
(straight-use-package 'editorconfig)
(defun setup-editorconfig-mode ()
  "Settings for 'editor-config-mode'."
  (interactive)
  (defvar editorconfig-trim-whitespaces-mode)
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))
(add-hook 'editorconfig-mode-hook #'setup-editorconfig-mode)
(editorconfig-mode 1)


;; ELECTRIC-PAIR-MODE
;; EMBEDDED
(electric-pair-mode 1)
(defvar electric-pair-pairs)
(setq electric-pair-pairs
      '(
        (?\« . ?\»)
        (?\„ . ?\“)))


;; EMACS LISP MODE
;; IT IS NOT A ELISP-MODE!
(defun setup-emacs-lisp-mode ()
  "Settings for EMACS Lisp Mode."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'emacs-lisp-mode-hook #'setup-emacs-lisp-mode)
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))


;; FLYCHECK
(straight-use-package `flycheck)
(defun setup-flycheck-mode ()
  "Settings for 'flycheck-mode'."
  (interactive)
  (defvar flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc")
  (setq flycheck-check-syntax-automatically '(mode-enabled save new-line)
        flycheck-indication-mode 'left-margin
        flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc"))
(add-hook 'flycheck-mode-hook #'setup-flycheck-mode)


;; FLYCHECK-COLOR-MODE-LINE: highlight status line by flycheck state
;; https://github.com/flycheck/flycheck-color-mode-line
(straight-use-package 'flycheck-color-mode-line)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;; FLYCHECK INDICATOR
(straight-use-package 'flycheck-indicator)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook 'flycheck-indicator-mode))


;; FLYCHECK-POS-TIP
;; https://github.com/flycheck/flycheck-pos-tip
(when (display-graphic-p)
  (progn
    (straight-use-package 'flycheck-pos-tip)
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode))))


;; FORMAT ALL
;; https://github.com/lassik/emacs-format-all-the-code
(straight-use-package 'format-all)
(global-set-key (kbd "<f12>") 'format-all-buffer)


;; GO-MODE
;; https://github.com/dominikh/go-mode.el
(straight-use-package 'go-mode)
(defun setup-go-mode()
  "Settings for 'go-mode'."
  (interactive)
  (abbrev-mode 1)
  (buffer-face-mode 1)
  (flycheck-mode 1) ;; Turn on linters
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (smart-tabs-mode 1)
  (visual-line-mode 1) ;; Highlight current line
  (whitespace-mode 1) ;; Show spaces, tabs and other
  (ws-butler-mode 1)) ;; Delete trailing spaces on changed lines)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook #'setup-go-mode)


;; HELM
(straight-use-package 'helm)
(global-set-key (kbd "<f10>") 'helm-buffers-list)
(helm-mode 1)


;; HELM-COMPANY
(straight-use-package 'helm-company)
(define-key company-active-map (kbd "C-:") 'helm-company)


;; IBUFFER
(straight-use-package 'ibuffer)
(defalias 'list-buffers 'ibuffer)
(require 'ibuffer)
(require 'ibuf-ext)
(defun setup-ibuffer ()
  "Settings for 'ibuffer-mode'."
  (interactive)
  (setq ibuffer-expert 1
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
        ibuffer-show-empty-filter-groups nil ;; Do not show empty groups
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

  (hl-line-mode 1)
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))
(global-set-key (kbd "<f2>") 'ibuffer)
(add-to-list 'ibuffer-never-show-predicates "^\\*")
(add-hook 'ibuffer-mode-hook #'setup-ibuffer)


;; IVY-MODE
;; https://github.com/abo-abo/swiper#ivy
(straight-use-package 'ivy)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)


;; IVY RICH
;; https://github.com/Yevgnen/ivy-rich
(straight-use-package 'ivy-rich)
(ivy-rich-mode 1)


;; JSON-MODE
;; https://github.com/joshwnj/json-mode
(straight-use-package 'json-mode)
(defun setup-json-mode()
  "Settings for json-mode."
  (company-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.\\(?:json\\|bowerrc\\|jshintrc\\)\\'" . json-mode))
(add-hook 'json-mode-hook #'setup-json-mode)


;; LINUM MODE
(require 'linum)
(setq linum-format 'dynamic)


;; LSP-MODE
;; https://github.com/emacs-lsp/lsp-mode
(straight-use-package 'lsp-mode)
(add-hook 'python-mode-hook 'lsp)


;; LSP-JEDI
(straight-use-package 'lsp-jedi)


;; LSP UI
(straight-use-package 'lsp-ui)


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
  (setq header-line-format " "
        left-margin-width 4
        line-spacing 3
        right-margin-width 4
        word-wrap t)

  ;; Additional modes
  (abbrev-mode 1)
  (buffer-face-mode 1)
  (flycheck-mode 1) ;; Turn on linters
  (hl-line-mode 1)
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (visual-line-mode 1) ;; Highlight current line
  (whitespace-mode 1) ;; Show spaces, tabs and other
  (ws-butler-mode 1) ;; Delete trailing spaces on changed lines
  (cond ;; Turn on spell-checking only in Linux
   ((string-equal system-type "gnu/linux")(flyspell-mode 1)))

  (set-face-attribute 'markdown-code-face        nil :family default-font-family)
  (set-face-attribute 'markdown-inline-code-face nil :family default-font-family))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'setup-markdown-mode)


;; Turn off menu bar
(menu-bar-mode 0)


;; MODE ICONS
;; https://github.com/ryuslash/mode-icons
(when (display-graphic-p)
  (progn
    (straight-use-package 'mode-icons)
    (mode-icons-mode 1)))


;; MONOKAI THEME
(straight-use-package 'monokai-theme)
(load-theme 'monokai t)
(require 'airline-themes)
(load-theme 'airline-doom-molokai t)


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


;; OVERWRITE-MODE
(overwrite-mode nil) ;; Disable overwrite mode


;; PHP-MODE
(straight-use-package 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))


;; POWERLINE
;; https://github.com/milkypostman/powerline
(straight-use-package 'powerline)


;; PROJECTILE-MODE
;; https://docs.projectile.mx/projectile/index.html
(straight-use-package 'projectile)
(require 'projectile)
(setq projectile-project-search-path '("~/repo/"))
(define-key projectile-mode-map (kbd "S-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode 1)


;; PROTOBUF-MODE
;; https://github.com/emacsmirror/protobuf-mode
(straight-use-package 'protobuf-mode)
(defun setup-protobuf-mode ()
  "Settings for 'protobuf-mode'."
  (interactive)

  (company-mode 1)
  (flycheck-mode 1)
  (hl-line-mode 1)
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'ptotobuf-mode-hook #'setup-protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))


;; PYTHON-MODE
(straight-use-package 'python-mode)
(defun setup-python-mode ()
  "Settings for 'python-mode'."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (hl-line-mode 1)
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'python-mode-hook #'setup-python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


;; RAINBOW DELIMITERS
;; https://github.com/Fanael/rainbow-delimiters
(straight-use-package 'rainbow-delimiters)


;; RAINBOW MODE
;; http://elpa.gnu.org/packages/rainbow-mode.html
(straight-use-package 'rainbow-mode)


;; RST-MODE
(defun setup-rst-mode ()
  "Settings for 'rst-mode'."
  (interactive)

  (flycheck-mode 1)
  (hl-line-mode 1)
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'rst-mode-hook #'setup-rst-mode)
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))


;; RUBY-MODE
(straight-use-package 'ruby-mode)
(defun setup-ruby-mode ()
  "Settings for 'ruby-mode'."
  (interactive)

  (company-mode 1)
  (flycheck-mode 1)
  (hl-line-mode 1)
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'ruby-mode-hook #'setup-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb\\'" .ruby-mode))


;; SAVE MINIBUFFER HISTORY
(require 'savehist)
(setq savehist-dir (expand-file-name "history.d" emacs-config-dir)
      savehist-file (expand-file-name "minibuffer-history" savehist-dir))
(unless (file-exists-p savehist-dir)
  (make-directory savehist-dir))
(savehist-mode 1)


;; PAREN-MODE
(show-paren-mode 1)


;; SAVE-PLACE-MODE
;; https://www.emacswiki.org/emacs/SavePlace
;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file.
(defvar save-place-dir (expand-file-name "places" emacs-config-dir))
(unless (file-exists-p save-place-dir)
  (make-directory save-place-dir))
(setq save-place-file (expand-file-name ".emacs-places" save-place-dir)
      save-place-forget-unreadable-files 1)
(save-place-mode 1)


;; SCALA MODE
(straight-use-package 'scala-mode)
(defun setup-scala-mode ()
  "Settings for 'scala-mode'."
  (interactive)
  (company-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'scala-mode-hook #'setup-scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))


;; SHELL-SCRIPT-MODE
(defun setup-shell-script-mode ()
  "Settings for 'shell-script-mode'."
  (interactive)

  (company-mode 1)
  (flycheck-mode 1)
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (smart-tabs-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'shell-script-mode #'setup-shell-script-mode)
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))


;; SMART-TABS-MODE
;; https://www.emacswiki.org/emacs/SmartTabs
(straight-use-package 'smart-tabs-mode)


;; SQL MODE
(defun setup-sql-mode ()
  "Settings for SQL-mode."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (linum-mode 1)
  (rainbow-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
(add-hook 'sql-mode-hook #'setup-sql-mode)


;; SWIPER
;; https://github.com/abo-abo/swiper}
(straight-use-package 'swiper)
(global-set-key (kbd "C-f") 'swiper-isearch)


;; TIDE-MODE
;; https://github.com/ananthakumaran/tide/
(straight-use-package 'tide)
(defun setup-tide-mode ()
  "Settings for 'tide-mode'."
  (interactive)
  (company-mode 1)
  (eldoc-mode 1)
  (flycheck-mode 1)
  (tide-hl-identifier-mode 1)
  (tide-setup)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'tide-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tide-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tide-mode))


;; TERRAFORM-MODE
;; https://github.com/emacsorphanage/terraform-mode
;; !!! DO NOT USE ABANDONED HCL-MODE: https://github.com/syohex/emacs-hcl-mode !!!
(straight-use-package 'terraform-mode)
(require 'terraform-mode)
(defun setup-terraform-mode ()
  "Settings for terraform-mode."
  (interactive)
  (setq flycheck-checker 'terraform)
  (company-mode 1)
  (flycheck-mode 1)
  (hl-line-mode 1)
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'setup-terraform-mode)


;; TREEMACS — awesome file manager (instead NeoTree)
;; https://github.com/Alexander-Miller/treemacs
(straight-use-package 'treemacs)
(global-set-key (kbd "<f8>") 'treemacs)
(eval-after-load 'treemacs '
  (progn
    (define-key treemacs-mode-map (kbd "C-<f8>") 'treemacs-switch-workspace)
    (define-key treemacs-mode-map (kbd "f") 'find-grep)))

(with-eval-after-load 'treemacs
  (defun treemacs-get-ignore-files (filename absolute-path)
    (or
     (string-equal filename ".emacs.desktop.lock")
     (string-equal filename "__pycache__")))
  (defvar treemacs-ignored-file-predicates)
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-get-ignore-files))


;; TREEMACS-ALL-THE-ICONS
(when (display-graphic-p)
  (straight-use-package 'treemacs-all-the-icons))


;; TREEMACS-DIRED
(when (display-graphic-p)
  (straight-use-package 'treemacs-icons-dired))


;; TYPESCRIPT MODE
;; https://github.com/emacs-typescript/typescript.el
(straight-use-package 'typescript-mode)
(defun setup-typescript-mode ()
  "Settings for 'typescript-mode'."
  (interactive)
  (flycheck-mode 1)
  (linum-mode 1))
(add-to-list 'auto-mode-alist '("\\.d.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'setup-typescript-mode)


;; UNDO-TREE
(straight-use-package 'undo-tree)
(global-undo-tree-mode 1)


;; WEB-BEAUTIFY
(straight-use-package 'web-beautify)


;; WEB-MODE
;; https://web-mode.org/
(straight-use-package 'web-mode)
(defun setup-web-mode()
  "Settings for web-mode."
  (setq web-mode-attr-indent-offset 4
        web-mode-css-indent-offset 2 ;; CSS
        web-mode-enable-block-face t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2)

  (company-mode 1)
  (flycheck-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-hook 'web-mode-hook #'setup-web-mode)


;; WHICH-KEY MODE
;; https://github.com/justbur/emacs-which-key
(straight-use-package 'which-key)
(which-key-mode 1)


;; WHITESPACE MODE
;; https://www.emacswiki.org/emacs/WhiteSpace
(straight-use-package 'whitespace-mode)
(defun setup-whitespace-mode ()
  "Settings for 'whitespace-mode'."
  (interactive)

  (setq whitespace-display-mappings
        '(
          (space-mark   ?\    [?\xB7]     [?.]) ; space
          (space-mark   ?\xA0 [?\xA4]     [?_]) ; hard space
          (newline-mark ?\n   [?¶ ?\n]    [?$ ?\n]) ; end of line
          (tab-mark     ?\t   [?\xBB ?\t] [?\\ ?\t]) ; tab
          )
        ;; Highlight lines with length bigger than 1000 chars
        whitespace-fill-column 1000)

  ;; Markdown-mode hack
  (set-face-attribute 'whitespace-space nil
                      :family default-font-family
                      :foreground "#75715E")
  (set-face-attribute 'whitespace-indentation nil
                      :family default-font-family
                      :foreground "#E6DB74"))
(add-hook 'whitespace-mode-hook #'setup-whitespace-mode)


;; WS-BUTLER-MODE
;; DO NOT USE: (add-to-list 'write-file-functions 'delete-trailing-whitespace))!
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
  (hl-line-mode 1)
  (linum-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook #'setup-yaml-mode)

;;; init.el ends here
