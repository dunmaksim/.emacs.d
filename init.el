;;; Summary
;;; Commentary:
;;; Main EMACS settings file, load settings from parts.

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(eval-when-compile(require 'cl))

(require 'package)
(cua-mode t)

(setq package-archives nil)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize nil)

;; ; Load and install use-package if required
(unless(package-installed-p 'use-package)
  (message "EMACS install use-package.el")
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(set-face-attribute 'default nil :height 110)
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

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

;; Show line numbers everywhere
(global-linum-mode t)

;; Resize windows
(global-set-key(kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key(kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key(kbd "S-C-<down>") 'shrink-window)
(global-set-key(kbd "S-C-<up>") 'enlarge-window)

;; Isearch
(global-set-key (kbd "C-M-r") 'isearch-backward-other-window)
(global-set-key (kbd "C-M-s") 'isearch-forward-other-window)

(defun xah-new-empty-buffer()
  "Open a new empty buffer.
  URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
  Version 2015-06-12"
  (interactive)
  (let((ξbuf(generate-new-buffer "untitled")))
    (switch-to-buffer ξbuf)
    (funcall(and initial-major-mode))
    (setq buffer-offer-save t)))

;; Save/close/open
(global-set-key(kbd "C-w") 'kill-this-buffer)
(global-set-key(kbd "C-s") 'save-buffer)
(global-set-key(kbd "C-S-s") 'write-file)
(global-set-key(kbd "C-r") 'revert-buffer)
(global-set-key(kbd "C-a") 'mark-whole-buffer)
(global-set-key(kbd "M-'") 'comment-or-uncomment-region)
(global-set-key(kbd "C-o") 'dired)
(global-set-key(kbd "C-n") 'xah-new-empty-buffer)
(global-set-key(kbd "C-+") 'text-scale-increase)
(global-set-key(kbd "C--") 'text-scale-decrease)

;; Buffers and windows
(global-set-key(kbd "C-<next>") 'next-buffer)
(global-set-key(kbd "C-<prior>") 'previous-buffer)
(global-set-key(kbd "C-<tab>") 'other-window)

(global-set-key(kbd "M-3") 'delete-other-windows)
(global-set-key(kbd "M-4") 'split-window-horizontally)
(global-set-key(kbd "M-5") 'split-window-vertically)
(global-set-key(kbd "M-6") 'balance-windows)

(global-set-key(kbd "C-f") 'isearch-forward)
(global-set-key(kbd "C-h") 'query-replace)
(global-set-key(kbd "C-S-h") 'query-replace-regexp)

(global-set-key(kbd "M-a") 'execute-extended-command)
(global-set-key(kbd "M-x") 'kill-whole-line)
(global-set-key(kbd "<esc>") 'keyboard-quit)

(global-unset-key (kbd "<insert>")) ;; Disable overwrite mode
(global-unset-key (kbd "M-,")) ;; Disable M-, as markers

(when(get-buffer "*scratch*")
  (kill-buffer "*scratch*"))

(fset 'yes-or-no-p 'y-or-n-p); ; ; Shortcuts for yes and no

;;; Format file before save
(defun format-current-buffer()
  (indent-region (point-min) (point-max)))


(add-to-list 'write-file-functions 'delete-trailing-whitespace)


;;; Save user settings in dedicated file
(setq custom-file "~/.emacs.d/settings.el")
(load-file "~/.emacs.d/settings.el")

(use-package airline-themes
  :requires powerline
  :config (load-theme 'airline-molokai))

(use-package all-the-icons
  :config
  (unless (file-directory-p "~/.local/share/fonts/") (all-the-icons-install-fonts)))


(use-package all-the-icons-dired
  ;;; Show icons in the dired mode
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  ;;; Show icons in the ibuffer mode
  :requires all-the-icons
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package auto-virtualenvwrapper
  :hook (python-mode . auto-virtualenvwrapper-activate))

(use-package py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package beacon
  :commands beacon-mode)

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package clang-format)

(use-package company
  :commands company-mode
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  (defun check-expansion ()
    (save-excursion
      (if (outline-on-heading-p t)
          nil
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil))))))

  (define-key company-mode-map [tab]
    '(menu-item "maybe-company-expand" nil
                :filter (lambda (&optional _)
                          (when (check-expansion)
                            #'company-complete-common))))
  ;; :init
  (global-company-mode t))

(use-package company-c-headers
  :config (add-to-list 'company-backends 'company-c-headers))

(use-package company-quickhelp
  :bind
  (:map company-active-map
        ("C-c h" . company-quickhelp-manual-begin)))

(use-package css-mode
  :mode "\\.css\\'")

(use-package highlight-doxygen
  :config(highlight-doxygen-global-mode 1))

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package elp)

(use-package elpy
  :ensure t
  :init (elpy-enable))

(use-package emmet-mode
  :mode  ("\\.html\\'" . emmet-mode)
  :bind
  ("C-j" . emmet-expand-line))

(use-package flycheck
  :commands flycheck-mode
  :init(add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package flycheck-indicator
  :ensure t
  :after flycheck
  :hook ('flycheck-mode-hook 'flycheck-indicator-mode))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config (flycheck-pos-tip-mode))

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :config (flycheck-rust-setup))

(use-package format-all)

(use-package helm
  :bind ([f10] . helm-buffers-list))

(use-package helm-company
  :bind
  (:map company-mode-map ("C-:" . helm-company))
  (:map company-active-map ("C-:" . helm-company)))

(use-package highlight-numbers
  :init(add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package ibuffer
  :bind([f2] . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook #'(lambda ()(ibuffer-switch-to-saved-filter-groups "default"))))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.bowerrc\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)))

(use-package json-reformat
  :requires json-mode)


;;;;; LSP MODE

(use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
           (python-mode . lsp)
           (javascript-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)


(use-package lsp-ui :commands lsp-ui-mode)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

;;;;; LSP MODE

(use-package magit
  :bind([f5] . magit-status))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package mode-icons
  :config (mode-icons-mode))

(use-package monokai-theme
  :config (load-theme 'monokai t))

(use-package neotree
  :requires all-the-icons
  :bind
  ([f8] . neotree-toggle))

(overwrite-mode nil)

(use-package persp-mode
  :init (persp-mode +1))

(use-package powerline)

;; (use-package prettier-js
;;   :hook
;;   (js2-mode . prettier-js-mode))

(use-package pyenv-mode)

(use-package python-mode
  :mode ("\\.py\\'" . python-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (show-paren-mode 1)
  (electric-pair-mode 1))

(scroll-bar-mode -1)


(use-package tide
  ;;; From https://github.com/ananthakumaran/tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.d.ts\\'" . typescript-mode))

(use-package web-beautify)

(use-package web-mode
  :commands web-mode
  :mode(("\\.phtml\\'" . web-mode)
        ("\\.html\\'" . web-mode))
  :hook (add-hook 'before-save-hook 'web-beautify-html-buffer t t))

(use-package winum)

(use-package yasnippet
  :requires prog-mode
  :defer 10
  :mode("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  ;; Create snippets directory, if does not exists
  (if (not (file-directory-p "~/.emacs.d/snippets/"))
      (make-directory "~/.emacs.d/snippets/"))
  (yas-load-directory (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :init
  ;; Checking existing 'snippets' dir. Required for yasnippet.
  (unless (file-directory-p "~/.emacs.d/snippets/") (mkdir "~/.emacs.d/snippets/"))
  :requires yasnippet)

;;; init.el ends here
