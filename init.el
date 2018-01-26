;;; init.el --- Summary
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

(setq package-archives nil)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/") t)

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
  URL `http: // ergoemacs.org / emacs / emacs_new_empty_buffer.html'
  Version 2015 - 06 - 12"
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

(when(get-buffer "*scratch*")
  (kill-buffer "*scratch*"))

(fset 'yes-or-no-p 'y-or-n-p); ; ; Shortcuts for yea and no

;;; Format file before save
(defun format-current-buffer()
  (indent-region (point-min) (point-max)))
(defun untabify-current-buffer()
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max)))
  nil)
(add-to-list 'write-file-functions 'untabify-current-buffer)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)


;;; Save user settings in dedicated file
(setq custom-file "~/.emacs.d/settings.el")
(load-file "~/.emacs.d/settings.el")

(use-package airline-themes
  :requires powerline
  :config (load-theme 'airline-molokai))

(use-package all-the-icons)

(use-package anaconda-mode
  :hook ((python-mode-hook . anaconda-mode)
         (python-mode-hook . anaconda-eldoc-mode)))

(use-package beacon
  :diminish
  :commands beacon-mode)

(use-package company
  :diminish
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

  (eval-after-load "yasnippet"
    '(progn
       (defun company-mode/backend-with-yas (backend)
         (if (and (listp backend) (member 'company-yasnippet backend))
             backend
           (append (if (consp backend) backend (list backend))
                   '(:with company-yasnippet))))
       (setq company-backends
             (mapcar #'company-mode/backend-with-yas company-backends))))
  :init (global-company-mode t))

(use-package company-anaconda
  :requires anaconda-mode
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package company-quickhelp
  :bind
  (:map company-active-map
        ("C-c h" . company-quickhelp-manual-begin)))

(use-package css-mode
  :mode "\\.css\\'")

;; (use-package elpy
;;   :mode "\\.py\\'"
;;   :requires python-mode
;;   :hook (add-hook 'before-save-hook #'elpy-format-code)
;;   :init
;;   (elpy-enable)
;;   (defalias 'workon 'pyvenv-workon))

(use-package emmet-mode
  :mode  ("\\.html\\'" . emmet-mode)
  :bind
  ("C-j" . emmet-expand-line))

(use-package flycheck
  :commands flycheck-mode
  :init(add-hook 'after-init-hook #'global-flycheck-mode))

(use-package helm
  :bind ([f10] . helm-buffers-list))

(use-package highlight-numbers
  :hook(prog-mode . highlight-numbers-mode))

(use-package ibuffer
  :bind([f2] . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook #'(lambda ()(ibuffer-switch-to-saved-filter-groups "default"))))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :requires flycheck
  :bind(
        :map js2-mode-map
             ("M-n" . flycheck-next-error)
             ("M-p" . flycheck-previous-error))
  :config
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1)
  (js2-highlight-unused-variables-mode t))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.bowerrc\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)))

(use-package json-reformat
  :after json-mode)

(use-package magit
  :bind([f5] . magit-status))

(use-package markdown-mode
  :mode "\\.md\\'"
  :commands markdown-mode)

(use-package monokai-theme
  :config (load-theme 'monokai t))

(use-package neotree
  :requires all-the-icons
  :bind
  ([f8] . neotree-toggle))

(use-package persp-mode
  :init (persp-mode +1))

(use-package powerline)

(use-package py-isort
  :hook
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package python-mode
  :mode "\\.py\\'"
  :interpreter ("python" . python-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(scroll-bar-mode -1)

(use-package tide
  :ensure t
  :commands (tide-setup)
  :init
  (defun +tide-setup ()
    (interactive)
    (when (locate-dominating-file default-directory "tsfmt.json")
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    ;; Disable `flycheck' linting for Typescript Definition files.
    (when (and (buffer-file-name)
               (string-match-p ".d.ts$" (buffer-file-name)))
      (when (fboundp 'flycheck-mode)
        (flycheck-mode -1)))
    (tide-setup)
    (tide-hl-identifier-mode +1))
  (add-hook 'typescript-mode-hook #'+tide-setup)

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (or
                     (locate-dominating-file default-directory "tsconfig.json")
                     (locate-dominating-file default-directory "jsconfig.json"))
                (+tide-setup))))

  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up Tide mode if Typescript.
              (when (string-equal (file-name-extension buffer-file-name) "tsx")
                (setq-local web-mode-enable-auto-quoting nil)
                (when (fboundp 'yas-activate-extra-mode)
                  (yas-activate-extra-mode 'typescript-mode))
                (+tide-setup))))
  :config
  ;; Set up Typescript linting with `web-mode'.
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'typescript-tslint 'web-mode)))

;; (use-package tide
;;   :commands tide-setup tide-mode setup-tide-mode
;;   :config
;;   (defun setup-tide-mode ()
;;     (interactive)
;;     ;;(require 'typescript-mode)
;;     (tide-setup)
;;     (tide-mode +1)
;;     (flycheck-mode +1)
;;     (setq-default tab-width 2)
;;     (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;     (eldoc-mode +1)
;;     (tide-hl-identifier-mode +1)
;;     (company-mode +1))
;;   (require 'typescript-mode)
;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)

;;   ;; aligns annotation to the right hand side
;;   (setq company-tooltip-align-annotations t)
;;   ;; format options
;;   ;; (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;;   ;; support for JS files
;;   (add-hook 'js2-mode-hook #'setup-tide-mode)
;;   ;; from now on web-mode is required
;;   (require 'web-mode)
;;   ;; support for TSX files
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;                 (setup-tide-mode))))
;;   ;; support for JSX files
;;   (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (when (string-equal "jsx" (file-name-extension buffer-file-name))
;;                 (setup-tide-mode)))))

(use-package typescript-mode
  :commands typescript-mode)

(use-package web-beautify)

(use-package web-mode
  :commands web-mode
  :requires web-beautify
  :mode(("\\.phtml\\'" . web-mode)
        ("\\.html\\'" . web-mode))
  :hook (add-hook 'before-save-hook 'web-beautify-html-buffer t t))

(use-package yasnippet
  :after prog-mode
  :defer 10
  :diminish yas-minor-mode
  :mode("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-load-directory (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;;; init.el ends here
