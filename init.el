;;; init.el --- Summary
;;; Commentary:
;;; Main EMACS settings file, load settings from parts.

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p) ;;; Shortcuts for yes and no
(cua-mode t) ;;; Ctrl+C, Ctrl+V like Windows
(set-face-attribute 'default nil :height 110)
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; Show line numbers everywhere
(global-linum-mode t)
(global-hl-line-mode t)
(overwrite-mode nil)

;; Resize windows
(global-set-key(kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key(kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key(kbd "S-C-<down>") 'shrink-window)
(global-set-key(kbd "S-C-<up>") 'enlarge-window)

(when window-system
  (blink-cursor-mode 0)
  (fringe-mode 2)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (window-divider-mode 0))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

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


;; STRAIGHT

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


; Isearch
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

(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))

;; (defun format-current-buffer()
;;   ;;; Format file before save
;;   (indent-region (point-min) (point-max)))

(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;;; Save user settings in dedicated file
(setq custom-file "~/.emacs.d/settings.el")
(load-file "~/.emacs.d/settings.el")


;;;; AIRLINE THEMES
(straight-use-package 'airline-themes)


;;;; ALL THE ICONS
(straight-use-package 'all-the-icons)
(unless (file-directory-p "~/.local/share/fonts/") (all-the-icons-install-fonts))


;;;; ALL THE ICONS DIRED
(straight-use-package 'all-the-icons-dired)
;;;;(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(with-eval-after-load 'all-the-icons (all-the-icons-dired-mode))

;;;; ALL THE ICONS IBUFFER
;;;; https://github.com/seagle0128/all-the-icons-ibuffer
(straight-use-package 'all-the-icons-ibuffer)
(with-eval-after-load 'all-the-icons ibuffer (all-the-icons-ibuffer-mode))
(all-the-icons-ibuffer-mode 1)


;;;; AUTO VIRTUALENVWRAPPER
(straight-use-package 'auto-virtualenvwrapper)
(add-hook 'python-mode-hook 'auto-virtualenvwrapper-activate)


;;;; BEACON
(straight-use-package 'beacon)
(beacon-mode 1)


;;;; COMPANY
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;(use-package company
;;  :commands company-mode
;;  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
;;  (defadvice company-pseudo-tooltip-unless-just-one-frontend
;;      (around only-show-tooltip-when-invoked activate)
;;    (when (company-explicit-action-p)
;;      ad-do-it))

;;  (defun check-expansion ()
;;    (save-excursion
;;      (if (outline-on-heading-p t)
;;          nil
;;        (if (looking-at "\\_>") t
;;          (backward-char 1)
;;          (if (looking-at "\\.") t
;;            (backward-char 1)
;;            (if (looking-at "->") t nil))))))

;;  (define-key company-mode-map [tab]
;;    '(menu-item "maybe-company-expand" nil
;;                :filter (lambda (&optional _)
;;                          (when (check-expansion)
;;                            #'company-complete-common))))
;;  ;; :init
;;  (global-company-mode t))


;;;; COMPANY QUICKHELP
(straight-use-package 'company-quickhelp)
;;(use-package company-quickhelp
;;  :bind
;;  (:map company-active-map
;;        ("C-c h" . company-quickhelp-manual-begin)))

;;;; CSS-MODE
(straight-use-package 'css-mode)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))


;;(use-package highlight-doxygen
;;  :config(highlight-doxygen-global-mode 1))

;;(use-package edit-server
;;  :if window-system
;;  :init
;;  (add-hook 'after-init-hook 'server-start t)
;;  (add-hook 'after-init-hook 'edit-server-start t))

(straight-use-package 'elpy)
(elpy-enable)


;;;; EMMET
(straight-use-package 'emmet-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . emmet-mode))


;;;; FLYCHECK
(straight-use-package `flycheck)
(global-flycheck-mode 1)


;;;; FLYCHECK INDICATOR
(straight-use-package 'flycheck-indicator)
(add-hook 'flycheck-mode-hook 'flycheck-indicator-mode)

;;;; FLYCHECK-POS-TIP
;;;; https://github.com/flycheck/flycheck-pos-tip
(straight-use-package 'flycheck-pos-tip)
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))


;;;; FORMAT ALL
(straight-use-package 'format-all)


;;;; HELM
(straight-use-package 'helm)
;;(use-package helm
;;  :bind ([f10] . helm-buffers-list))

;;(use-package helm-company
;;  :bind
;;  (:map company-mode-map ("C-:" . helm-company))
;;  (:map company-active-map ("C-:" . helm-company)))


;;;; HIGHLIGHT NUMBERS
(straight-use-package 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)


;;;; IBUFFER
(straight-use-package 'ibuffer)
(global-set-key (kbd "<f2>") 'ibuffer)
(add-hook 'ibuffer-mode-hook #'(lambda ()(ibuffer-switch-to-saved-filter-groups "default")))


;;;; JSON-MODE
(straight-use-package 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.bowerrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc\\'" . json-mode))

;;(use-package json-reformat)





;;;;; LSP MODE
(straight-use-package 'lsp-mode)
(add-hook 'python-mode 'lsp)
(add-hook 'javacript-mode 'lsp)


;;(use-package lsp-mode
;;  :pin melpa-stable
;;  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;         (python-mode . lsp)
;;         (javascript-mode . lsp)
;;         ;; if you want which-key integration
;;         (lsp-mode . lsp-enable-which-key-integration))
;;  :commands lsp)


;;(use-package lsp-ui :commands lsp-ui-mode)

;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;;(use-package lsp-jedi
;;  :ensure t
;;  :pin melpa-stable
;;  :config
;;  (with-eval-after-load "lsp-mode"
;;    (add-to-list 'lsp-disabled-clients 'pyls)
;;    (add-to-list 'lsp-enabled-clients 'jedi)))

;;;;; LSP MODE

(straight-use-package 'magit)
(global-set-key (kbd"<f5>") 'magit-status)


;;;; MARKDOWN MODE
(straight-use-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;;; MODE ICONS
(straight-use-package 'mode-icons)
(mode-icons-mode t)


;;;; MONOKAI THEME
(straight-use-package 'monokai-theme)
(load-theme 'monokai t)


;;;; NEOTREE
(straight-use-package 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)


;;;; PERSP-MODE
(straight-use-package 'persp-mode)
(persp-mode +1)

;;;; POWERLINE
(straight-use-package 'powerline)

(straight-use-package 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)


;;;; PYENV-MODE
(straight-use-package 'pyenv-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


;;;; RAINBOW DELIMITERS
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(show-paren-mode 1)
(electric-pair-mode 1)


(straight-use-package 'tide)
(defun setup-tide-mode()
  (tide-setup)
  (tide-hl-identifier-mode))
(add-hook 'typescript-mode 'setup-tide-mode)
(add-hook 'typescript-mode
          (function (lambda ()(add-hook 'before-save-hook 'tide-format-before-save))))

(straight-use-package 'typescript-mode)
(add-to-list 'auto-mode-alist
             '("\\.ts\\'" . typescript-mode)
             '("\\.d.ts\\'" . typescript-mode))

(straight-use-package 'web-beautify)

;;(leaf web-mode
;;  :commands web-mode
;;  :mode(("\\.phtml\\'" . web-mode)
;;        ("\\.html\\'" . web-mode))
;;  :hook (add-hook 'before-save-hook 'web-beautify-html-buffer t t))

;;; init.el ends here
