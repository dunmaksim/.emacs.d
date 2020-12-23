;;; init.el --- Summary
;;; Commentary:
;;; Main EMACS settings file, load settings from parts.

;;; Code:


(setq
 inhibit-startup-message t ;; No startup message
 initial-scratch-message "" ;; No scratch message
 initial-major-mode 'fundamental-mode ;; fundamental-mode by default
 inhibit-splash-screen t
 truncate-lines 1 ;; Wrap lines everywhere
 ) ;; disable splash screen

(fset 'yes-or-no-p 'y-or-n-p) ;;; Shortcuts for yes and no
(set-face-attribute 'default nil :height 120)

;; Font settings for Linux and Windows
(cond
 (
  (string-equal system-type "windows-nt")
  (when (member "Consolas" (font-family-list))
    (set-face-attribute 'default nil :font "Consolas")))
 (
  (string-equal system-type "gnu/linux")
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-face-attribute 'default nil :font "DejaVu Sans Mono"))))


(global-linum-mode t) ;; Show line numbers everywhere
(global-hl-line-mode t) ;; Highlight current line
(overwrite-mode nil) ;; Disable overwrite mode

;; Resize windows
(global-set-key(kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key(kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key(kbd "S-C-<down>") 'shrink-window)
(global-set-key(kbd "S-C-<up>") 'enlarge-window)

(when window-system
  (blink-cursor-mode 0)
  (fringe-mode 2)
  (scroll-bar-mode 0) ;; Off scrollbars
  (menu-bar-mode 0) ;; Off menu
  (tool-bar-mode 0) ;; Off toolbar
  (tooltip-mode 0) ;; No windows for tooltip
  (window-divider-mode 0)
  )

;;; Save user settings in dedicated file
(setq custom-file "~/.emacs.d/settings.el")
(load-file "~/.emacs.d/settings.el")

;; ENCODING
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; AUTO TRUNCATE LINES
(setq-default truncate-lines t)

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
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)
(global-set-key (kbd "C-r") 'revert-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-'") 'comment-or-uncomment-region)
(global-set-key (kbd "C-o") 'dired)
(global-set-key (kbd "C-n") 'xah-new-empty-buffer)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Buffers and windows
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "<f7>") 'other-window)

(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "M-4") 'split-window-horizontally)
(global-set-key (kbd "M-5") 'split-window-vertically)
(global-set-key (kbd "M-6") 'balance-windows)

;; Search and replace
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-h") 'query-replace)
(global-set-key (kbd "C-S-h") 'query-replace-regexp)

(global-set-key (kbd "M-a") 'execute-extended-command)
(global-set-key (kbd "M-x") 'kill-whole-line)
(global-set-key (kbd "<esc>") 'keyboard-quit)

(global-set-key (kbd "M--")(lambda()(interactive)(insert "—")))

(global-unset-key (kbd "<insert>")) ;; Disable overwrite mode
(global-unset-key (kbd "M-,")) ;; Disable M-, as markers

(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))

(add-to-list 'write-file-functions 'delete-trailing-whitespace)


;; AIRLINE THEMES
(straight-use-package 'airline-themes)


;; ALL THE ICONS
(straight-use-package 'all-the-icons)
(cond
 ;; Install fonts in GNU / Linux
 ((string-equal system-type "gnu/linux")(unless (file-directory-p "~/.local/share/fonts/") (all-the-icons-install-fonts)))
 ((string-equal system-type "windows-nt")(progn (message "Download and install fonts with all-the-icons-install-fonts command."))))


;; ALL THE ICONS DIRED
;; https://github.com/jtbm37/all-the-icons-dired
(straight-use-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; ALL THE ICONS IBUFFER
;; https://github.com/seagle0128/all-the-icons-ibuffer
(straight-use-package 'all-the-icons-ibuffer)


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
  (setq
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case nil
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-quickhelp-delay 1
   company-tooltip-align-annotations t)
  (global-company-mode t))
(add-hook 'after-init-hook 'customize-company-mode-hook)


;; Будем читать разные файлы как INI / CONF
(straight-use-package 'conf-mode)
(add-to-list 'auto-mode-alist '("\\.pylintrc\\'" . conf-mode))


;; CUA-MODE
(cua-mode t) ;;; Ctrl+C, Ctrl+V like Windows


;; ELECTRIC-PAIR-MODE
(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\« . ?\»)
        (?\„ . ?\“)))


;; ELISP MODE
(defun setup-elisp-mode ()
  "Settings for EMACS Lisp Mode."
  (interactive)
  (rainbow-delimiters-mode 1))
(add-hook 'emacs-lisp-mode-hook #'setup-elisp-mode)
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))

;; ELPY
;; https://elpy.readthedocs.io/
(straight-use-package 'elpy)
(elpy-enable)
;; Автоформат кода при сохранении файла
(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'elpy-format-code nil t)))
(remove-hook 'elpy-modules 'elpy-module-flymake)
;; Отключить старый flymake, включить flycheck
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; Псевдоним для pyvenv-workon: workon
(defalias 'workon 'pyvenv-workon)


;;;; EMMET
(straight-use-package 'emmet-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . emmet-mode))


;;;; FLYCHECK
(straight-use-package `flycheck)
(global-flycheck-mode 1)


;;;; FLYCHECK INDICATOR
(straight-use-package 'flycheck-indicator)
(add-hook 'flycheck-mode-hook 'flycheck-indicator-mode)


;; FLYCHECK-POS-TIP
;; https://github.com/flycheck/flycheck-pos-tip
(straight-use-package 'flycheck-pos-tip)
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))


;; FORMAT ALL
;; https://github.com/lassik/emacs-format-all-the-code
(straight-use-package 'format-all)


;;;; HELM
(straight-use-package 'helm)
(global-set-key (kbd "<f10>") 'helm-buffers-list)


;;;; HELM-COMPANT
(straight-use-package 'helm-company)
(define-key company-active-map (kbd "C-:") 'helm-company)


;;;; HIGHLIGHT NUMBERS
(straight-use-package 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)


;;;; IBUFFER
(straight-use-package 'ibuffer)
(defun setup-ibuffer ()
  "Settings for ibuffer mode."
  (interactive)
  (setq
   ibuffer-hidden-filter-groups (list "Helm" "*Internal*")
   ibuffer-maybe-show-regexps nil
   ibuffer-saved-filter-groups (quote
                                (("default"
                                  ("Dired"
                                   (mode . dired-mode))
                                  ("Python"
                                   (or
                                    (mode . python-mode)
                                    (mode . elpy-mode)))
                                  ("Web"
                                   (or
                                    (mode . web-mode)))
                                  ("DOCS"
                                   (or
                                    (mode . markdown-mode)
                                    (mode . yaml-mode)))
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
                                  ("Lisp"
                                   (mode . emacs-lisp-mode))
                                  ("Emacs"
                                   (or
                                    (name . "^\\*scratch\\*$")
                                    (name . "^\\*Messages\\*$")
                                    (name . "^\\*\\(Customize\\|Help\\)")
                                    (name . "\\*\\(Echo\\|Minibuf\\)"))))))
   ibuffer-shrink-to-minimum-size t
   ibuffer-use-other-window nil)
  (ibuffer-switch-to-saved-filter-groups "default")
  (ibuffer-update nil)
  (all-the-icons-ibuffer-mode 1))
(global-set-key (kbd "<f2>") 'ibuffer)
(add-hook 'ibuffer-mode-hook #'setup-ibuffer)


;; JSON-MODE
;; https://github.com/joshwnj/json-mode
(straight-use-package 'json-mode)
(defun setup-json-mode()
  "Settings for json-mode."
  (rainbow-delimiters-mode +1)
  (flycheck-mode 1)
  )
(add-to-list 'auto-mode-alist '("\\.\\(?:json\\|bowerrc\\|jshintrc\\)\\'" . json-mode))
(add-hook 'json-mode-hook #'setup-json-mode)

;; LSP MODE
(straight-use-package 'lsp-mode)
(add-hook 'python-mode 'lsp)
(add-hook 'javacript-mode 'lsp)


;; MAGIT
;; https://magit.vc/
(straight-use-package 'magit)
(global-set-key (kbd"<f5>") 'magit-status)


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
   word-wrap t
   )
  ;; Additional modes
  (abbrev-mode 1)
  (buffer-face-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode-enable)
  (visual-line-mode 1)
  (cond
   ((string-equal system-type "gnu/linux")(flyspell-mode 1))))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'setup-markdown-mode)


;; MODE ICONS
;; https://github.com/ryuslash/mode-icons
(straight-use-package 'mode-icons)
(mode-icons-mode t)


;; MONOKAI THEME
(straight-use-package 'monokai-theme)
(load-theme 'monokai t)


;; ORG-MODE
;; https://orgmode.org/
(defun setup-org-mode ()
  "Settings for org-mode."
  (interactive)
  (setq
   left-margin-width 4
   right-margin-width 4
   word-wrap t)
)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook #'setup-org-mode)


;; POWERLINE
;; https://github.com/milkypostman/powerline
(straight-use-package 'powerline)
(powerline-default-theme)


;; PYENV-MODE
(straight-use-package 'pyenv-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


;; RAINBOW DELIMITERS
;; https://github.com/Fanael/rainbow-delimiters
(straight-use-package 'rainbow-delimiters)
(rainbow-delimiters-mode 1)


;; PAREN-MODE
(show-paren-mode 1)


;; TIDE
;; https://github.com/ananthakumaran/tide
(straight-use-package 'tide)
(defun setup-tide-mode()
  "Settings for tide-mode.el."
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1)
  (rainbow-delimiters-mode +1)
  (tide-restart-server)
  (flycheck-mode +1)
  (company-mode +1)
  (setq
   tide-format-before-save t
   company-tooltip-align-annotations t))
(add-hook 'typescript-mode #'setup-tide-mode)
(add-hook 'js2-mode #'setup-tide-mode)


;; TREEMACS
(straight-use-package 'treemacs)
(global-set-key (kbd "<f8>") 'treemacs)
(with-eval-after-load 'treemacs
  (defun treemacs-ignore-example (filename absolute-path)
    (or (string-equal filename "*\\__pycache__\\/\\'")
        (string-prefix-p "/x/y/z/" absolute-path)))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-example))


;; TREEMACS-ALL-THE-ICONS
(straight-use-package 'treemacs-all-the-icons)


;; TREEMACS-DIRED
(straight-use-package 'treemacs-icons-dired)


;;; TYPESCRIPT MODE
(straight-use-package 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.d.ts\\'" . typescript-mode))


;; WEB-BEAUTIFY
(straight-use-package 'web-beautify)

;; WEB-MODE
;; https://web-mode.org/
(straight-use-package 'web-mode)
(defun setup-web-mode()
  "Settings for web-mode."
  (setq
   web-mode-attr-indent-offset 4
   web-mode-css-indent-offset 2 ;; CSS
   web-mode-enable-block-face t
   web-mode-enable-css-colorization t
   web-mode-enable-current-element-highlight t
   web-mode-markup-indent-offset 2)) ;; HTML
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-hook 'web-mode-hook #'setup-web-mode)

;; YAML-MODE
;; https://github.com/yoshiki/yaml-mode
(straight-use-package 'yaml-mode)
(defun setup-yaml-mode ()
  "Settings for yaml-mode."
  (interactive)
  (flycheck-mode +1)
  (rainbow-delimiters-mode +1))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-hook 'yaml-mode #'setup-yaml-mode)

;;; init.el ends here
