;;; init.el --- Summary
;;; Commentary:
;;; Main EMACS settings file, load settings from parts.

;;; Code:

(defvar emacs-config-dir (file-name-directory user-init-file) "Root directory with settings.")
(defvar autosave-dir (concat emacs-config-dir "saves") "Directory for autosaves.")
(defvar backups-dir (concat emacs-config-dir "backups") "Directory for backups.")
(defvar is-gui-mode (display-graphic-p) "EMACS runned in GUI mode.")

;;; Создание каталогов для резервных копий и файлов автосохранения
(unless (file-exists-p autosave-dir)
  (progn
    (make-directory autosave-dir)
    (message "Создана директория для файлов автосохранения.")))

(require 'calendar)
(require 'face-remap)
(require 'ispell)
(setq
 auto-save-file-name-transforms `((".*" , autosave-dir) t)
 calendar-week-start-day 1
 create-lockfiles nil
 cursor-type 'bar
 custom-file (expand-file-name "custom.el" emacs-config-dir)
 delete-old-versions t
 indent-tabs-mode nil
 inhibit-splash-screen t
 inhibit-startup-message t
 initial-major-mode 'markdown-mode
 initial-scratch-message nil
 make-backup-files t
 overwrite-mode-binary nil
 overwrite-mode-textual nil
 ring-bell-function #'ignore
 tab-width 4
 text-scale-mode-step 1.1 ;; Шаг увеличения масштаба
 truncate-lines 1
 use-dialog-box nil
 user-full-name "Dunaevsky Maxim"
 visible-bell nil)

(message backup-directory-alist)

;; Aspell для Linux, в Windows без проверки орфографии
(when (string-equal system-type "gnu/linux")
  (setq  ispell-program-name "/usr/bin/aspell"))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar generic-packages '
  (
   adoc-mode
   anaconda-mode
   ansible
   apache-mode
   apt-sources-list
   company
   company-anaconda
   company-terraform
   company-web
   dash
   diff-hl
   direnv
   dockerfile-mode
   doom-modeline ;; https://github.com/seagle0128/doom-modeline
   easy-hugo
   edit-indirect
   editorconfig
   flycheck
   flycheck-clang-tidy
   flycheck-color-mode-line
   flycheck-indicator
   flycheck-pos-tip
   format-all
   go-mode
   helm ; https://github.com/emacs-helm/helm
   helm-company
   highlight-indentation ; https://github.com/antonj/Highlight-Indentation-for-Emacs
   hl-line
   ibuffer
   js2-mode
   json-mode
   lsp-mode ; https://github.com/emacs-lsp
   lsp-ui ; https://github.com/emacs-lsp/lsp-ui
   magit
   markdown-mode
   miniedit
   multiple-cursors
   nlinum
   nlinum-hl ;; https://github.com/hlissner/emacs-nlinum-hl
   org
   org-roam
   php-mode
   ;; powerline ; https://github.com/milkypostman/powerline
   protobuf-mode
   pyenv-mode ; https://github.com/pythonic-emacs/pyenv-mode
   python
   python-mode
   rainbow-delimiters ; https://github.com/Fanael/rainbow-delimiters
   restclient ; https://github.com/pashky/restclient.el
   scala-mode
   terraform-mode
   tide
   treemacs
   treemacs-icons-dired
   treemacs-magit
   typescript-mode
   undo-tree
   web-beautify
   web-mode
   which-key
   ws-butler
   yaml-mode
   yasnippet

   airline-themes ; THEMES
   base16-theme
   monokai-theme
   ) "Packages for any EMACS version: console and UI.")

(defvar graphic-packages '
  (
   all-the-icons
   all-the-icons-dired ;; https://github.com/wyuenho/all-the-icons-dired
   all-the-icons-ibuffer ; https://github.com/seagle0128/all-the-icons-ibuffer
   mode-icons ; https://github.com/ryuslash/mode-icons
   ) "Packages only for graphical mode.")

(defvar required-packages)
(if is-gui-mode
    (setq required-packages (append generic-packages graphic-packages generic-packages))
  (setq required-packages generic-packages))

;; Установка необходимых пакетов
(defun install-required-packages ()
  "Install all required packages."
  (message "Установка отсутствующих пакетов...")
  (defvar packages-refreshed 0 "Список пакетов обновлён.")
  (dolist (pkg required-packages)
    (unless (package-installed-p pkg)
      (when (equal packages-refreshed 0)
        (progn
          (package-refresh-contents)
          (message "Список доступных пакетов обновлён...")
          (setq packages-refreshed 1)))
      (package-install pkg t))))
(install-required-packages)


;; Now EMACS "see" packages in "straight" directory
;; (add-to-list 'load-path (expand-file-name "straight" emacs-config-dir))
(fset 'yes-or-no-p 'y-or-n-p) ;;; Shortcuts for yes and no


;; Resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-x o") 'next-multiframe-window)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

(global-set-key (kbd "C-f") 'isearch-forward)
;; Размер шрифта
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "M-=") 'text-scale-decrease)

;; Settings for window (not only a Windows!) system.
(defvar default-font-family nil "Default font family.")
(when is-gui-mode
  (progn
    ;; Install iconic fonts
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
      (progn (message "Download and install fonts with all-the-icons-install-fonts command."))))

    ;; Turn on iconic modes
    (require 'all-the-icons)
    (require 'all-the-icons-dired)
    (require 'all-the-icons-ibuffer)
    (require 'mode-icons)
    (all-the-icons-dired-mode 1)
    (fringe-mode 2)
    (mode-icons-mode 1)
    (scroll-bar-mode 0) ;; Off scrollbars
    (set-face-attribute 'default nil :height 130)
    (tool-bar-mode 0) ;; Off toolbar
    (tooltip-mode 0) ;; No windows for tooltip
    (window-divider-mode 0)
    )

  ;; Font settings for Linux and Windows
  (defvar available-fonts (font-family-list))
  (cond
   ( ;; Windows
    (string-equal system-type "windows-nt")
    (when (member "Consolas" available-fonts)
      (setq default-font-family "Consolas")))
   ( ;; Linux
    (string-equal system-type "gnu/linux")
    (cond
     (
      (member "Source Code Pro" available-fonts)
      (setq default-font-family "Source Code Pro"))
     (
      (member "DejaVu Sans Mono" available-fonts)
      (setq default-font-family "DejaVu Sans Mono")))))
  (set-face-attribute 'default nil :family default-font-family))


;;; Save user settings in dedicated file
(setq custom-file (expand-file-name "settings.el" emacs-config-dir))
(when (file-exists-p custom-file)
  (progn
    (install-required-packages)
    (load-file custom-file)))


;; Auto-revert mode
(global-auto-revert-mode 1)


;; ENCODING
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)


;; Settings for hotkeys on any layout
(require 'quail)
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
  (let
      (
       ($buf
        (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))


;; Save/close/open
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-r") 'revert-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-'") 'comment-or-uncomment-region)
(global-set-key (kbd "C-o") 'dired)


(global-set-key (kbd "<f7>") 'xah-new-empty-buffer) ;; Buffers and windows
(global-set-key (kbd "<f9>") 'sort-lines) ;; Sort lines
(global-set-key (kbd "<esc>") 'keyboard-quit) ;; Execute commands — like [g]
(global-set-key (kbd "<f3>") 'replace-string)

;; Switch windows with C-x and arrow keys
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)


(global-set-key (kbd "M--") (lambda () (interactive) (insert "—"))) ;; Long dash by Alt+-

(global-unset-key (kbd "<insert>")) ;; Disable overwrite mode
(global-unset-key (kbd "M-,")) ;; Disable M-, as markers

(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))


;; ACE-WINDOW
;; https://github.com/abo-abo/ace-window
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)


;; ADOC MODE
;;(require 'adoc-mode)
(defun setup-adoc-mode ()
  "Settings for 'adoc-mode'."
  (interactive)
  (diff-hl-mode 1)
  (flycheck-mode 1)
  (highlight-indentation-mode 1)
  (nlinum-mode)
  (rainbow-delimiters-mode 1)
  (visual-line-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
(add-hook 'adoc-mode-hook #'setup-adoc-mode)


;; ANSIBLE MODE
(require 'ansible)
(defun setup-ansible-mode ()
  "Settings for 'ansible-mode'."
  (interactive)
  (company-mode 1))
(add-hook 'ansible-mode-hook #'setup-ansible-mode)

;; APT SOURCES LIST MODE
;; https://git.korewanetadesu.com/apt-sources-list.git
(defun setup-apt-sources-list-mode ()
  "Settings for 'apt-sources-list-mode'."
  (interactive)
  (diff-hl-mode 1)
  (flycheck-mode 1)
  (nlinum-mode 1)
  (company-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1)
  (visual-line-mode 1)
  (rainbow-delimiters-mode 1))
(add-to-list 'auto-mode-alist (cons "\\.list\\'" 'apt-sources-list-mode))
(add-hook 'apt-sources-list-mode #'setup-apt-sources-list-mode)


;; COLUMN NUMBER MODE
;; BUILTIN
;; Show column number in modeline
(column-number-mode 1)


;; COMPANY-MODE
;;https://company-mode.github.io/
(require 'company)
(require 'company-dabbrev)
(setq
 company-dabbrev-ignore-case nil
 company-dabbrev-downcase nil
 company-dabbrev-ignore-case nil
 company-idle-delay 0
 company-minimum-prefix-length 2
 company-tooltip-align-annotations t)


;; COMPANY-WEB
(require 'company-web)


;; CONF MODE
(require 'conf-mode)
(add-to-list 'auto-mode-alist '("\\.flake8\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.ini\\'" . conf-mode ))
(add-to-list 'auto-mode-alist '("\\.list\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.pylintrc\\'" . conf-mode))


;; DELETE SELECTION MODE
;; BUILTIN
;; Delete selected region
(delete-selection-mode 1)


;; DESKTOP-SAVE-MODE
(require 'desktop)
(setq
 desktop-save t
 desktop-modes-not-to-save
 '(
   dired-mode
   Info-mode
   info-lookup-mode))
(desktop-save-mode 1)


;; DIRED
(require 'dired)
(when (string-equal system-type "gnu/linux")
  ;; Это может не работать в Windows, надо проверить
  (setq dired-listing-switches "-lahX --group-directories-first"))
(defun setup-dired-mode ()
  "Settings for 'dired-mode'."
  (auto-revert-mode 1)
  (hl-line-mode 1))
(add-hook 'dired-mode-hook #'setup-dired-mode)

;; DIRENV-MODE
;; https://github.com/wbolster/emacs-direnv
(require 'direnv)
(setq direnv-use-faces-in-summary nil)
(direnv-mode 1)


;; DOCKERFILE-MODE
(defun setup-dockerfile-mode ()
  "Settings for 'dockerfile-mode'."
  (company-mode 1)
  (flycheck-mode 1)
  (lsp-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("Dockerfile'" . dockerfile-mode))
(add-hook 'dockerfile-mode-hook #'lsp)
(add-hook 'dockerfile-mode-hook #'setup-dockerfile-mode)


;; DOOM-MODELINE
;; https://github.com/seagle0128/doom-modeline
(require 'doom-modeline)
(setq
 doom-modeline-hud is-gui-mode
 doom-modeline-icon is-gui-mode
 doom-modeline-lsp t
 doom-modeline-major-mode-icon is-gui-mode
 ;; doom-modeline-minor-modes t
 doom-modeline-project-detection 'auto)
(doom-modeline-mode 1)


;; EDITORCONFIG EMACS
;; https://github.com/editorconfig/editorconfig-emacs
(require 'editorconfig)
(setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
(editorconfig-mode 1)


;; ELECTRIC-PAIR-MODE
;; EMBEDDED
(require 'electric)
(electric-pair-mode 1)


;; EMACS LISP MODE
;; IT IS NOT A ELISP-MODE!
(defun setup-emacs-lisp-mode ()
  "Settings for EMACS Lisp Mode."
  (interactive)
  (company-mode 1)
  (diff-hl-mode)
  (flycheck-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'emacs-lisp-mode-hook #'setup-emacs-lisp-mode)
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))


;; FLYCHECK
(require 'flycheck)
(require 'flycheck-color-mode-line) ;; https://github.com/flycheck/flycheck-color-mode-line
(require 'flycheck-indicator)
(require 'flycheck-pos-tip) ;; https://github.com/flycheck/flycheck-pos-tip
(setq
 flycheck-check-syntax-automatically '(mode-enabled save new-line)
 flycheck-locate-config-file-functions
 '(
   flycheck-locate-config-file-by-path
   flycheck-locate-config-file-ancestor-directories
   flycheck-locate-config-file-home)
 flycheck-highlighting-mode 'lines
 flycheck-indication-mode 'left-fringe
 flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc"
 )
(defun setup-flycheck-mode ()
  "Minor modes for 'flycheck-mode'."
  (interactive)
  (flycheck-color-mode-line-mode 1)
  (flycheck-indicator-mode 1)
  (flycheck-pos-tip-mode is-gui-mode))
(add-hook 'flycheck-mode-hook #'setup-flycheck-mode)

;; FORMAT ALL
;; https://github.com/lassik/emacs-format-all-the-code
(require 'format-all)
(global-set-key (kbd "<f12>") 'format-all-buffer)


;; GO-MODE
;; https://github.com/dominikh/go-mode.el
(defun setup-go-mode()
  "Settings for 'go-mode'."
  (interactive)
  (abbrev-mode 1)
  (buffer-face-mode 1)
  (company-mode 1)
  (diff-hl-mode 1)
  (flycheck-mode 1) ;; Turn on linters
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (visual-line-mode 1) ;; Highlight current line
  (whitespace-mode 1) ;; Show spaces, tabs and other
  (ws-butler-mode 1)) ;; Delete trailing spaces on changed lines)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook #'setup-go-mode)


;; HELM
(require 'helm)
(setq
 completion-styles '(flex))
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)


;; HELM-COMPANY
(require 'helm-company)
(define-key company-active-map (kbd "C-:") 'helm-company)


;; IBUFFER
(require 'ibuffer)
(require 'ibuf-ext)
(defalias 'list-buffers 'ibuffer)
(setq
 ibuffer-expert 1
 ibuffer-hidden-filter-groups (list "Helm" "*Internal*")
 ibuffer-saved-filter-groups
 '(
   ("default"
    ("Dired"
     (mode . dired-mode))
    ("Markdown"
     (mode . markdown-mode))
    ("YAML"
     (mode . yaml-mode))
    ("Org"
     (mode . org-mode))
    ("Protobuf"
     (mode . protobuf-mode))
    ("Lisp"
     (mode . emacs-lisp-mode))
    ("Python"
     (or
      (mode . python-mode)
      (mode . elpy-mode)
      (mode . anaconda-mode)))
    ("Shell-script"
     (or
      (mode . shell-script-mode)
      (mode . sh-mode)))
    ("Terraform"
     (or
      (mode . terraform-mode)))
    ("SQL"
     (or
      (mode . sql-mode)))
    ("Web"
     (or
      (mode . js-mode)
      (mode . js2-mode)
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
      (name . "\\*\\(Echo\\|Minibuf\\)")))))
 ibuffer-show-empty-filter-groups nil ;; Do not show empty groups
 ibuffer-truncate-lines nil
 ibuffer-sorting-mode 'filename/process
 ibuffer-formats
 '(
   (
    mark
    modified
    read-only
    locked
    " "
    (mode 8 -1 :left)
    " "
    (name 30 40 :left :elide)
    " "
    filename-and-process)
   (
    mark
    " "
    (name 16 -1)
    " "
    filename))
 ibuffer-use-other-window nil)

(defun setup-ibuffer-mode ()
  "Settings for 'ibuffer-mode'."
  (interactive)
  (hl-line-mode 1)
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))
(global-set-key (kbd "<f2>") 'ibuffer)
;; (add-to-list 'ibuffer-never-show-predicates "^\\*")
(add-hook 'ibuffer-mode-hook #'setup-ibuffer-mode)


;; JAVA-MODE
(defun setup-java-mode ()
  "Settings for 'java-mode'."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'java-mode-hook #'setup-java-mode)


;; JS2-MODE
;; https://github.com/mooz/js2-mode
(require 'js2-mode)
(defun setup-js2-mode ()
  "Settings for 'js2-mode'."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'javascript-mode-hook #'setup-js2-mode)
(add-hook 'js2-mode-hook #'setup-js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; JSON-MODE
;; https://github.com/joshwnj/json-mode
(require 'json)
(defun setup-json-mode()
  "Settings for json-mode."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.\\(?:json\\|bowerrc\\|jshintrc\\)\\'" . json-mode))
(add-hook 'json-mode-hook #'setup-json-mode)


;; LSP MODE
(require 'lsp-mode)


;; MAGIT
;; https://magit.vc/
(require 'magit)
(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'magit-checkout)


;; Makefile
(defun setup-makefile-mode ()
  "Settings for Makefile-mode."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (highlight-indentation-mode 1)
  (hl-line-mode 1)
  (hl-line-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))
(add-hook 'makefile-mode-hook #'setup-makefile-mode)


;; MARKDOWN MODE
;; https://github.com/jrblevin/markdown-mode
(require 'markdown-mode)
(setq
 header-line-format " "
 left-margin-width 4
 line-spacing 4
 markdown-fontify-code-blocks-natively t
 right-margin-width 4
 tab-width 4
 word-wrap t)
(set-face-attribute 'markdown-code-face        nil :family default-font-family)
(set-face-attribute 'markdown-inline-code-face nil :family default-font-family)
(defun setup-markdown-mode()
  "Settings for editing markdown documents."
  (interactive)
  ;; Additional modes
  (abbrev-mode 1)
  (buffer-face-mode 1)
  (diff-hl-mode 1)
  (electric-pair-mode 1)
  (flycheck-mode 1) ;; Turn on linters
  (highlight-indentation-mode 1)
  (hl-line-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (visual-line-mode 1) ;; Highlight current line
  (whitespace-mode 1) ;; Show spaces, tabs and other
  (ws-butler-mode 1) ;; Delete trailing spaces on changed lines
  (yas-minor-mode 1) ;; Snippets
  (cond ;; Turn on spell-checking only in Linux
   (
    (string-equal system-type "gnu/linux")
    (flyspell-mode 1))))
(define-key markdown-mode-map (kbd "M-.") 'markdown-follow-thing-at-point)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\README\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'setup-markdown-mode)


;; Turn off menu bar
(require 'menu-bar)
(menu-bar-mode 0)


;; LOAD THEME
(load-theme 'monokai t)


;; MULTIPLE CURSORS
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-e") 'mc/edit-lines)
(if is-gui-mode
    (progn
      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)))


;; NLINUM MODE
;; https://elpa.gnu.org/packages/nlinum.html
(require 'nlinum)
(require 'nlinum-hl)
(setq nlinum-format "%d \u2502")
(add-hook 'post-gc-hook #'nlinum-hl-flush-all-windows)

;; ...or switches windows
(advice-add #'select-window :before #'nlinum-hl-do-select-window-flush)
(advice-add #'select-window :after  #'nlinum-hl-do-select-window-flush)

;; after X amount of idle time
(run-with-idle-timer 5 t #'nlinum-hl-flush-window)
(run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)



;; ORG-MODE
;; https://orgmode.org/
(require 'org)
(setq
 truncate-lines nil
 left-margin-width 4
 org-todo-keywords '((sequence "НОВАЯ" "НА РАСПАКОВКЕ" "РАСПАКОВАНА" "ОТМЕНЕНА" "В РАБОТЕ" "ТРЕБУЕТСЯ ИНФОРМАЦИЯ" "РЕВЬЮ" "ЗАКРЫТА БЕЗ СЛИЯНИЯ" "|" "ВЫПОЛНЕНА"))
 right-margin-width 4
 word-wrap t)
(defun setup-org-mode ()
  "Minor modes for 'org-mode'."
  (interactive)
  (company-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook #'setup-org-mode)



;; PHP-MODE
(require 'php)
(defun setup-php-mode ()
  "Minor modes for 'php-mode'."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (hl-line-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'php-mode-hook #'setup-php-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))


;; PROTOBUF-MODE
;; https://github.com/emacsmirror/protobuf-mode
(require 'protobuf-mode)
(defun setup-protobuf-mode ()
  "Settings for 'protobuf-mode'."
  (interactive)

  (company-mode 1)
  (flycheck-mode 1)
  (hl-line-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'ptotobuf-mode-hook #'setup-protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))


;; PYTHON-MODE
(require 'python)
(setq
 doom-modeline-env-enable-python t
 py-company-pycomplete-p t
 py-electric-comment-p t
 py-pylint-command-args "--max-line-length 120"
 py-virtualenv-workon-home "~/.virtualenvs"
 python-shell-interpreter "python3"
 tab-width 4)
(defun setup-python-mode ()
  "Settings for 'python-mode'."
  (interactive)
  (anaconda-mode 1)
  (company-mode 1)
  (flycheck-mode 1)
  (highlight-indentation-mode 1)
  (hl-line-mode 1)
  (lsp-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1)
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (define-key python-mode-map (kbd "M-/") 'jedi:show-doc)
  (define-key python-mode-map (kbd "M-?") 'helm-jedi-related-names))
(add-hook 'python-mode-hook #'lsp)
(add-hook 'python-mode-hook #'setup-python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


;; RST-MODE
(defun setup-rst-mode ()
  "Settings for 'rst-mode'."
  (interactive)

  (company-mode 1)
  (flycheck-mode 1)
  (hl-line-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'rst-mode-hook #'setup-rst-mode)
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))


;; RUBY-MODE
;;(straight-use-package 'ruby-mode)
(require 'ruby-mode)
(defun setup-ruby-mode ()
  "Settings for 'ruby-mode'."
  (interactive)

  (company-mode 1)
  (flycheck-mode 1)
  (highlight-indentation-mode 1)
  (hl-line-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'ruby-mode-hook #'setup-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb\\'" .ruby-mode))


;; PAREN-MODE
(show-paren-mode 1)


;; SAVE-PLACE-MODE
;; https://www.emacswiki.org/emacs/SavePlace
;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file.
(require 'saveplace)
(setq save-place-file (expand-file-name ".emacs-places" emacs-config-dir)
      save-place-forget-unreadable-files 1)
(save-place-mode 1)


;; SCALA MODE
;; https://github.com/hvesalai/emacs-scala-mode
(defun setup-scala-mode ()
  "Settings for 'scala-mode'."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
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
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))
(add-hook 'shell-script-mode #'setup-shell-script-mode)
(add-hook 'sh-mode-hook #'setup-shell-script-mode)


;; SQL MODE
(defun setup-sql-mode ()
  "Settings for SQL-mode."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
(add-hook 'sql-mode-hook #'setup-sql-mode)


;; TIDE-MODE
;; https://github.com/ananthakumaran/tide/
(defun setup-tide-mode ()
  "Settings for 'tide-mode'."
  (interactive)
  (company-mode 1)
  (eldoc-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (tide-hl-identifier-mode 1)
  (tide-setup)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-hook 'tide-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tide-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tide-mode))


;; TERRAFORM-MODE
;; https://github.com/emacsorphanage/terraform-mode
(with-eval-after-load "terraform-mode"
  (setq flycheck-checker 'terraform))
(defun setup-terraform-mode ()
  "Settings for terraform-mode."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (hl-line-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist (cons "\\.tf\\'" 'terraform-mode))
(add-hook 'terraform-mode-hook #'setup-terraform-mode)
(add-hook 'hcl-mode-hook #'setup-terraform-mode)


;; TREEMACS — awesome file manager (instead NeoTree)
;; https://github.com/Alexander-Miller/treemacs
(require 'treemacs)
(setq treemacs-width 30)
(defun treemacs-get-ignore-files (filename absolute-path)
  (or
   (string-equal filename ".emacs.desktop.lock")
   (string-equal filename "__pycache__")))
(add-to-list 'treemacs-ignored-file-predicates #'treemacs-get-ignore-files)
(define-key treemacs-mode-map (kbd "f") 'find-grep)
(global-set-key (kbd "<f8>") 'treemacs)
(treemacs-follow-mode 1)
(treemacs-git-mode 'simple)
(treemacs-filewatch-mode 1)

;; TREEMACS-ICONS-DIRED-MODE
(treemacs-icons-dired-mode 1)


;; TYPESCRIPT MODE
;; https://github.com/emacs-typescript/typescript.el
(require 'typescript-mode)
(defun setup-typescript-mode ()
  "Settings for 'typescript-mode'."
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (highlight-indentation-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.d.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'setup-typescript-mode)


;; UNDO-TREE
(require 'undo-tree)
(global-undo-tree-mode 1)


;; WEB-MODE
;; https://web-mode.org/
(require 'web-mode)
(setq
 web-mode-attr-indent-offset 4
 web-mode-css-indent-offset 2 ;; CSS
 web-mode-enable-block-face t
 web-mode-enable-css-colorization t
 web-mode-enable-current-element-highlight t
 web-mode-markup-indent-offset 2)

(defun setup-web-mode()
  "Settings for web-mode."
  (interactive)
  (company-mode 1)
  (company-web 1)
  (flycheck-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-hook 'web-mode-hook #'setup-web-mode)


;; WHICH-KEY MODE
;; https://github.com/justbur/emacs-which-key
(require 'which-key)
(which-key-mode 1)


;; WHITESPACE MODE
;; https://www.emacswiki.org/emacs/WhiteSpace
(require 'whitespace)
(setq
 whitespace-display-mappings
 '(
   (space-mark   ?\    [?\xB7]     [?.]) ; space
   (space-mark   ?\xA0 [?\xA4]     [?_]) ; hard space
   (newline-mark ?\n   [?¶ ?\n]    [?$ ?\n]) ; end of line
   (tab-mark     ?\t   [?\xBB ?\t] [?\\ ?\t]) ; tab
   )
 whitespace-line-column 1000) ;; Highlight lines with length bigger than 1000 chars)
(set-face-attribute 'whitespace-space nil
                    :family default-font-family
                    :foreground "#75715E")
(set-face-attribute 'whitespace-indentation nil
                    :family default-font-family
                    :foreground "#E6DB74")


;; YAML-MODE
;; https://github.com/yoshiki/yaml-mode
(require 'yaml-mode)
(defun setup-yaml-mode ()
  "Settings for yaml-mode."
  (interactive)
  (company-mode 1)
  (diff-hl-mode 1)
  (flycheck-mode 1)
  (highlight-indentation-mode 1)
  (hl-line-mode 1)
  (nlinum-mode 1)
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1)
  (ws-butler-mode 1))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yfm\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook #'setup-yaml-mode)


;; YASNIPPET
;; http://github.com/joaotavora/yasnippet
(require 'yasnippet)
(defvar snippets-dir
  (expand-file-name "snippets" emacs-config-dir))
(unless (file-exists-p snippets-dir)
  (make-directory snippets-dir))
(yas-global-mode 1)


(server-start)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(load custom-file)

(provide 'init.el)

;;; init.el ends here
