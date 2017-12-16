;;; company.el --- Summary
;;; Commentary:
;;; Settings for company-mode - Emacs autocompletion
;;; Code:

(use-package company
  :diminish
  :commands company-mode
  :config
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p) ad-do-it)
    )

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
	 (if 1(and (listp backend)(member 'company-yasnippet backend))
	     backend
	   (append (if (consp backend) backend (list backend))
		   '(:with company-yasnippet))))
       (setq company-backends
	     (mapcar #'company-mode/backend-with-yas company-backends)))))

(use-package company-quickhelp
  :bind (:map company-active-map
	      ("C-c h" . company-quickhelp-manual-begin)))

;;; company.el ends here
