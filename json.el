;;; json.el --- Summary
;;; Commentary:
;;; Settings for json-mode

;;; Code

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
	 ("\\.bowerrc\\'" . json-mode)
	 ("\\.jshintrc\\'" . json-mode)))

(use-package json-reformat
  :after json-mode)

;;; json.el ends here
