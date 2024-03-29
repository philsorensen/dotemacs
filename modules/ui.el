;;; ui.el --- Initialize UI elements  -*- lexical-binding: t; -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; Initialize/setup the user interface elements.

;;; Code:

;; Turn on line and column in modeline and line numbers
(column-number-mode 1)
(line-number-mode 1)
(global-display-line-numbers-mode t)

;; remove menu bar if not GUI
(unless (display-graphic-p)
  (menu-bar-mode 0))

;; set theme
(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-common-palette-overrides '((comment yellow-cooler))))

(load-theme 'modus-vivendi :no-confirm)

;; Make help look better
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))



(provide 'ui)
;;; ui.el ends here
