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



(setup (:package doom-themes)
  (load-theme 'doom-dark+ t))


(provide 'ui)
;;; ui.el ends here
