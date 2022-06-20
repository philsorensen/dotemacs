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

;; set theme (customize fill-column-indicator)
(setup (:package modus-themes)
  (:option modus-themes-bold-constructs t
           modus-themes-italic-constructs t
           modus-themes-syntax '(yellow-comments))
  (modus-themes-load-themes)
  (:with-hook after-init-hook
    (:hook modus-themes-load-vivendi))
  (:with-hook modus-themes-after-load-theme-hook
    (:hook (lambda ()
             (set-face-attribute
               'fill-column-indicator nil
               :background (modus-themes-color 'bg-active))))))


(provide 'ui)
;;; ui.el ends here
