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

;; install 'all-the-icons' fonts
(setup (:package all-the-icons)
  (let ((font-file (concat (or (getenv "XDG_DATA_HOME")
			       (concat (getenv "HOME") "/.local/share/"))
			   "fonts/all-the-icons.ttf")))
    (unless (file-exists-p font-file)
      (all-the-icons-install-fonts t))))

;; set theme
(setup (:package doom-themes)
  (load-theme 'doom-dark+ t))

;; doom-modeline
(setup (:package doom-modeline)
  (:option doom-modeline-height 15
           doom-modeline-bar-width 6
	   doom-modeline-minor-modes t
	   doom-modeline-buffer-file-name-style 'file-name)
  (:with-hook after-init-hook
    (:hook doom-modeline-init)))


(provide 'ui)
;;; ui.el ends here
