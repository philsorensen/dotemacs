;;; test.el --- configuration testing  -*- lexical-binding: t -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; This file contains various things that are in progress and
;; expiremental.

;;; Code:

;; Basic appearence changes
(setq inhibit-startup-screen t)

(column-number-mode 1)
(line-number-mode 1)
(global-display-line-numbers-mode t)

(when (display-graphic-p)
  (menu-bar-mode 1))

(use-package doom-themes
  :init
  (load-theme 'doom-dark+ t))

(use-package all-the-icons)
(let ((font-file (concat (or (getenv "XDG_DATA_HOME")
			     (concat (getenv "HOME") "/.local/share/"))
			 "fonts/all-the-icons.ttf")))
  (unless (file-exists-p font-file)
    (all-the-icons-install-fonts t)))

;(use-package doom-modeline
;  :init
;  (doom-modeline-mode 1))


;; completions, etc

(use-package counsel
  :diminish
  :config
  (ivy-mode 1)
  (counsel-mode 1))
 
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))


(provide 'test)

;;; test.el ends here
