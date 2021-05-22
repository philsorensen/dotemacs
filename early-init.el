;;; early-init.el --- Emacs early initialization  -*- lexical-binding: t -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; This is the early initialization file.  It is called before the GUI
;; is initialized and is used to set package.el parameters, frame and
;; interface defaults, and startup opitimizations.

;;; Code:

;; package.el initializtion

(setq package-user-dir (expand-file-name "packages" user-emacs-directory))

(with-eval-after-load 'package
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t))

;; Frame/GUI options

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(fringe-mode 12)

(setq default-frame-alist
      '((width . 85)
	(height . 40)
	(font . "Fira Code Retina")))


;;; early-init.el ends here
