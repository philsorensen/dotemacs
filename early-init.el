;;; early-init.el --- Emacs early initialization  -*- lexical-binding: t -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; This is the early initialization file.  It is called before the GUI is
;; initialized and is used to set package.el initialization parameters, frame
;; and interface defaults, and startup opitimizations.

;;; Code:

;;;; package.el initialization

(defvar pas/package-dir (locate-user-emacs-file "packages"))

(setq package-user-dir pas/package-dir
      package-quickstart-file (expand-file-name "quickstart.el" pas/package-dir)
      package-quickstart t)


;;;; Frame/GUI options

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(fringe-mode 12)

(setq default-frame-alist
      '((width . 85)
        (height . 40)
        (font . "Fira Code Retina-11")))

;;; early-init.el ends here
