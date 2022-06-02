;;; early-init.el --- Emacs early initialization  -*- lexical-binding: t -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; This is the early initialization file.  It is called before the GUI is
;; initialized and is used to set package.el initialization parameters, frame
;; and interface defaults, and startup opitimizations.

;;; Code:

;;;; startup optimizations

;; increase GC threshold for faster startup (256MiB)
(setq gc-cons-threshhold (expt 2 28))

;; set threshhold to back to 2MiB (default 800kB) after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)
            (setq gc-cons-threshhold (expt 2 21))))

;;;; package.el initialization

(defvar pas/package-dir (locate-user-emacs-file "packages"))

(setq package-user-dir pas/package-dir
      package-quickstart-file (expand-file-name "quickstart.el" pas/package-dir)
      package-quickstart t)


;;;; Frame/GUI options
(setq inhibit-startup-screen t)

(dolist (param '((width . 85)
                 (height . 40)
                 (vertical-scroll-bars . nil)
                 (horizonal-scroll-bars . nil)
                 (left-fringe . 12)
                 (right-fringe . 12)
                 (tool-bar-lines . 0)
                 (font . "JetBrains Mono-12")
                 (foreground-color . "#ffffff")
                 (background-color . "#000000")))
  (add-to-list 'default-frame-alist param))


;;; early-init.el ends here
