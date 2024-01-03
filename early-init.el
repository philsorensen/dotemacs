;;; early-init.el --- Emacs early initialization  -*- lexical-binding: t -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; This is the early initialization file.  It is called before the GUI is
;; initialized and is used to set startup opitimizations, package.el
;; initialization parameters, frame and interface defaults, and native compile
;; configuration.

;;; Code:

(require 'xdg)


;;;; set XDG directories for cache, data, and state

(defconst pas--cache-dir
  (file-name-as-directory (file-name-concat (xdg-cache-home) "emacs")))

(defconst pas--data-dir
  (file-name-as-directory (file-name-concat (xdg-data-home) "emacs")))

(defconst pas--state-dir
  (file-name-as-directory (file-name-concat (xdg-state-home) "emacs")))


;;;; startup optimizations

;; increase GC threshold for faster startup (256MiB)
(setq gc-cons-threshhold (expt 2 28))

;; set threshhold to back to 2MiB (default 800kB) after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)
            (setq gc-cons-threshhold (expt 2 21))))

;; load latest version of file
(setq load-prefer-newer t)


;;;; package.el initialization

;; move package directory to $XDG_DAT_HOME/emacs/elpa
(setq package-user-dir (file-name-concat pas--data-dir "elpa"))

;; setup repos and priorities
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa"  . 10)))


;;;; frame/GUI options

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


;;;; native compile configuration

;; set cache to $XDG_CACHE_HOME/emacs/eln-cache
(startup-redirect-eln-cache (file-name-concat pas--cache-dir "eln-cache"))

;; log errors but don't pop up window
(setq native-comp-async-report-warnings-errors 'silent)


;;; early-init.el ends here
