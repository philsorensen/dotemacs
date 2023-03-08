;;; init.el --- main init file for Emacs  -*- lexical-binding: t -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>
;; URL: https://github.com/philsorensen/dotemacs
;; Compatibility: emacs-version >= 27.1

;;; Commentary:

;; This is the main file for my Emacs setup.  It finishes the setup of the
;; package.el packaging system, then calls other 'modules' for setup of other
;; parts, and finally loads the customization and local.el files.  The
;; initialization files use of the use-package for package configuration.

;;; Code:

;; Check compatibility
(if (version< emacs-version "27.1")
    (error "This Emacs setup only works with version 27.1 and above"))


;;;; Setup for packages

;; Setup package.el
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa"  . 10)))

;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-compute-statistics t)
(setq use-package-expand-minimally t)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package use-package-ensure-system-package)

;; Setup automatic package updates
(use-package auto-package-update
  :custom
  (auto-package-update-interval 1)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-last-update-day-filename
   (expand-file-name "last-package-update-day" pas/package-dir))
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "13:00"))


;;;; Load the rest of the configuration from seperate files

;; Add modules directory to the load path
(add-to-list 'load-path (locate-user-emacs-file "modules"))

(require 'defaults)
(require 'ui)
;(require 'completion)

(require 'programming)


;;;; "Local" overrides loaded from customization file and local.el

(let ((new-custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file new-custom-file)
  (load custom-file t))

(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (load local-file t))


;;; init.el ends here
