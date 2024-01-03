;;; init.el --- main init file for Emacs  -*- lexical-binding: t -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>
;; URL: https://github.com/philsorensen/dotemacs
;; Compatibility: emacs-version >= 27.1

;;; Commentary:

;; This is the main file for my Emacs setup.  It sets up periodic
;; updates for installed packages, configures use-package, and then
;; pulls in the rest of the configuration from the 'modules'
;; directory.  Finally it loads any extra local setup from the
;; customization system (custom.el) and local.el files.

;;; Code:

;; Check compatibility
(if (version< emacs-version "29.1")
    (error "This Emacs setup only works with version 29.1 and above"))


;;;; Setup auto-updates and use-package

;; Run package updates after startup and then daily at 13:00
(run-with-timer 30 nil 'package-upgrade-all)
(run-at-time "13:00" 86400 'package-upgrade-all)

;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-compute-statistics t)
(setq use-package-expand-minimally t)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package use-package-ensure-system-package)


;;;; Load the rest of the configuration from seperate files

;; Add modules directory to the load path
(add-to-list 'load-path (locate-user-emacs-file "modules"))

(require 'defaults)
(require 'ui)
(require 'completion)

(require 'projects)
(require 'programming)


;;;; "Local" overrides loaded from customization file and local.el

(let ((new-custom-file (file-name-concat pas--data-dir "custom.el")))
  (setq custom-file new-custom-file)
  (load custom-file t))

(let ((local-file (file-name-concat pas--data-dir "local.el")))
  (load local-file t))


;;; init.el ends here
