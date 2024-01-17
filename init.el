;;; init.el --- main init file for Emacs  -*- lexical-binding: t -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>
;; URL: https://github.com/philsorensen/dotemacs
;; Compatibility: emacs-version >= 29.1

;;; Commentary:

;; This is the main file for my Emacs setup.  It configures
;; use-package, and then pulls in the rest of the configuration from
;; the 'modules' directory.  Finally it loads any extra local setup
;; from the customization system (custom.el) and local.el files.

;;; Code:

;; Check compatibility
(if (version< emacs-version "29.1")
    (error "This Emacs setup only works with version 29.1 and above"))


;;;; Configure use-package

(require 'use-package)
(require 'use-package-ensure)

;; "Production" use-package settings
(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-expand-minimally t)

;; "Debug" settings (when --debug-init)
(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t))

;; Add :ensure-system-package keyword
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
