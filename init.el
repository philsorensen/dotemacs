;;; init.el --- main init file for Emacs  -*- lexical-binding: t -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>
;; URL: https://github.com/philsorensen/dotemacs
;; Compatibility: emacs-version >= 27.1

;;; Commentary:

;; This is the main file for my Emacs setup.  It does the initial
;; setup of the packaging system and then calls other files for setup
;; of other pieces.  These initialization files make use of the
;; 'use-package' package.

;;; Code:

;; Check compatibility
(if (version< emacs-version "27.1")
    (error "This Emacs setup only works with version 27.1 and above"))


;;;; Setup use-package and auto-package-update

;; Install/setup use-package
(if (not (boundp 'package-archive-contents))
  (package-refresh-contents))

(dolist (needed-package '(use-package diminish bind-key))
  (unless (package-installed-p needed-package)
    (package-install needed-package)))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(customize-set-variable 'use-package-always-ensure t)

;; Setup package updates
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t))

;; Startup hook for packages updates
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (package-refresh-contents t)
	      (run-with-idle-timer 600 t #'auto-package-update-maybe)))



;;;; Load the rest of the configuration from broken out files

;; add .emacs.d/init directory to the load path
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'test)


;;;; "Local" overrides.  Not saved in code repository 

;; Load customization from separate file
(let ((pas-custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (not (file-exists-p pas-custom-file))
    (shell-command (concat "touch " pas-custom-file)))
  (setq custom-file pas-custom-file)
  (load custom-file))

;; Load local overrides
(let ((pas-local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (not (file-exists-p pas-local-file))
    (shell-command (concat "touch " pas-local-file)))
  (load pas-local-file))

;;; init.el ends here
