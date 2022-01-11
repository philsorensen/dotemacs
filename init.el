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

;; Load customization from separate file
(let ((new-custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (not (file-exists-p new-custom-file))
    (shell-command (concat "touch " new-custom-file)))
  (setq custom-file new-custom-file)
  (load custom-file))


;;;; Setup for packages

;; Setup package.el
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu" . 100)
                                   ("melpa" . 10))
      package-menu-hide-low-priority t)

(if (not (boundp 'package-archive-contents))
  (package-refresh-contents))

;; Install setup.el
(unless (package-installed-p 'setup)
  (package-install 'setup))

;; Setup auto package updates
(setup (:package auto-package-update)
  (:option auto-package-update-delete-old-versions t
           auto-package-update-last-update-day-filename
             (expand-file-name "last-package-update-day" pas/package-dir)))

(run-with-idle-timer 1 nil #'(lambda ()
                               (package-quickstart-refresh)
                               (auto-package-update-maybe)))
(auto-package-update-at-time "13:00")


;;;; Load the rest of the configuration from seperate files

;; Add modules directory to the load path
(add-to-list 'load-path (locate-user-emacs-file "modules"))


;;;; "Local" overrides.  Not saved in code repository

(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (not (file-exists-p local-file))
    (shell-command (concat "touch " local-file)))
  (load local-file))

;;; init.el ends here
