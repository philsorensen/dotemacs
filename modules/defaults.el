;;; defaults.el --- basic default settings            -*- lexical-binding: t; -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; Set basic defaults for emacs and users

;;; Code:

;; Set email address based on username
(if (string-equal user-login-name "pas37")
    (setq user-mail-address "pas37@cornell.edu")
  (setq user-mail-address "phil.a.sorensen@gmail.com"))

;; Prefer utf-8
(prefer-coding-system 'utf-8)

;; Use spaces not tabs
(setq-default indent-tabs-mode nil)

;; Auto-revert
(setup autorevert
  (:option global-auto-revert-non-file-buffers t
           auto-revert-check-vc-info t)
  (:with-hook after-init-hook
    (:hook global-auto-revert-mode)))


(provide 'defaults)
;;; defaults.el ends here
