;;; defaults.el --- basic default settings  -*- lexical-binding: t; -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; Set basic defaults for emacs

;;; Code:

;; Set email address based on username
(if (string-equal user-login-name "pas37")
    (setq user-mail-address "pas37@cornell.edu")
  (setq user-mail-address "phil.a.sorensen@gmail.com"))

;; Prefer utf-8
(prefer-coding-system 'utf-8)

;; Use spaces not tabs
(setq-default indent-tabs-mode nil)

;; Default to auto-fill and set to 80 charaters
(setq-default fill-column 80
              auto-fill-function 'do-auto-fill)

;; Auto-revert
(setq global-auto-revert-non-file-buffers t
      auto-revert-check-vc-info t)

(add-hook 'after-init-hook #'global-auto-revert-mode)


(provide 'defaults)
;;; defaults.el ends here
