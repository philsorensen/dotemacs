;;; defaults.el --- basic default settings  -*- lexical-binding: t; -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; Set basic defaults for emacs

;;; Code:

;;;; User information

;; Set email address based on username
(if (string-equal user-login-name "pas37")
    (setq user-mail-address "pas37@cornell.edu")
  (setq user-mail-address "phil.a.sorensen@gmail.com"))


;;;; Buffer defaults

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


;;;; Persistence/state (files stored in $XDG_STATE_HOME/emacs)

;; Move auto-save-list/
(setq auto-save-list-file-prefix
      (file-name-concat pas--state-dir "auto-save-list/.saves-"))

;; Recent files
(setq recentf-save-file (file-name-concat pas--state-dir "recentf"))
(recentf-mode 1)

;; Save minibuffer history
(setq savehist-file (file-name-concat pas--state-dir "history"))
(savehist-mode 1)


;;;; Daily tasks

(defun pas--daily-tasks ()
  "Tasks to run daily"
  (interactive)
  (native-compile-prune-cache)
  (package-upgrade-all))

;; Run daily tasks 1 minute after startup and then daily at 23:59
(run-with-timer 60 nil #'pas--daily-tasks)
(run-at-time "23:59" 86400 #'pas--daily-tasks)


(provide 'defaults)
;;; defaults.el ends here
