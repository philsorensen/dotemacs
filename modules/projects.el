;;; projects.el --- setup for project related stuff  -*- lexical-binding: t; -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; This is the setup for projects.  This includes setup for project
;; settings, version control, global LSP settings, and general setting
;; related to "projects".

;;; Code:

;;;; Project settings (pull latest from ELPA)

(use-package project)


;;;; Global LSP settings (using eglot)

(use-package eglot
  :bind
  ("M-n" flymake-goto-next-error)
  ("M-p" flymake-goto-prev-error))


;;;; Add <mode>-local-vars-hook call to hack-local-variables-hook

;; (See: https://stackoverflow.com/questions/5147060).  This is
;; needed to use file-local and dir-local variables in hooks

(defun run-local-vars-mode-hook ()
  "Run a hook for the major mode after the local variable have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)


(provide 'projects)
;;; projects.el ends here
