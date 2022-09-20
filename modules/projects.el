;;; projects.el --- setup for project related stuff  -*- lexical-binding: t; -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; This is the setup for projects.  This includes setup for project
;; settings, version control, global LSP settings. 

;;; Code:

;;;; Project settings (pull lastest from ELPA)

(setup (:package project))


;;;; Global LSP settings (using eglot)

(setup (:package eglot))


(provide 'projects)
;;; projects.el ends here
