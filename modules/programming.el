;;; programming.el --- setup for programming  -*- lexical-binding: t; -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; This is the "global" and language specific setups for programming
;; in various languages that derive from prog-mode.

;;; Code:

;;;; Setup for all languages

(add-hook 'prog-mode-hook
          (lambda ()
            ;; show visual indicator at column 80
            (setq-local display-fill-column-indicator-column 80)
            (display-fill-column-indicator-mode)
            
            ;; auto-fill comments at column 70
            (setq-local comment-auto-fill-only-comments t
                        fill-column 70)))


(provide 'programming)
;;; programming.el ends here
