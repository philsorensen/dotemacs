;;; completion.el --- basic completion settings  -*- lexical-binding: t; -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; This file contains the setup for basic minibuffer command completions with
;; additional information and completion-at-point/completion-at-region
;; completions.

;;; Code:

;;;; Minibuffer completions and additional info

(setup (:package vertico)
  (:option vertico-cycle t)
  (vertico-mode 1))

(setup (:package marginalia)
  (marginalia-mode 1))

(setup (:package consult))

(setup (:package orderless)
  (:option completion-styles '(orderless)
           completion-category-defaults nil
           completion-category-overrides '((file (styles partial-completion)))))

(setup (:package embark)
  (:global [remap describe-bindings] #'embark-bindings)
  (:option prefix-help-command #'embark-prefix-help-command))


(provide 'completion)
;;; completion.el ends here
