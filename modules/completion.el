;;; completion.el --- basic completion settings  -*- lexical-binding: t; -*-

;; Author: Phillip Sorensen <phil.a.sorensen@gmail.com>

;;; Commentary:

;; This file contains the setup for basic minibuffer command completions with
;; additional information and completion-at-point/completion-at-region
;; completions.

;;; Code:

;;;; Minibuffer completions and additional info

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :bind
  ([remap describe-bindings] . embark-bindings)
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(setup (:package corfu)
  (:option corfu-auto t)
  (global-corfu-mode))


(provide 'completion)
;;; completion.el ends here
