;;; Code:

(require 'req-package)

(add-hook 'prog-mode-hook 'subword-mode) ; Recognize subwords in camel case words.

;; Color lines longer than 100 characters by turning on customized whitespace-mode.
(setq-default
 whitespace-line-column 100
 whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; Auto-complete.
(req-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
    (setq company-idle-delay 0)))

;; On the fly syntax checking.
(req-package flycheck
  :ensure t
  :config
  (progn
    ;; NOTE: Syntax checking is not working correctly for stack projects currently so I turn it off here for Haskell.
    (setq flycheck-global-modes '(not haskell-mode))
    (global-flycheck-mode)
    ))

;; Colors delimiters (parentheses) according to their depth/level.
(req-package rainbow-delimiters
  :ensure t
  :config
  (progn
    ;; TODO: This does not work for some reason! Rainbow delimiters mode is disabled in all the buffers,
    ;;   I need to enable it manually, per buffer, to get it working, I don't understand why.
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  ))

(req-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    ))

(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode) ; show current function def at top.

(req-package stickyfunc-enhance :ensure t)  ; Improves semantic-stickyfunc-mode.

(provide 'init-ide-common)
;;; init-ide-common.el ends here
