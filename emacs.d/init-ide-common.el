;;; Code:

(require 'req-package)

(add-hook 'prog-mode-hook 'subword-mode) ; Recognize subwords in camel case words.

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
    (global-flycheck-mode)
    ;; NOTE: Syntax checking is not working correctly for stack projects currently so I turn it off here for Haskell.
    (setq flycheck-global-modes '(not haskell-mode))
    ))

;; Colors delimiters (parentheses) according to their depth/level.
(req-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

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
