;;; Code:

(require 'req-package)

; Requirement is to have js-beautify node package installed globaly!
; Any configuration is done through .jsbeautifyrc files, that can be put inside project.
(req-package web-beautify
  :ensure t
  :config
  (progn
    (eval-after-load 'js2-mode
      '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
    (eval-after-load 'json-mode
      '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
    (eval-after-load 'web-mode
      '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
    (eval-after-load 'css-mode
      '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))))

(req-package json-mode :ensure t)

(req-package js2-mode
  :ensure t
  ;; :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    (setq js2-highlight-level 3) ; Rich highlighting
    (setq-default js2-basic-offset 2)

    ;; Don't warn about missing semicolon.
    (setq js2-strict-missing-semi-warning nil)
    (setq js2-missing-semi-one-line-override t)

    (req-package ac-js2
      :ensure t
      :config
      (progn
        (add-hook 'js2-mode-hook 'ac-js2-mode)))))

(req-package rjsx-mode
  :ensure t
  :mode ("\\.js\\'" . rjsx-mode))

(req-package vue-mode
  :ensure t
  :mode ("\\.vue\\'" . vue-mode))

(req-package web-mode
  :ensure t
  :mode ("\\.html?\\'" . web-mode))

(req-package pug-mode :ensure t)

(req-package less-css-mode :ensure t)

(req-package scss-mode :ensure t)

(req-package stylus-mode :ensure t)

(req-package tide  ;; Tide - Typescript Interactive Development Environment
  :ensure t
  :config
  (progn
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))
    (setq company-tooltip-align-annotations t) ; aligns annotation to the right hand side
    (add-hook 'before-save-hook 'tide-format-before-save) ; formats the buffer before saving
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    ;; format options
    (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
    ))

(provide 'init-ide-web)
;;; init-ide-web.el ends here
