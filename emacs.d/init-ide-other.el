;;; Code:

(require 'req-package)

(req-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :mode ("\\.markdown\\'" . markdown-mode))

(req-package markdown-preview-mode :ensure t)

(req-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(require 'generic-x) ; Generic Mode (for obscure languages).

(req-package cython-mode :ensure t)

(req-package cmake-mode :ensure t)

(add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))

(provide 'init-ide-other)
;;; init-ide-other.el ends here
