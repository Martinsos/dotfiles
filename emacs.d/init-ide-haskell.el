;;; Code:

(require 'req-package)

(req-package haskell-mode
  :ensure t
  :config
  (progn
    (setq haskell-indentation-layout-offset 4)
    (setq haskell-indentation-starter-offset 4)
    (setq haskell-indentation-left-offset 4)
    (setq haskell-indentation-where-pre-offset 2)
    (setq haskell-indentation-where-post-offset 2)
    ;; NOTE: In config for flycheck, I disabled it for haskell-mode! In case I figure out how to make it work nicely, I should reenable it probably.
    ))

(provide 'init-ide-haskell)
;;; init-ide-haskell.el ends here
