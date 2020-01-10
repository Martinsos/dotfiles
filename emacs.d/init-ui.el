;;; Code:

(require 'req-package)


(set-frame-font "DroidSansMono-10")

(column-number-mode t) ; Column number is shown at mode line

(menu-bar-mode -1) ; remove menu bar

;; Customize GUI
(if (display-graphic-p)
  (progn
    (tool-bar-mode -1) ; remove tool bar
    (scroll-bar-mode -1))) ; remove scrolls

(req-package zenburn-theme
  :ensure t
  :config
  (progn
    (load-theme 'zenburn t)
    ))

(req-package smart-mode-line
  :ensure t
  :config
  (progn
    (sml/setup)
    ;; Only minor modes that match this list of regexes will be shown.
    (setq rm-included-modes
      (format "^ \\(%s\\)$"
        (mapconcat #'identity
                   '("FlyC.*"
                     ;; "Some other mode regexp can go here."
                     )
                   "\\|")))

    ;; Display line and columnd as (line, column).
    (setq sml/line-number-format "(%3l")
    (setq sml/numbers-separator ",")
    (setq sml/col-number-format "%2c)")

    (setq sml/use-projectile-p 'before-prefixes)

    ;; TODO: reorder elements in mode line. Check mode-line-format.
    ;; Code below should work, however it does not. How can I make sure to run it after smart-mode-line?
    ;; (defun my-mode-line-format ()
    ;;   (setq mode-line-format
    ;;         (list
    ;;          "%e"
    ;;          'mode-line-mule-info
    ;;          'mode-line-client
    ;;          'mode-line-modified
    ;;          'mode-line-remote
    ;;          'mode-line-frame-identification
    ;;          'mode-line-buffer-identification
    ;;          'sml/pos-id-separator
    ;;          'mode-line-position
    ;;          '(vc-mode vc-mode)
    ;;          'mode-line-front-space
    ;;          'sml/pre-modes-separator
    ;;          'mode-line-modes
    ;;          'mode-line-misc-info
    ;;          'mode-line-end-spaces))
    ;;   )
    ;; (eval-after-load 'smart-mode-line 'my-mode-line-format)
    ))


(provide 'init-ui)
;;; init-ui.el ends here
