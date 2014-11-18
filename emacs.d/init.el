;;; Define where is custom file - it is modified by emacs when using menu to customize.
(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(load custom-file)

;;; Add package archives from which packages will be installed
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; Ensure that req-package is installed and used.
;;; req-package uses use-package but enables dependencies through :require.
(if (not (package-installed-p 'req-package))
    (progn
      (package-refresh-contents)
      (package-install 'req-package)))
(require 'req-package)


;;---------- General settings -----------;;
(display-time) ; Display time in mode line
(column-number-mode t) ; Column number is shown at mode line
(global-linum-mode t) ; Show line numbers
(setq-default indent-tabs-mode nil) ; Replace tabs with spaces
(windmove-default-keybindings 'meta) ; Change buffer with M + arrow
(show-paren-mode t) ; Highlight matching parent
;; Customize GUI
(if (display-graphic-p)
  (progn
    (tool-bar-mode -1) ; remove tool bar
    (scroll-bar-mode -1))) ; remove scrolls
;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)
;;---------------------------------------;;


;;-------------- Packages ---------------;;
;; Dependencies are automatically installed by package.el!

(req-package c-mode
  :mode ("\\.ispc\\'" . c-mode))

(req-package workgroups
  :config
  (progn
    ;;; windows layout: load workgroups on start, save them on exit
    (workgroups-mode 1)
    (setq wg-file (concat user-emacs-directory "myWorkgroups"))
    (wg-load wg-file)
    (add-hook 'kill-emacs-hook (lambda () (wg-save wg-file)))))

(req-package auto-complete
  :config
  (progn
    (require 'auto-complete-config)
    (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))
    (ac-config-default)))

(req-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    (setq js2-highlight-level 3) ; Rich highlighting
    (setq-default js2-basic-offset 2)
    (req-package ac-js2
      :config
      (progn
        (add-hook 'js2-mode-hook 'ac-js2-mode)))))

(req-package haskell-mode)

(req-package web-mode
  :mode ("\\.html?\\'" . web-mode))

(req-package less-css-mode)

(req-package-finish) ; Load packages in right order.
;;---------------------------------------;;


