(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(load custom-file)



;;; load libraries
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path (concat user-emacs-directory "elpa/"))

; workgroups
(add-to-list 'load-path (concat user-emacs-directory "elisp/workgroups"))
(require 'workgroups)

; nxhtml
(load (concat user-emacs-directory "elisp/nxhtml/autostart.el"))
;;;



;;; add ELPA libraries
(require 'package)
(when (>= emacs-major-version 24)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
)
(package-initialize)



;;; mode line
(display-time)

;;; Replace tabs with spaces
(setq-default indent-tabs-mode nil)
;(setq-default tab-width 4)

;;; keybindings
(windmove-default-keybindings 'meta) ; change buffer with M+arrow

;;; minor modes
(if (display-graphic-p)
  (progn
    (tool-bar-mode -1) ; remove tool bar
    (scroll-bar-mode -1))) ; remove scrolls

(column-number-mode t) ; column number is shown at mode line
(global-linum-mode t) ; show line numbers
(show-paren-mode t) ; highlight matching parent

;;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)

;;; windows layout: load workgroups on start, save them on exit
(workgroups-mode 1)
(setq wg-file (concat user-emacs-directory "myWorkgroups"))
(wg-load wg-file)
(add-hook 'kill-emacs-hook (lambda () (wg-save wg-file)))

;;; nXhtml
(tabkey2-mode t) ; double tab runs autocomplete

;;; php-mode
(setq php-manual-path (concat user-emacs-directory "php-manual/"))

;;; multiple cursors, TODO: set key bindings.
(require 'multiple-cursors)

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; make emacs awesome javascript IDE
(add-hook 'js2-mode-hook 'ac-js2-mode) ; auto-completion
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
; rich highlighting
(setq js2-highlight-level 3)
; tab is 2 spaces
(eval-after-load "js2-mode" '(progn (setq-default js2-basic-offset 2)))
; brings javascript refactoring
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-r")

(add-to-list 'auto-mode-alist '("\\.ispc\\'" . c-mode))
