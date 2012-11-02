(setq user-emacs-directory "~/dotfiles/emacs.d/")

(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(load custom-file)

;;; load libraries
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(require 'sr-speedbar)
(require 'workgroups "workgroups/workgroups")
(load (concat user-emacs-directory "elisp/nxhtml/autostart.el"))



;;; add ELPA libraries
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
)

;;; mode line
(display-time)

;;; keybindings
(windmove-default-keybindings 'meta) ; change buffer with M+arrow

;;; minor modes
(tool-bar-mode -1) ; remove tool bar
(scroll-bar-mode -1) ; remove scrolls
(column-number-mode t) ; column number is shown at mode line
(linum-mode t) ; show line numbers
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
