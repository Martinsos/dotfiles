(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(load custom-file)



;;; load libraries
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path (concat user-emacs-directory "elpa/"))

(require 'sr-speedbar)

; workgroups
(add-to-list 'load-path (concat user-emacs-directory "elisp/workgroups"))
(require 'workgroups)

; nxhtml
(load (concat user-emacs-directory "elisp/nxhtml/autostart.el"))

; auto-complete
(add-to-list 'load-path (concat user-emacs-directory "elpa/auto-complete-20121022.2254/"))
(add-to-list 'load-path (concat user-emacs-directory "elpa/popup-20121020.1203/"))
(require 'auto-complete-config)
;;;




;;; add ELPA libraries
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
)

;;; mode line
(display-time)

;;; Replace tabs with spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; keybindings
(windmove-default-keybindings 'meta) ; change buffer with M+arrow

;;; minor modes
(tool-bar-mode -1) ; remove tool bar
(scroll-bar-mode -1) ; remove scrolls
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

;;; auto-complete
(ac-config-default)
;(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "elpa/auto-complete-20121022.2254/dict"))

;;; topcoder plugin
;(with-demoted-errors
;  (gnuserv-start)
;  (load-library (concat user-emacs-directory "elisp/topcoder/topcoder.el")) )
(condition-case nill
    (progn
      (gnuserv-start)
      (load-library (concat user-emacs-directory "elisp/topcoder/topcoder.el")) )
  (error (message-box "Topcoder plugin encountered error!\nCheck if gnuserv is installed.")) )
       

;;; open .ispc files with c-mode
(add-to-list 'auto-mode-alist '("\\.ispc\\'" . c-mode))
