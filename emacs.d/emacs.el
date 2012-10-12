(setq user-emacs-directory "~/dotfiles/emacs.d/")

(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(load custom-file)

(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(require 'sr-speedbar)
(require 'workgroups "workgroups/workgroups")



;;; open emacs in fullscreen
(defun fullscreen () "fullscreen"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
     '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
     '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)
;(fullscreen)

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
(wg-load (concat user-emacs-directory "myWorkgroups"))
(add-hook 'kill-emacs-hook (lambda () (wg-save wg-file)))
