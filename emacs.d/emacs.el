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


;;; minor modes
(column-number-mode t) ; column number is shown at mode line

;;; windows layout: load workgroups on start, save them on exit
(workgroups-mode 1)
(wg-load (concat user-emacs-directory "myWorkgroups"))
(add-hook 'kill-emacs-hook (lambda () (wg-save wg-file)))
