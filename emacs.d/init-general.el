;;; Code:

(require 'req-package)


(setq user-full-name "Martin Sosic"
      user-mail-address "sosic.martin@gmail.com")

(setq inhibit-startup-screen t) ; Don't show welcome screen.

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t) ; Start in full screen. Alternatively use toggle-frame-maximized to start maximized.

(global-auto-revert-mode t) ; Keeps buffers synced with file changes outside of emacs.

(setq-default indent-tabs-mode nil) ; Replace tabs with spaces

(windmove-default-keybindings 'meta) ; Change buffer with M + arrow

(show-paren-mode t) ; Highlight matching parenthesses.

(setq ring-bell-function 'ignore) ; So that Emacs does not produce noises all the time.

(delete-selection-mode t) ; delete the selection with a keypress

(semantic-mode 1) ; parses current source file and provides easy local navigation/editing.

(setq magit-last-seen-setup-instructions "1.4.0")  ;; So magit does not complain.

;;------ File backups and saving ------;;
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default nil
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
;;--------------------------;;

;;------- Zoom in / zoom out ------;;
(defun zoom-in ()
  "Make everything a little bit bigger."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

(defun zoom-out ()
  "Make everything a little bit smaller."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))))

(global-set-key (kbd "C-=") 'zoom-in)
(global-set-key (kbd "C--") 'zoom-out)
;;---------------------------------;;

;; Takes care of trailing whitespaces (removal, highlighting).
(req-package ethan-wspace
  :ensure t
  :config
  (progn
    (setq mode-require-final-newline NIL)
    (global-ethan-wspace-mode 1)))


(provide 'init-general)
;;; init-general.el ends here
