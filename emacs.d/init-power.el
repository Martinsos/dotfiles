;;; package --- Summary
;;;   This package brings general (power)tools (navigation, project management, ...)
;;;   that make huge difference for our emacs experience!

;;; Commentary:

;;; Code:

(require 'req-package)


;; Session / workspace manager (remembers open buffers / windows).
(req-package perspective
  :ensure t
  :config
  (progn
    (persp-mode 1)
    (setq persp-state-default-file (concat user-emacs-directory "myPerspectives"))
    (add-hook 'kill-emacs-hook 'persp-state-save)
    (persp-state-load persp-state-default-file)
  ))

;; Smart window switching.
(req-package ace-window
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-x o") 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    ))

;; Visual, navigable tree of undo-s (I love it).
(req-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode)))

(req-package ace-jump-mode  ;; You can jump to start of any token in buffer.
  :ensure t
  :config
  (progn
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)))

(req-package projectile  ;; Brings concept of "project" to emacs.
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    ))

(req-package neotree  ;; Nice file directory tree.
  :ensure t
  :require projectile
  :config
  (progn
    (setq neo-theme 'ascii)
    (setq neo-window-width 30)
    (setq neo-window-fixed-size nil)
    (setq neo-auto-indent-point t)
    (setq projectile-switch-project-action 'neotree-projectile-action)

    (defun neotree-project-dir ()
      "Open NeoTree using the git root."
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (neotree-toggle)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name)))
          (message "Could not find git project root."))))

    (global-set-key [f8] 'neotree-project-dir)))

;; Helm makes searching for anything nicer.
;; It works on top of many other commands / packages and gives them nice, flexible UI.
(req-package helm
  :ensure t
  :config
  (progn
    (require 'helm-config :ensure t)

    ;; Use C-c h instead of default C-x c, it makes more sense.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (setq
     ;; move to end or beginning of source when reaching top or bottom of source.
     helm-move-to-line-cycle-in-source t
     ;; search for library in `require' and `declare-function' sexp.
     helm-ff-search-library-in-sexp t
     ;; scroll 8 lines other window using M-<next>/M-<prior>
     helm-scroll-amount 8
     helm-ff-file-name-history-use-recentf t
     helm-echo-input-in-header-line t)

    (global-set-key (kbd "M-x") 'helm-M-x)
    (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

    (global-set-key (kbd "C-x C-f") 'helm-find-files)

    (global-set-key (kbd "M-y") 'helm-show-kill-ring)

    (global-set-key (kbd "C-x b") 'helm-mini)
    (setq helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t)

    ;; TOOD: helm-semantic has not syntax coloring! How can I fix that?

    (setq helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match t)

    ;; Lists all occurences of a pattern in buffer.
    (global-set-key (kbd "C-c h o") 'helm-occur)

    (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

    ;; open helm buffer inside current window, not occupy whole other window
    (setq helm-split-window-in-side-p t)
    (setq helm-autoresize-max-height 50)
    (setq helm-autoresize-min-height 30)
    (helm-autoresize-mode 1)

    (helm-mode 1)
    ))

(req-package helm-projectile
  :ensure t
  :require helm projectile
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    ))

;; Enables using ag for fast searching for a string through whole codebase.
;; NOTE: Needed for helm-projectile-ag command.
;; NOTE: Requires the_silver_searcher (ag) to be installed on the machine!
(req-package helm-ag :ensure t)


(provide 'init-power)
;;; init-power.el ends here
