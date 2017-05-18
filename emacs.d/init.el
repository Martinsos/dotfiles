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
(setq user-full-name "Martin Sosic"
      user-mail-address "sosic.martin@gmail.com")

(set-frame-font "DroidSansMono-10")

(global-auto-revert-mode t) ; Keeps buffers synced with file changes outside of emacs.

(display-time) ; Display time in mode line
(column-number-mode t) ; Column number is shown at mode line

(global-linum-mode t) ; Show line numbers

(setq-default indent-tabs-mode nil) ; Replace tabs with spaces

(windmove-default-keybindings 'meta) ; Change buffer with M + arrow

(show-paren-mode t) ; Highlight matching parent

(menu-bar-mode -1) ; remove menu bar

;; Customize GUI
(if (display-graphic-p)
  (progn
    (tool-bar-mode -1) ; remove tool bar
    (scroll-bar-mode -1))) ; remove scrolls

(add-hook 'prog-mode-hook 'subword-mode) ; Recognize subwords in camel case words.

(delete-selection-mode t) ; delete the selection with a keypress

(semantic-mode 1) ; parses current source file and provides easy local navigation/editing.

;;------ Saving files ------;;
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
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
;;--------------------------;;

;;---------------------------------------;;


(add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))


;;-------------- Packages ---------------;;
;; Dependencies are automatically installed by package.el!
;; TODO: maybe use use-package instead of req-package?

(req-package zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t)
    ))

(req-package workgroups
  :config
  (progn
    ;;; windows layout: load workgroups on start, save them on exit
    (workgroups-mode 1)
    (setq wg-file (concat user-emacs-directory "myWorkgroups"))
    (setq wg-switch-on-load nil)
    (setq wg-morph-on nil)
    (wg-load wg-file)
    (add-hook 'kill-emacs-hook (lambda () (wg-update-all-workgroups-and-save)))))

(req-package undo-tree
  :config
  (progn
    (global-undo-tree-mode)))

(req-package cmake-mode)

; Takes care of trailing whitespaces (removal, highlighting).
(req-package ethan-wspace
  :config
  (progn
    (setq mode-require-final-newline nil)
    (global-ethan-wspace-mode 1)))

(req-package ace-jump-mode
  :config
  (progn
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)))

(req-package projectile
  :config
  (progn
    (projectile-global-mode)
    ))

(req-package neotree
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

; Requirement is to have js-beautify node package installed globaly!
; Any configuration is done through .jsbeautifyrc files, that can be put inside project.
(req-package web-beautify
  :config
  (progn
    (eval-after-load 'js2-mode
      '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
    (eval-after-load 'json-mode
      '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
    (eval-after-load 'web-mode
      '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
    (eval-after-load 'css-mode
      '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))))

(req-package json-mode)

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

(req-package scss-mode)

(req-package stylus-mode)

(req-package coffee-mode
  :config
  (progn
    (custom-set-variables '(coffee-tab-width 2))))

(req-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :mode ("\\.markdown\\'" . markdown-mode))

(req-package markdown-preview-mode)

(req-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(req-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(req-package elm-mode)

(req-package cython-mode)

(req-package csharp-mode
  :mode ("\\.cs$" . csharp-mode))

;; Tide - Typescript Interactive Development Environment
(req-package tide
  :config
  (progn
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))
    (setq company-tooltip-align-annotations t) ; aligns annotation to the right hand side
    (add-hook 'before-save-hook 'tide-format-before-save) ; formats the buffer before saving
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    ;; format options
    (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
    ))

;; Displays the colors (hex and similar) with their exact color.
(req-package rainbow-mode)
;; Make sure that rainbow-mode is always on.
;; NOTE: This was not working when inside of req-package block so I put it outside.
;; I am not sure why that happens, and would like to understand it better.
(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(my-global-rainbow-mode 1)

(req-package generic-x)

;; Helm makes searching for anything nicer.
;; It works on top of many other commands / packages and gives them nice, flexible UI.
(req-package helm
  :config
  (progn
    (require 'helm-config)

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
  :require helm projectile
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    ))

;; Auto-complete.
(req-package company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))

;; On the fly syntax checking.
(req-package flycheck
  :config
  (progn
    (global-flycheck-mode)))


;; --------------------- C / C++ IDE ------------------ ;;
;; Makes emacs an awesome IDE for C/C++.
(req-package irony
  :config
  (progn
    (unless (irony--find-server-executable) (call-interactively #'irony-install-server))

    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)

    ;; Here irony will search for compilation database (compile_commands.json or .clang_complete)
    ;; in project structure and use it to fuel the auto-completion.
    ;; This compilation database has to be generated by us, this is not something irony can do.
    ;; It could be generated by cmake while building project, or using `bear` with the tool
    ;; that we are using to build the project, or created manually (.clang_complete).
    ;; Since compilation databases often do not contain information about header files,
    ;; it can also be a good option to have compile_commands.json for c(pp) files and .clang_complete
    ;; as fallback for headers.
    (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                    irony-cdb-clang-complete))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    ))

(req-package company-irony  ;; Provides company with auto-complete for C, C++ and obj-C.
  :require company irony
  :config
  (progn
    (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))))


(req-package flycheck-irony  ;; Flycheck checker for C, C++ and obj-C.
  :require flycheck irony
  :config
  (progn
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

;; Eldoc shows argument list of the function you are currently writing in the echo area.
;; irony-eldoc brings support for C, C++ and obj-C.
(req-package irony-eldoc
  :require eldoc irony
  :config
  (progn
    (add-hook 'irony-mode-hook #'irony-eldoc)))
;; ---------------------------------------------------- ;;


(req-package-finish) ; Load packages in right order.

;; Load custom made pms mode.
(load (concat user-emacs-directory "pms-mode.el"))
;;---------------------------------------;;
