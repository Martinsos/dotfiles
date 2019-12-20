;;; Temporary fix for emacs bug: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341 .
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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

(column-number-mode t) ; Column number is shown at mode line

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

(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode) ; show current function def at top.

(semantic-mode 1) ; parses current source file and provides easy local navigation/editing.

(setq magit-last-seen-setup-instructions "1.4.0")  ;; So magit does not complain.

(setq ring-bell-function 'ignore) ; So that Emacs does not produce noises all the time.

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
      auto-save-default nil
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
;;--------------------------;;

;;------- Zoom in / zoom out ------;;
(global-set-key (kbd "C-=")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (+ old-face-attribute 10)))))
(global-set-key (kbd "C--")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (- old-face-attribute 10)))))
;;---------------------------------;;

;;---------------------------------------;;


(add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))

(require 'generic-x) ; Generic Mode (for obscure languages).

;;-------------- Packages ---------------;;
;; Dependencies are automatically installed by package.el!
;; TODO: maybe use use-package instead of req-package?

(req-package zenburn-theme
  :ensure t
  :config
  (progn
    (load-theme 'zenburn t)
    ))

(req-package workgroups
  :ensure t
  :config
  (progn
    ;;; windows layout: load workgroups on start, save them on exit
    (workgroups-mode 1)
    (wg-mode-line-remove-display)
    (setq wg-file (concat user-emacs-directory "myWorkgroups"))
    (setq wg-switch-on-load nil)
    (setq wg-morph-on nil)
    (wg-load wg-file)
    (add-hook 'kill-emacs-hook (lambda () (wg-update-all-workgroups-and-save)))))

;; Smart window switching.
(req-package ace-window
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-x o") 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
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

(req-package stickyfunc-enhance :ensure t)  ; Improves semantic-stickyfunc-mode.

(req-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    ))

(req-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode)))

(req-package cmake-mode :ensure t)

; Takes care of trailing whitespaces (removal, highlighting).
(req-package ethan-wspace
  :ensure t
  :config
  (progn
    (setq mode-require-final-newline nil)
    (global-ethan-wspace-mode 1)))

(req-package ace-jump-mode
  :ensure t
  :config
  (progn
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)))

(req-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    ))

(req-package neotree
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

; Requirement is to have js-beautify node package installed globaly!
; Any configuration is done through .jsbeautifyrc files, that can be put inside project.
(req-package web-beautify
  :ensure t
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

(req-package json-mode :ensure t)

(req-package js2-mode
  :ensure t
  ;; :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    (setq js2-highlight-level 3) ; Rich highlighting
    (setq-default js2-basic-offset 2)

    ;; Don't warn about missing semicolon.
    (setq js2-strict-missing-semi-warning nil)
    (setq js2-missing-semi-one-line-override t)

    (req-package ac-js2
      :ensure t
      :config
      (progn
        (add-hook 'js2-mode-hook 'ac-js2-mode)))))

(req-package rjsx-mode
  :ensure t
  :mode ("\\.js\\'" . rjsx-mode))

(req-package vue-mode
  :ensure t
  :mode ("\\.vue\\'" . vue-mode))

(req-package haskell-mode
  :ensure t
  :config
  (progn
    (custom-set-variables
     '(haskell-indentation-layout-offset 4)
     '(haskell-indentation-starter-offset 4)
     '(haskell-indentation-left-offset 4)
     '(haskell-indentation-where-pre-offset 2)
     '(haskell-indentation-where-post-offset 2)
     )))

(req-package intero
  :ensure t
  :config
  (progn
    (add-hook 'haskell-mode-hook 'intero-mode)))

(req-package web-mode
  :ensure t
  :mode ("\\.html?\\'" . web-mode))

(req-package pug-mode :ensure t)

(req-package less-css-mode :ensure t)

(req-package scss-mode :ensure t)

(req-package stylus-mode :ensure t)

(req-package coffee-mode
  :ensure t
  :config
  (progn
    (custom-set-variables '(coffee-tab-width 2))))

(req-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :mode ("\\.markdown\\'" . markdown-mode))

(req-package markdown-preview-mode :ensure t)

(req-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(req-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(req-package elm-mode :ensure t)

(req-package cython-mode :ensure t)

(req-package csharp-mode
  :ensure t
  :mode ("\\.cs$" . csharp-mode))

;; Tide - Typescript Interactive Development Environment
(req-package tide
  :ensure t
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

;; Auto-complete.
(req-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
    (setq company-idle-delay 0)))

;; On the fly syntax checking.
(req-package flycheck
  :ensure t
  :config
  (progn
    (global-flycheck-mode)))


;; --------------------- C / C++ IDE ------------------ ;;
;; I have both irony and rtags, although they are alternatives to each other.
;; Irony consumes much less resources, however it does not have following of symbol,
;; which rtags has. Also, rtags works with headers without any problems.
;; I will keep both of them for now, so I can decide in the future which one to use for what.
;;
;; In general, I prefer irony over rtags if they work exactly the same, since irony is more lightweight.
;; both rtags and irony have good autocomplete - rtags autocomplete also works for header files
;; out of the box which is better (irony plans to support it in the future). I do have a feeling that
;; irony sometimes returns more info on autocomplete types.
;; Flychecks seem to works similarly, although some say that rtags has better reports, and I also feel
;; that might be correct.
;; Rtags has jump to definition, find references and similar stuff which irony does not have.
;;
;; Good strategy seems to be using irony for auto-complete and flycheck,
;; since irony works correct and fast for those,
;; and on the other hand using rtags for following symbols and everything else while not letting it reindex
;; each time there is a change in a file - instead having it reindex manually from time to time.

;; Makes emacs an awesome IDE for C/C++.
(req-package irony
  :ensure t
  :config
  (progn
    (unless (irony--find-server-executable) (call-interactively #'irony-install-server))

    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)

    ;; Here irony will search for compilation database (compile_commands.json or .clang_complete)
    ;; in project structure and use it to fuel the auto-completion.
    ;; This compilation database has to be generated by us, this is not something irony can do.
    ;; It could be generated by cmake while building project, or using `bear` with the tool
    ;; that we are using to build the project, or created manually (.clang_complete).
    ;; Since compilation databases often do not contain information about header files,
    ;; it can also be a good option to have compile_commands.json for c(pp) files and .clang_complete
    ;; as fallback for headers.
    ;; NOTE: rtags knows how to work with headers without .clang_complete!
    (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                    irony-cdb-clang-complete))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    ))

(req-package company-irony  ;; Provides company with auto-complete for C and C++.
  :ensure t
  :require company irony
  :config
  (progn
    (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))))

(req-package flycheck-irony  ;; Flycheck checker for C and C++.
  :ensure t
  :require flycheck irony
  :config
  (progn
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

;; Eldoc shows argument list of the function you are currently writing in the echo area.
;; irony-eldoc brings support for C and C++.
(req-package irony-eldoc
  :ensure t
  :require eldoc irony
  :config
  (progn
    (add-hook 'irony-mode-hook #'irony-eldoc)))

;; Brings google coding style to C/C++.
;; (req-package google-c-style
;;   :config
;;   (progn
;;     (add-hook 'c-mode-common-hook 'google-set-c-style)
;;     (add-hook 'c-mode-common-hook 'google-make-newline-indent)))


;; rtags indexes C / C++ projects and enables us to do stuff like auto-completion,
;; finding references / definitions and similar - it uses libclang to actually
;; "understand" the project.
;;
;; rtags is actually just an emacs client for rdm daemon, which is the main logic and
;; runs in background and re-indexes files as needed.
;; rdm and rc (general client for rdm) can be installed through emcas by running rtags-install or manually
;; (manually gives more control, and it is best to configure it as systemd socket service),
;; and we have to do that only once, when setting up emacs / rtags for the first time.
;; I like the best approach with systemd socket for now, since there I can control number of
;; processes that rtags uses. This is important because on larger projects reindexing takes a lot of
;; CPU, so it makes sense to either go with smaller number of processes or turning automatic reindexing off.
;;
;; For each new project, we have to manually register it with rdm. That is done by running
;; `rc -J <path_to_compile_commands.json>`. If you installed rtags through emacs, rc is somewhere in its internal
;; directory structure, so you have to find it to run this command. Also, make sure that rdm is running,
;; and make sure it finishes indexing.
;; rtags will make sure to automatically detect which project currently active buffer belongs to
;; and tell rdm to switch to that project.
;;
;; NOTE: I commented rtags and helm-rtags because I had not set up rtags for the project I am working on.
;;       They should be uncommented once rtags is set up.
;; (req-package rtags
;;   :config
;;   (progn
;;     (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
;;     (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

;;     (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
;;     (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
;;     (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
;;     (rtags-enable-standard-keybindings)

;;     (setq rtags-use-helm t)

;;     ;; Shutdown rdm when leaving emacs.
;;     (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
;;     ))

;; ;; TODO: Has no coloring! How can I get coloring?
;; (req-package helm-rtags
;;   :require helm rtags
;;   :config
;;   (progn
;;     (setq rtags-display-result-backend 'helm)
;;     ))

;; ;; Use rtags for auto-completion.
;; (req-package company-rtags
;;   :require company rtags
;;   :config
;;   (progn
;;     (setq rtags-autostart-diagnostics t)
;;     (rtags-diagnostics)
;;     (setq rtags-completions-enabled t)
;;     (push 'company-rtags company-backends)
;;     ))

;; ;; Live code checking.
;; (req-package flycheck-rtags
;;   :require flycheck rtags
;;   :config
;;   (progn
;;     ;; ensure that we use only rtags checking
;;     ;; https://github.com/Andersbakken/rtags#optional-1
;;     (defun setup-flycheck-rtags ()
;;       (flycheck-select-checker 'rtags)
;;       (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;       (setq-local flycheck-check-syntax-automatically nil)
;;       (rtags-set-periodic-reparse-timeout 2.0)  ;; Run flycheck 2 seconds after being idle.
;;       )
;;     (add-hook 'c-mode-hook #'setup-flycheck-rtags)
;;     (add-hook 'c++-mode-hook #'setup-flycheck-rtags)
;;     ))
;; ---------------------------------------------------- ;;


(req-package-finish) ; Load packages in right order.

;; Load custom made pms mode.
(load (concat user-emacs-directory "pms-mode.el"))
;;---------------------------------------;;
