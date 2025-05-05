;;; -*- lexical-binding: t; -*-

;; NOTE: This file was generated from Emacs.org on 2025-05-05 21:48:25 CEST, don't edit it manually.

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
      (build (expand-file-name "elpaca/" elpaca-builds-directory))
      (order (cdr elpaca-order))
      (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq elpaca-lock-file (expand-file-name "elpaca-lock.eld" user-emacs-directory))

(defun my/elpaca-write-lock-file ()
  (interactive)
  (elpaca-write-lock-file elpaca-lock-file)
)

(elpaca elpaca-use-package (elpaca-use-package-mode)) ; Install/setup use-package.
(setq use-package-always-ensure t) ; Tells use-package to have :ensure t by default for every package it manages.

(require 'cl-lib) ;; Common utilities and functions, e.g. cl-some, cl-loop, ... .

;; Package for displaying content in a nice inline overlay.
;; I use it in the rest of the config in some place(s).
(use-package quick-peek)

(defun my/var-state (var)
  "Returns the value of a variable with specified name, or 'my/var-unbound if it is not bound."
  (if (boundp var) (symbol-value var) 'my/var-unbound)
)

(defun my/local-var-state (var)
  "Returns the value of a buffer-local variable with specified name, or 'my/var-unbound if it is not bound."
  (if (local-variable-p var) (symbol-value var) 'my/var-unbound)
)

(defun set-local (var value)
  "Like setq-local but takes a var symbol (analogous to setq and set)."
  (set (make-local-variable var) value)
)

(defun my/save-local-var-state (var)
  "Save the current state of buffer-local VAR (symbol) and return a lambda that restores VAR to its original state.
USAGE:
  (let ((restore-foo (my/save-local-var-state 'foo)))
    ...
    (funcall restore-foo)
  )"
  (let ((og-state (my/local-var-state var)))
    (lambda ()
      (if (eq og-state 'my/var-unbound)
	(kill-local-variable var)
      (set-local var og-state)
    )
    )
  )
)

(defun my/save-local-vars-state (vars)
  "Like my/save-local-var-state but takes a list of vars.
USAGE:
  (let ((restore-vars (my/save-local-vars-state '(foo bar buzz))))
    ...
    (funcall restore-vars)
  )"
  (let ((restore-fns (mapcar #'my/save-local-var-state vars)))
    (lambda () (dolist (restore-fn restore-fns) (funcall restore-fn)))
  )
)

(defun my/set-local-vars-with-restore (vars-and-values)
  "Set each variable in VARS-AND-VALUES as a buffer-local variable with the specified value.
Returns a lambda that, when called, restores each variable to its original buffer-local state.
VARS-AND-VALUES should be a list of (VAR . VALUE) pairs.
USAGE:
  (let ((restore-vars (my/save-local-vars-with-restore '((foo . 42) (bar .  t)))))
    ...
    (funcall restore-vars)
  )"
  (let* ((vars (mapcar #'car vars-and-values))
	 (restore-fn (my/save-local-vars-state vars)))
    (dolist (var-and-value vars-and-values)
      (let ((var (car var-and-value))
	    (value (cdr var-and-value)))
	(set-local var value)
      )
    )
    restore-fn
  )
)

(defun my/save-mode-state (mode)
  "Save the current state (enabled or disabled) of MODE (symbol) and return a lambda that restores MODE to its original state.
USAGE:
  (let ((restore-evil-local-mode (my/save-mode-state 'evil-local-mode)))
    ...
    (funcall restore-evil-local-mode)
  )"
  (let ((og-mode-var-state (my/var-state mode)))
    (lambda ()
      (when (not (eq og-mode-var-state 'my/var-unbound))
        (funcall mode (if (eq og-mode-var-state nil) -1 1))
      )
    )
  )
)

(defun my/save-modes-state (modes)
  "Like my/save-mode-state but takes a list of modes.
USAGE:
  (let ((restore-modes (my/save-modes-state '(evil-local-mode org-tidy-mode))))
    ...
    (funcall restore-modes)
  )"
  (let ((restore-fns (mapcar #'my/save-mode-state modes)))
    (lambda () (dolist (restore-fn restore-fns) (funcall restore-fn)))
  )
)

(defun my/set-modes-with-restore (modes-and-values)
  "Set each mode in MODES-AND-VALUES with the specified value.
Returns a lambda that, when called, restores each mode to its original state (enabled/disabled).
MODES-AND-VALUES should be a list of (MODE . VALUE) pairs.
USAGE:
  (let ((restore-modes (my/save-modes-with-restore '((evil-local-mode . -1) (org-tidy-mode . 1)))))
    ...
    (funcall restore-modes)
  )"
  (let* ((modes (mapcar #'car modes-and-values))
	 (restore-fn (my/save-modes-state modes)))
    (dolist (mode-and-value modes-and-values)
      (let ((mode (car mode-and-value))
	    (value (cdr mode-and-value)))
	(funcall mode value)
      )
    )
    restore-fn
  )
)

(defun random-atom (xs)
  "Returns a random atom from the given list."
  (nth (random (length xs)) xs)
)

(defvar my-motivational-quotes
  '("\"You have power over your mind – not outside events. Realize this, and you will find strength.\" – Marcus Aurelius"
    "\"First say to yourself what you would be; and then do what you have to do.\" – Epictetus"
    "\"Waste no more time arguing about what a good man should be. Be one.\" - Marcus Aurelius"
    "\"No man is free who is not master of himself.\" – Epictetus"
    "\"Well-being is attained by little and little, and nevertheless is no little thing itself.\" – Zeno of Citium"
    "\"Don’t explain your philosophy. Embody it.\" - Epictetus"
    "\"No great thing is created suddenly.\" – Epictetus"
    "\"Begin at once to live, and count each separate day as a separate life.\" – Seneca"
    "\"Dwell on the beauty of life. Watch the stars, and see yourself running with them.\" – Marcus Aurelius"
    "\"Nulla dies sine linea.\" - Pliny the Elder"
    )
)

(use-package emacs
  :ensure nil
  :config
  (setq inhibit-startup-message t)
  (defun display-startup-echo-area-message () (message nil))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (setq ring-bell-function 'ignore)
  (set-fringe-mode 10)
  (setq-default fill-column 100)
  (column-number-mode) ; Show row:column in mode line.
  (visual-line-mode 1) ; Treat wrapped lines as multiple lines when moving around.
  (global-hl-line-mode 1) ; Highlights the line in which cursor is.
  (global-auto-revert-mode t) ; Automatically reload files if they change on disk (will ask if conflict).
  (add-hook 'window-setup-hook 'toggle-frame-fullscreen t) ; Start in fullscreen.
  (setq-default indent-tabs-mode nil) ; Don't use tabs when indenting.
  (delete-selection-mode t) ; Delete the selection with a keypress.

  ;; Sets default font (at size 10). I use Monaspace font (Neon variant) which was created by
  ;; "Github Next" -> Github's dev tools research team. I specifically installed otf "frozen"
  ;; version that comes with all the goodies baked in (check below under Manual Setup).
  ;; Some other nice fonts: "RobotoMono Nerd Font" (best right after Monaspace),
  ;; "Source Code Pro", "Noto Sans Mono".
  ;; NOTE: Monaspace has very cool "texture healing" feature where some letters are widened or narrowed
  ;;   when there is space due to the neighbouring letters, but Emacs doesn't (yet, there is a todo))
  ;;   support OTF's "contextual alternate" feature that is needed for this.
  ;;   If it does support it one day, I should enable it to reap all the benefits of Monaspace font.
  (set-face-attribute 'default nil :family "Monaspace Neon" :height 100)

  (setq gc-cons-threshold 100000000) ; Default is low, so we set it to 100mb. Helps with e.g. lsp-mode.
  (setq read-process-output-max (* 1024 1024)) ;; Default is low, so we set it to 1mb. Helps with e.g. lsp-mode.

  (setq initial-major-mode 'org-mode) ; Start Scratch buffer with Org mode.
  (setq initial-scratch-message (concat "# " (random-atom my-motivational-quotes) "\n\n"))

  (global-subword-mode) ; Makes vim motions treat subwords in camelCase as individual words.

  ;; Emacs by default creates different kinds of additional files on the disk while editing.
  ;; While they can be useful in theory, I found that I don't have much need for any of them,
  ;; while on the other hand they litter the disk and are a potential security issue if they contain
  ;; sensitive information copied from the files that were being edited.
  (setq make-backup-files nil) ; I either have files version controlled or I will manually create backup.
  (setq auto-save-default nil) ; I save so often myself that I don't have a need for this.
  (setq create-lockfiles nil) ; I don't have a situation where multiple emacses want to edit the same file.

  (setq custom-file "/dev/null") ; Prevent emacs from adding `customize` system choices to my init.el.
)

(use-package epg-config
  :ensure nil ; emacs built-in
  :config
  ; Makes emacs query the passphrase via minibuffer, instead of external program.
  (setq epg-pinentry-mode 'loopback)
)

(use-package plstore
  :ensure nil ; emacs built-in
  :config
  ; Stops plstore from asking for passphrase many times, instead it caches it and reuses it.
  ; NOTE: If this stops working, alternative is to set it to use GPG key for encryption.
  ;       Check plstore code for instructions on this if it will ever be needed.
  (setq-default plstore-cache-passphrase-for-symmetric-encryption t)
)

;; doom-themes have nice, high quality themes.
(use-package doom-themes
  :ensure (:wait t) ; Too ensure theme gets loaded as early as possible, so there is no white scren.
  :config
  ;; I went with moonlight for now. palenight is also nice.
  ;; TODO: Figure out where and how is the best way to do theme customization. I am guessing it shoudl be happening in a central place,
  ;;   even if it is about other packages faces, and that it should happen next to loading of the theme?
  (load-theme 'doom-moonlight t)
)

;; Nice themes by Prot.
;; `ef-dream' is nice, also `ef-night'.
(use-package ef-themes
  :ensure (:wait t) ; Too ensure theme gets loaded as early as possible, so there is no white scren.
)

;; TODO: Configure better or use some other modeline.
(use-package doom-modeline
  :custom
  (doom-modeline-height 40)
  (doom-modeline-buffer-encoding nil)
  :config
  (doom-modeline-mode 1)
)

(use-package emacs
  :ensure nil
  :config
  ;; Default emacs undo limits are quite low so we increase them here.
  (setq undo-limit         50000000) ; ~50mb.
  (setq undo-strong-limit 100000000) ; ~100mb.
  (setq undo-outer-limit  300000000) ; ~300mb.
)

(use-package undo-fu
  :config
  (setq undo-fu-ignore-keyboard-quit t) ; I don't want C-g to trigger normal emacs undo behavior.
)

;; Displays undo history as a tree and lets you move through it.
(use-package vundo
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)

  ;;;;;; Vundo Live Diff ;;;;;;
  ;; In vundo, you have to manually mark one node and call diff on another node to get their diff.
  ;; Here we extend vundo to have "live diff mode", that always shows diff between current node and its parent.
  ;; I turn it on by default. It can be toggled by pressing "F".
  (defun vundo-live-diff-post-command ()
    "Post command hook function for live diffing."
    (when (not (memq this-command '(vundo-quit vundo-confirm)))
      (progn
        (vundo-diff-mark (vundo-m-parent (vundo--current-node vundo--prev-mod-list)))
        (vundo-diff)
      )
    )
  )
  (define-minor-mode vundo-live-diff-mode
    "Shows live diff between the current node and its parent."
    :lighter nil
    (if vundo-live-diff-mode
      (add-hook 'post-command-hook #'vundo-live-diff-post-command 0 t)
      (remove-hook 'post-command-hook #'vundo-live-diff-post-command t)
    )
  )
  (evil-define-key 'normal vundo-mode-map (kbd "F") #'vundo-live-diff-mode)
  (add-hook 'vundo-mode-hook (lambda () (vundo-live-diff-mode 1)))
  ;;;;;/ Vundo Live Diff ;;;;;;
)

;; general.el provides convenient, unified interface for key definitions.
;; It can do many cool things, one of them is specifying leader key and prefixes.
;; For best results, you should do all/most of the key defining via general (e.g. `general-define-key`).
;; NOTE: I found general.el to be really complex, and I haven't invested the time to properly understand it.
;;   Therefore, I don't completely understand if the config below is written in the best way, but
;;   it was recommended by others and it seems to work.
(use-package general
  :ensure (:wait t) ; Load it immediately, so that I can use :general keyword in use-package declarations below if I want.
  :config
  (general-evil-setup t)

  ;; Here we use the following two expressions to create a function ~my/leader-keys~ that we can then use to created
  ;; new keys that all start with SPC prefix (leader key).
  (general-define-key
    :prefix-map 'my/leader-map
    :states '(motion normal insert visual emacs)
    :keymaps 'override ; Override any other keymaps with same keybindings. Otherwise I get issues with the `motion` and SPC, since `motion` already defines keybindings for SPC.
    :prefix "SPC" ; This will be active only in "normal"-like states (so `normal`, `motion` and `emacs`).
    :global-prefix "C-SPC" ; This will be always active.
  )
  (general-create-definer my/leader-keys
    :keymaps 'my/leader-map)

  ;; TODO: Also, should I use :general keyword in use-package? Figure this out, the best way to define keybindings with SPC prefix,
  ;;   should they all be here, or in their respective packages, or what.
  (my/leader-keys
    "SPC" '("M-x (exec cmd)" . counsel-M-x)
    "TAB" '("previous buffer" . my/alternate-buffer)
    "RET" '("work diary" . (lambda () (interactive) (org-agenda nil "d")))

    "^"   '("top-level keybindings" . which-key-show-top-level)

    "C-u" '("universal argument" . universal-argument)

    "0"   '("jump to window 0" . winum-select-window-0)
    "1"   '("jump to window 1" . winum-select-window-1)
    "2"   '("jump to window 2" . winum-select-window-2)
    "3"   '("jump to window 3" . winum-select-window-3)
    "4"   '("jump to window 4" . winum-select-window-4)

    ;; TODO: When inside counsel-projectile-rg, you can do C-c C-o to persist the search results in a special buffer,
    ;;   and then in that buffer you can press enter on any of them and jump to that location.
    ;;   This is awesome, but how will I remember this? Somehow help myself remember this. Another candidate for "hint"/"help" zone?
    ;;   Btw Helm (in Spacemacs) has this bar at the bottom where it shows which command was just run and some hints (C-z for actions, ...).
    ;;   Is this something I can replicate, at least for Ivy?
    "/"   '("search in project" . counsel-projectile-rg)
    "*"   '("search in project w/input" . counsel-projectile-rg-region-or-symbol)

    "`"   '("mark ring" . counsel-mark-ring)

    "t"   '("toggles" . (keymap)) ; This is how prefix is defined.
    "tt"  '("choose theme" . counsel-load-theme)
    "ts"  '("scale text" . hydra-text-scale/body)

    "a"   '("apps" . (keymap))
    "au"  '("undo tree" . vundo)

    "af"  '("fun" . (keymap))
    "afa" 'animate-birthday-present
    "afb" 'blackbox
    "afc" 'butterfly
    "afd" 'doctor
    "afe" 'bubbles
    "aff" 'dunnet
    "afg" 'gnugo
    "afh" 'hanoi
    "afi" 'gomoku
    "afj" 'solitaire
    "afl" 'life
    "afp" 'pong
    "afs" 'snake
    "aft" 'tetris
    "afx" '5x5
    "afz" 'zone

    "q"   '("quit" . (keymap))
    "qq"  '("quit" . save-buffers-kill-terminal)
    "qr"  '("restart" . restart-emacs)

    "w"   '("windows" . (keymap))
    "ww"  '("other window" . ace-window)
    "wd"  '("delete window" . delete-window)
    "wx"  '("delete window and buffer" . kill-buffer-and-window)
    "w/"  '("split vertically" . split-window-right)
    "w-"  '("split horizontally" . split-window-below)
    "wr"  '("resize window" . hydra-window-resize/body)
    "wm"  '("move window" . hydra-window-move/body)
    "w."  '("focus window" . delete-other-windows)
    "w="  '("balance window sizes" . balance-windows)

    "b"   '("buffers" . (keymap))
    "bb"  '("switch buffer" . ivy-switch-buffer)
    "bd"  '("kill buffer" . kill-current-buffer)
    "bs"  '("go to scratch" . scratch-buffer)
    "bm"  '("go to messages" . my/switch-to-messages-buffer)
    "bp"  '("previous buffer" . hydra-buffer-next-prev/previous-buffer)
    "bn"  '("next buffer" . hydra-buffer-next-prev/next-buffer)
    "br"  '("reload buffer" . revert-buffer)
    "bv"  '("select whole buffer" . mark-whole-buffer)

    "e"   '("errors" . (keymap))

    "f"   '("files" . (keymap))
    "fj"  '("jump in file" . avy-goto-char-timer)
    "ff"  '("find file" . counsel-find-file)
    "fs"  '("save" . save-buffer)
    "fr"  '("recent files" . counsel-recentf)

    "fe"  '("emacs" . (keymap))
    "feo" '("open Emacs.org file" . my/open-emacs-org-file)
    "fei" '("open init.el file" . my/open-init-file)

    "i"   '("ai" . (keymap))

    "v"   '("eval (elisp)" . (keymap))
    "vl"  '("last-sexp" . eval-last-sexp)
    "vv"  '("top-level form" . eval-defun)
    "vr"  '("region" . eval-region)
    "v:"  '("expression" . eval-expression)

    "o"   '("org" . (keymap))
    "oa"  '("agenda" . org-agenda)
    "oc"  '("capture" . org-capture)
    "ol"  '("store link" . org-store-link)

    "p"   '("projects" . (keymap))
    "pf"  '("find file" . counsel-projectile-find-file)
    "pd"  '("find dir" . projectile-find-dir)
    "pb"  '("switch buffer" . projectile-switch-to-buffer)
    "pp"  '("switch project" . counsel-projectile-switch-project)
    "pr"  '("find and replace" . projectile-replace)
    "p."  '("all commands" . projectile-command-map)
  )

  (general-define-key
    :states '(normal visual)
    :keymaps 'override
    "p" 'my/paste-after-then-hydra
    "P" 'my/paste-before-then-hydra
  )
)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-add-column-padding 2)
  (setq which-key-min-display-lines 5)
  (which-key-mode)
)

(use-package hydra
  :config
  (defhydra hydra-text-scale ()
    "Scale text"
    ("j" text-scale-decrease "out")
    ("k" text-scale-increase "in")
    ("r" (progn (text-scale-increase 0)) "restore")
    ("q" nil "quit" :exit t)
  )

  (defhydra hydra-buffer-next-prev ()
    "Next/previous buffer"
    ("p" previous-buffer "previous")
    ("n" next-buffer "next")
    ("q" nil "quit" :exit t)
  )

  (defhydra hydra-paste ()
    "Choose what to paste from the kill ring"
    ("C-j" evil-paste-pop "previous")
    ("C-k" evil-paste-pop-next "next")
    ("/" (progn
          (evil-undo-pop) ; Undo last paste.
          ;; NOTE: Ideally, I would put the new paste (about to be selected by counsel-yank-pop)
          ;; starting exactly from the same place as previous paste, as e.g. evil-paste-pop does,
          ;; but I haven't found a simple way to implement that, so I do a more simplistic
          ;; approach, below that is a bit less precise (e.g. adds redundant newline). However I
          ;; don't think that is a big problem if one decided to browse kill ring visually, you
          ;; care less about speed / formatting then.
          (goto-char (nth 2 evil-last-paste)) ; Put cursor back where it was before the last paste.
          (if (eq last-command 'evil-paste-before)
              (evil-insert-newline-above)
              (evil-insert-newline-below)
          )
          (counsel-yank-pop) ; Browse kill ring, pick entry and paste it.
        )
        "browse"
    )
    ("q" nil "quit" :exit t)
  )
  (defun my/paste-after-then-hydra ()
    (interactive)
    (call-interactively 'evil-paste-after)
    (hydra-paste/body)
    ;; This way this command is recognized as evil-paste-after, making evil-paste-after a last-command,
    ;; which is a requirement for evil-paste-pop functions from hydra-paste to be able to be executed
    ;; after this one.
    (setq this-command 'evil-paste-after)
  )
  (defun my/paste-before-then-hydra ()
    (interactive)
    (call-interactively 'evil-paste-before)
    (hydra-paste/body)
    (setq this-command 'evil-paste-before)
  )

  (defhydra hydra-window-resize (:hint nil)
    "
  Resize window
  -------------
              _h_: ⇾ ⇽          ↑        ↓
                            _k_:     _j_:
              _l_: ⇽ ⇾          ↓        ↑
  "
    ("h" shrink-window-horizontally)
    ("j" shrink-window)
    ("k" enlarge-window)
    ("l" enlarge-window-horizontally)
    ("q" nil "quit" :exit t)
  )

  (defhydra hydra-window-move (:hint nil)
    "
  Move window
  -----------
                      _k_: top
              _h_: left       _l_: right
                      _j_: bottom
  "
    ("h" evil-window-move-far-left)
    ("j" evil-window-move-very-bottom)
    ("k" evil-window-move-very-top)
    ("l" evil-window-move-far-right)
    ("q" nil "quit" :exit t)
  )
)

;; CHEATSHEET: C-z puts us into `emacs` mode, which is normal situation without evil.
(use-package evil
  :init
  (setq evil-want-integration t)  ; Required by evil-collection.
  (setq evil-want-keybinding nil) ; Required by evil-collection.
  (setq evil-undo-system 'undo-fu)
  ;; C-u-scroll needs explicit enabling because in Emacs C-u is important, it is
  ;; universal argument. But I don't use it much, so I rather go with vi's
  ;; scroll, which I use a lot. I anyway mapped universal argument to also be SPC C-u.
  (setq evil-want-C-u-scroll t)
  :custom
  (evil-shift-width 2) ; When shifting text left or right with < or >, do it for 2 spaces.
  :config
  (evil-mode 1)
  (define-key evil-motion-state-map (kbd "SPC") nil) ; To avoid conflict with me using SPC as leader key (defined via general.el).
  (define-key evil-motion-state-map (kbd ",") nil) ; I prefer using "," for mode-specific keymap (e.g. for lsp).
)

(use-package evil-escape
  :after evil
  :custom
  (evil-escape-key-sequence "fd")
  :config
  (evil-escape-mode)
)

;; Sets evil keybindings in many more parts of emacs than evil-mode does by default,
;; and in a better way than evil does.
(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer nil)  ; If set to `t` it messes up / overrides my custom keybindings for Ivy (e.g. C-k).
  :config (evil-collection-init)
)

(use-package org
  :defer t
  :hook
  (org-mode . (lambda ()
    (org-indent-mode) ; Enforces correct indentation under each heading.
    (visual-line-mode 1)
    (setq evil-auto-indent nil)
  ))
  :config
  (general-define-key
   :states '(normal)
   :keymaps 'org-mode-map
   :prefix ","
   "cs" 'org-schedule
   "cd" 'org-deadline
   "ct" 'org-set-tags-command
   "ce" 'org-set-effort
   "cb" 'org-toggle-checkbox

   "J" 'org-priority-down
   "K" 'org-priority-up
   "t" 'org-todo
  )

  ;; Set headers to have different sizes.
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :height (cdr face))
  )

  (setq org-log-into-drawer t)
  (setq org-habit-graph-column 60)

  (setq org-ellipsis " ▼")
  (set-face-attribute 'org-ellipsis nil :foreground (face-attribute 'shadow :foreground))

  (add-to-list 'org-modules
	'org-habit
  )

  (setq org-priority-faces '((?A . (:foreground "#ff757f" :weight normal))
                             (?B . (:foreground "orange" :weight light))
                             (?C . (:foreground "yellow" :weight light))))
)

;; Replace header and list bullets (*, **, -, +, ...) with nice bullets.
(use-package org-superstar
  :after (org)
  :defer t
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; I use `org-superstar-leading-bullets' to hide leading stars of the heading by setting them to space.
  ;; I use two spaces because that is how they have shown it should be done in the docs and it works well.
  ;; Why don't I use `org-superstar-remove-leading-stars', which should hide them for real?
  ;; Because when I tried using it, it resulted in headings being shifted too much to the left.
  (org-superstar-leading-bullet "  ")
  (org-superstar-item-bullet-alist '((?* . ?★) (?+ . ?✦) (?- . ?•)))
  :config
  (set-face-attribute 'org-superstar-item nil :foreground (face-attribute 'font-lock-keyword-face :foreground))
)

;; Org Tempo expands snippets to structures defined in org-structure-template-alist and org-tempo-keywords-alist.
(use-package org-tempo
  :after (org)
  :ensure nil ; Comes with org already.
)

; Colors tags in org mode with "random" colors based on their string hash.
(use-package org-rainbow-tags
  :after (org)
  :hook ((org-mode org-agenda-finalize) . org-rainbow-tags-mode)
  :custom
  (org-rainbow-tags-extra-face-attributes '(:slant 'italic :weight 'normal))
  (org-rainbow-tags-adjust-color-percent 100) ; Make colors as light as possible, so they work well with dark bg.
)

;; Display "prettified" pieces of text in their raw shape when point is on them.
;; E.g. links or superscript.
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t)
  (org-appear-trigger 'always)
  ;; Make bold and italic and similar nice, since we now have org-appear
  ;; to show them as raw when needed.
  (org-hide-emphasis-markers t)
)

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
)

(with-eval-after-load 'org
  ;; Here we define our custom structure templates (snippets) for quickly creating code blocks.
  ;; Typing e.g. "<elTAB" will expand it to snippet.
  (dolist (key-to-block-type '(("sh" . "src shell")
                               ("el" . "src emacs-lisp")
			       ("py" . "src python")))
    (add-to-list 'org-structure-template-alist key-to-block-type)
  )
  
  ;; Define which languages can be evaluated/executed in org files.
  ;; Org will load support for them.
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t))
  )

  (setq org-confirm-babel-evaluate nil) ; Don't ask for confirmation when evaluation a block.
)

(use-package org-gcal
  :init
  ;; Get calendar credentials from .authinfo file and use them.
  (let* ((gcal-auth-info (car (auth-source-search :host "gcal" :max 1 :require '(:user :secret)))))
    (setq org-gcal-client-id (plist-get gcal-auth-info :user)
          org-gcal-client-secret ((lambda (x) (if (functionp x) (funcall x) x))
                                  (plist-get gcal-auth-info :secret))
    )
  )
  ;; First elements of the pairs here are ids of google calendars.
  (setq my/calendar-events-wasp-org-file "~/Dropbox/calendar-events-wasp.org")
  (setq my/calendar-events-private-org-file "~/Dropbox/calendar-events-private.org")
  (setq org-gcal-fetch-file-alist `(("martin@wasp-lang.dev" . ,my/calendar-events-wasp-org-file)
				    ("sosic.martin@gmail.com" . ,my/calendar-events-private-org-file)
				    ))
)

(with-eval-after-load 'org
  (setq org-agenda-scheduled-leaders '("-> " "-%dd -> "))
  (setq org-agenda-deadline-leaders '("! " "+%dd ! " "-%dd ! "))
  ;; Make the current time in the time-grid (<- now --------) stand out.
  (set-face-attribute 'org-agenda-current-time nil
                      :foreground "#9a93cf" ;; Obtained by making org-time-grid face a bit purple.
                      :weight 'bold)
  ;; Make events in the time grid that are not scheduled tasks not stand out.
  (set-face-attribute 'org-agenda-calendar-event nil
                      :foreground (face-attribute 'org-time-grid :foreground))
)

(defvar my/after-org-agenda-mark-todo-deadlines-with-earlier-schedule-hook nil
  "Hook called after the marking of the todo deadlines with the earlier schedule")

(defun my/org-agenda-mark-todo-deadlines-with-earlier-schedule ()
  "Mark all todo deadline entries in agenda that have earlier schedule.
It will both mark them with a text property and also style them to be less emphasized."
  (save-excursion
    (while (< (point) (point-max))
      (let* ((entry-type (org-get-at-bol 'type))
             (entry-is-deadline (string= entry-type "deadline"))
             ;; org-hd-marker returns position of header in the original org buffer.
             (entry-marker (org-get-at-bol 'org-hd-marker))
             (entry-scheduled-time-str (when entry-marker (org-entry-get entry-marker "SCHEDULED")))
             (entry-deadline-time-str (when entry-marker (org-entry-get entry-marker "DEADLINE")))
             (entry-todo-state (org-get-at-bol 'todo-state))
             (entry-is-done (when entry-todo-state
                             (member entry-todo-state org-done-keywords-for-agenda)))
             (entry-is-todo (when entry-todo-state (not entry-is-done)))
             (entry-scheduled-before-deadline
              (and entry-scheduled-time-str
                    entry-deadline-time-str
                    (< (org-time-string-to-absolute entry-scheduled-time-str)
                      (org-time-string-to-absolute entry-deadline-time-str)
                    )
              )
             )
            )
        (when (and entry-is-deadline entry-is-todo entry-scheduled-before-deadline)
          (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ov 'face '(:weight extra-light :slant italic))
            (overlay-put ov 'category 'my-agenda-todo-deadline-with-earlier-schedule)
            (put-text-property (line-beginning-position) (line-end-position) 'is-todo-deadline-with-earlier-schedule t)
          )
        )
      )
      (forward-line)
    )
  )
  (run-hooks 'my/after-org-agenda-mark-todo-deadlines-with-earlier-schedule-hook)
)

(add-hook 'org-agenda-finalize-hook 'my/org-agenda-mark-todo-deadlines-with-earlier-schedule)

(require 'cl-lib)

(defun my/org-agenda-calculate-total-leftover-effort-today (point-limit)
  "Sum the leftover org agenda entries efforts for today from the current point till the POINT-LIMIT.
Return minutes (number)."
  (let (efforts)
    (save-excursion
      (while (< (point) point-limit)
        (let* ((entry-type (org-get-at-bol 'type))
               ;; org-hd-marker returns position of header in the original org buffer.
               (entry-marker (org-get-at-bol 'org-hd-marker))
               (entry-scheduled-time-str (when entry-marker (org-entry-get entry-marker "SCHEDULED")))
               (entry-deadline-time-str (when entry-marker (org-entry-get entry-marker "DEADLINE")))
               (entry-todo-state (org-get-at-bol 'todo-state))
               (entry-is-done (when entry-todo-state
                                (member entry-todo-state org-done-keywords-for-agenda)))
               (entry-is-todo (when entry-todo-state (not entry-is-done)))
              )
          (when (and entry-is-todo
                     ;; We intentionally didn't include the "upcoming deadline" entries.
                     (or (member entry-type '("scheduled" "past-scheduled" "timestamp"))
                         ;; We count only deadlines that also have a scheduled time on that same day.
                         ;; This is specific to how my agenda is set up: Such deadline entries will
                         ;; appear under the list of tasks for today, so we want to count them.
                         (and (string= entry-type "deadline")
                              entry-scheduled-time-str
                              entry-deadline-time-str
                              (= (org-time-string-to-absolute entry-scheduled-time-str)
                                 (org-time-string-to-absolute entry-deadline-time-str)
                              )
                         )
                     )
                )
            (push (org-entry-get entry-marker "Effort") efforts)
          )
        )
        (forward-line)
      )
    )
    (cl-reduce #'+
               (mapcar #'org-duration-to-minutes (cl-remove-if-not 'identity efforts))
               :initial-value 0
    )
  )
)

(defun my/org-agenda-insert-total-daily-leftover-efforts ()
  "Insert the total scheduled effort for each day inside the agenda buffer."
  (save-excursion
    (let (curr-date-header-pos)
      (while (setq curr-date-header-pos (text-property-any (point) (point-max) 'org-agenda-date-header t))
        (goto-char curr-date-header-pos)
        (end-of-line)
        (let* ((next-date-header-pos (text-property-any (point) (point-max) 'org-agenda-date-header t))
               (total-effort (my/org-agenda-calculate-total-leftover-effort-today
                              (or next-date-header-pos (point-max))))
              )
          (insert-and-inherit (concat " (∑⏳ = " (org-duration-from-minutes total-effort) ")"))
        )
        (forward-line)
      )
    )
  )
)

(add-hook 'org-agenda-finalize-hook 'my/org-agenda-insert-total-daily-leftover-efforts)

(use-package org-super-agenda
  :after org
  :init 
  ;; org-super-agenda-header-map is keymap for super agenda headers and normally it just copies keybindings
  ;; from org-agenda-mode-map, but since I modify those later with evil-org, then I don't want
  ;; org-super-agenda-header-map sticking to the old keybindings and having super agenda headers behave
  ;; in default, non-evil way (e.g. "j" when on them doesn't move down but opens calendar).
  ;; I haven't managed to figure out how to update it to behave in an evil fashion, so I ended up just disabling
  ;; it completely, and that works great.
  (setq org-super-agenda-header-map nil)
  (setq org-super-agenda-keep-order t) ; Can degrade performance, which is why it isn't enabled by default.
  :config
  (org-super-agenda-mode)
)

(with-eval-after-load 'evil
  ;; TODO: I am basing these keybindings on the evil-org-agenda-set-keys function from
  ;;   evil-org-agenda.el (from evil-org package), but I copied them directly here so I can easily modify
  ;;   them as I wish.

  (evil-set-initial-state 'org-agenda-mode 'motion)

  (evil-define-key 'motion org-agenda-mode-map
    ;; Opening org file.
    (kbd "<tab>") 'org-agenda-goto
    (kbd "RET") 'org-agenda-switch-to
    (kbd "M-RET") 'org-agenda-recenter

    ;; Motion.
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    "gH" 'evil-window-top
    "gM" 'evil-window-middle
    "gL" 'evil-window-bottom
    (kbd "C-j") 'org-agenda-next-item
    (kbd "C-k") 'org-agenda-previous-item
    (kbd "[[") 'org-agenda-earlier
    (kbd "]]") 'org-agenda-later

    ;; manipulation
    ;; We follow standard org-mode bindings (not org-agenda bindings):
    ;; <HJKL> change todo items and priorities.
    ;; M-<jk> drag lines.
    ;; M-<hl> cannot demote/promote, we use it for "do-date".
    "J" 'org-agenda-priority-down
    "K" 'org-agenda-priority-up
    "H" 'org-agenda-do-date-earlier
    "L" 'org-agenda-do-date-later
    "t" 'org-agenda-todo
    (kbd "M-j") 'org-agenda-drag-line-forward
    (kbd "M-k") 'org-agenda-drag-line-backward
    (kbd "C-S-h") 'org-agenda-todo-previousset ; Original binding "C-S-<left>"
    (kbd "C-S-l") 'org-agenda-todo-nextset ; Original binding "C-S-<right>"

    ;; undo
    "u" 'org-agenda-undo

    ;; actions
    "dd" 'org-agenda-kill
    "dA" 'org-agenda-archive
    "da" 'org-agenda-archive-default-with-confirmation
    "ct" 'org-agenda-set-tags
    "ce" 'org-agenda-set-effort
    "cs" 'org-agenda-schedule
    "cd" 'org-agenda-deadline
    "cT" 'org-timer-set-timer
    "i" 'org-agenda-diary-entry
    "a" 'org-agenda-add-note
    "A" 'org-agenda-append-agenda
    "C" 'org-agenda-capture
    "e" 'org-agenda-tree-to-indirect-buffer
    "o" 'org-agenda-goto

    ;; mark
    "m" 'org-agenda-bulk-toggle
    "~" 'org-agenda-bulk-toggle-all
    "%" 'org-agenda-bulk-mark-regexp
    "x" 'org-agenda-bulk-action

    ;; refresh
    "gr" 'org-agenda-redo
    "gR" 'org-agenda-redo-all

    ;; quit
    "ZQ" 'org-agenda-exit
    "ZZ" 'org-agenda-quit

    ;; display
    "gD" 'org-agenda-view-mode-dispatch
    "ZD" 'org-agenda-dim-blocked-tasks

    ;; clock
    "I" 'org-agenda-clock-in ; Original binding
    "O" 'org-agenda-clock-out ; Original binding
    "cg" 'org-agenda-clock-goto
    "cc" 'org-agenda-clock-cancel
    "cr" 'org-agenda-clockreport-mode

    ;; go and show
    "." 'org-agenda-goto-today
    "gc" 'org-agenda-goto-calendar
    "gC" 'org-agenda-convert-date
    "gd" 'org-agenda-goto-date
    "gh" 'org-habit-stats-view-habit-at-point-agenda
    "gm" 'org-agenda-phases-of-moon
    "gs" 'org-agenda-sunrise-sunset
    "gt" 'org-agenda-show-tags
    "ge" 'org-agenda-entry-text-mode

    "p" 'org-agenda-date-prompt
    "P" 'org-agenda-show-the-flagging-note

    "+" 'org-agenda-manipulate-query-add
    "-" 'org-agenda-manipulate-query-subtract
  )
)

;; I wait for org-gcal because in :init of org-gcal I define vars that hold paths to files with
;; calendar events, and I need to know those paths so I can show events in the agenda.
(with-eval-after-load 'org (with-eval-after-load 'org-gcal

  (defun my/make-work-diary-cmd-agenda-block-base-settings (show-daily-checklist show-other-tasks)
    "Base settings for the agenda block in my work-diary custom agenda commands."
    `((org-agenda-prefix-format " %12s %5e %?-12t")
      (org-agenda-sorting-strategy '(time-up
                                     todo-state-down
                                     priority-down
                                     scheduled-up
                                     urgency-down)
      )
      (org-super-agenda-groups
       '(
         ,@(if show-daily-checklist
               '(;; Repeating tasks to be done every day, including today.
                 (:name "Daily Checklist"
                        :and (:category "dc"
                              :not (:log t))
                 )
                )
             '()
           )
         (:name "Todo (today)"
                :and (:time-grid t :not (:log t))
         )
         (:name "        No specific time:"
                :and (:category "task"
                      :scheduled t
                      :not (:log t))
         )
         ;; Discard "closed" logs for items scheduled for today because they will be shown
         ;; as done already above, so we don't want to repeat it.
         ;; NOTE: Due to this bug in super-agenda https://github.com/alphapapa/org-super-agenda/issues/42,
         ;;   `:scheduled today` works as you would expect only when agenda is actually focused on today,
         ;;   because today means the actual day today, not the day that agenda daily view is focusing on.
         ;;   Therefore if I look at yesterday, this discarding doesn't work and I get double done entries.
         (:discard (:and (:scheduled today :log closed)))
         (:name none
                 :and (:category "task"
                       :log closed)
         )
         (:name "Clock log"
                :log clocked
         )
         ,@(if show-other-tasks
               '(
                 (:name "Other (e.g. deadline w/o scheduled)"
                        :anything t
                 )
                )
             '(
               (:discard (:anything t))
              )
           )
       )
      )
     )
  )

  (defun my/make-work-diary-cmd-base-settings ()
    "Base settings for my work-diary custom agenda commands."
    `((org-agenda-files `("~/Dropbox/work-diary.org"
                          ,my/calendar-events-wasp-org-file
                          ,my/calendar-events-private-org-file
                         ))

      ;; Starts agenda log mode, which means that special extra "log" entries are added to agenda,
      ;; in this logs about closing an entry and logs about clocking an entry. I could also have
      ;; added 'state' if needed. I track "closed" logs in order to ensure that entries that are
      ;; DONE but have been scheduled in the past are shown in agenda (normally they are
      ;; not). What is not great is that they are not normal but special log entries which are a
      ;; bit different, so a bit harder to organize.
      (org-agenda-start-with-log-mode '(closed clock))

      ;; Org agenda shows both scheduled and deadline entries for an item, when available.
      ;; I don't want that: having duplicate entries for the same item is confusing.
      ;; With settings below, I have the following behaviour when item is both scheduled and has
      ;; deadline:
      ;;  - If it is not deadline yet, then only scheduled entry is shown (be it late or not).
      ;;  - If the deadline is today or has passed, only deadline entry is shown.
      ;; NOTE: (org-agenda-skip-deadline-prewarning-if-scheduled t) will still show both scheduled
      ;;   and deadline entry if the deadline is today. That is why we also need
      ;;   (org-agenda-skip-scheduled-if-deadline-is-shown t), to solve that case.
      (org-agenda-skip-deadline-prewarning-if-scheduled t)
      (org-agenda-skip-scheduled-repeats-after-deadline t)
      (org-agenda-skip-scheduled-if-deadline-is-shown t)

      (org-todo-keyword-faces
       '(("EPIC" . (:foreground "orchid" :weight bold))
         ("INPR" . (:foreground "dark orange" :weight bold))
         ;; I got #bf6900 by darkening the "dark orange" which allegedly is #ff8c00.
         ("BLCK" . (:foreground "#bf6900" :weight bold :strike-through t))
         ("CANCELED" . (:foreground "dim gray" :weight bold :strike-through t))
         ("CANCELED[EPIC]" . (:foreground "dim gray" :weight bold :strike-through t))
         ("CHKL" . (:foreground "grey" :weight bold))
         ("NOTE" . (:foreground "white" :weight bold))
         ;; I obtained #446a73 by adding a bit of green to the color of org-agenda-done face.
         ("DONE" . (:foreground "#446a73" :weight bold))
        )
      )
     )
  )

  (defun my/make-work-diary-day-cmd (cmd-key cmd-name cmd-start-day)
    `(,cmd-key ,cmd-name
       (;; The main view: a list of tasks for today.
	(agenda ""
		(,@(my/make-work-diary-cmd-agenda-block-base-settings t t)
                 (org-agenda-span 'day)
		 (org-habit-show-all-today t)
                 (org-agenda-time-grid '((daily remove-match)
                                         (800 1000 1200 1400 1600 1800 2000)
                                         " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"
                                        )
                 )
		)
	)
	(alltodo ""
		 ((org-agenda-overriding-header "")
		  (org-agenda-prefix-format " %5e ")
		  (org-super-agenda-groups
		   '((:name "Notes"
  			    :category "note"
  		     )
		     (:discard (:scheduled t :deadline t :time-grid t))
                     (:name "All tasks with no schedule / deadline"
			    :category "task"
		     )
		     (:discard (:anything t))
		    )
		  )
		 )
	)
       )
       (,@(my/make-work-diary-cmd-base-settings)
        (org-agenda-start-day ,cmd-start-day)
       )
    )
  )

  (defun my/org-has-scheduled-prefix ()
    (if (org-get-scheduled-time (point)) "🗓️" "  ")
  )

  (defun my/org-has-deadline-prefix ()
    (if (org-get-deadline-time (point)) "⏰" "  ")
  )

  (let (;; TODO: Pull this info (current sprint tag, maybe also start day)
        ;;   from the work-diary.org file. I could have a heading there called Sprints
        ;;   with category "sprints" where each subheading is a single sprint.
        ;;   And each of those would have properties "sprint_tag" and "sprint_start_date"
        ;;   and similar. Maybe even not those properties, but instead just a tag :s<num>:
        ;;   and SCHEDULED set? Anyway, they would have that metadata on them, and I could
        ;;   pull it in, either for the first heading, or for the one tagged with :current:,
        ;;   something like that.
        (work-diary-sprint-current-tag "s39")
        (work-diary-sprint-start-weekday 3) ; 3 is Wednesday in org agenda.
        (work-diary-sprint-length-in-weeks 2)
       )
    (setq org-agenda-custom-commands
          (list
           (my/make-work-diary-day-cmd "d" "Work Diary: today"      nil)
           (my/make-work-diary-day-cmd "D" "Work Diary: tomorrow" "+1d")

           `("w" "Work Diary: sprint calendar"
             ((agenda ""
                      (,@(my/make-work-diary-cmd-agenda-block-base-settings nil nil)
                       (org-agenda-span ,(* 7 work-diary-sprint-length-in-weeks))
                       (org-agenda-time-grid '((require-timed remove-match)
                                               ()
                                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"
                                              )
                       )
                      )
              )
             )
             (,@(my/make-work-diary-cmd-base-settings)
              ;; NOTE: `org-agenda-start-on-weekday' works only if sprint length is 7 or 14 days.
              (org-agenda-start-on-weekday ,work-diary-sprint-start-weekday)
             )
            )

           `("A" "Work Diary: all tasks"
             ((alltodo ""
                       ((org-agenda-overriding-header "")
                        (org-agenda-prefix-format
                         " %(my/org-has-scheduled-prefix) %(my/org-has-deadline-prefix) %5e ")
                        (org-super-agenda-groups
                         '((:name ,(concat "Current sprint (" work-diary-sprint-current-tag ") tasks" )
                                  :order 0
                                  :and (:category "task" :tag ,work-diary-sprint-current-tag))
                           (:name "Epics"
                                  :order 1
                                  :and (:category "task" :todo "EPIC"))
                           (:name "To read"
                                  :order 3
                                  :and (:category "task" :tag "read"))
                           (:name "Tasks"
                                  :order 2
                                  :category "task")
                           (:discard (:anything t))
                          )
                        )
                       )
             ))
             (,@(my/make-work-diary-cmd-base-settings)
             )
            )

           '("p" "Private Diary"
             (;; The main view: a list of tasks for today.
              (agenda ""
                      ((org-agenda-span 'day)
                       (org-agenda-prefix-format " %12s %5e ")
                       (org-agenda-sorting-strategy '(todo-state-down priority-down urgency-down effort-down))
                       (org-habit-show-all-today t)
                       (org-super-agenda-groups
                        '((:name "Habits"
                                 :and (:category "habit"
                                       :not (:log t))
                          )
                          ;; Tasks to be done today.
                          (:name "Todo"
                                 :and (:category "task"
                                       :scheduled t
                                       :not (:scheduled future)
                                       :not (:log t))
                          )
                          ;; Tasks that were done today.
                          (:name none
                                 :and (:category "task"
                                       :log closed)
                          )
                         )
                       )
                      )
              )
              ;; All tasks without a schedule or a deadline.
              (alltodo ""
                       ((org-agenda-overriding-header "")
                        (org-agenda-prefix-format " %5e ")
                        (org-super-agenda-groups
                         '((:discard (:scheduled t :deadline t :time-grid t))
                           (:name "All tasks with no schedule / deadline"
                                  :category "task"
                           )
                           (:discard (:anything t))
                          )
                        )
                       )
              )
             )
             ((org-agenda-files `("~/Dropbox/private-diary.org" ,my/calendar-events-private-org-file))

              (org-agenda-start-with-log-mode '(closed clock))
              (org-agenda-skip-scheduled-if-done t)
              (org-agenda-skip-deadline-if-done t)

              (org-agenda-skip-deadline-prewarning-if-scheduled t)
              (org-agenda-skip-scheduled-repeats-after-deadline t)
             )
            )
          )
    )
  )
))

(defun my/work-diary-open-sprint-planning-windows ()
  "Open windows for sprint planning."
  (interactive)
  (org-toggle-sticky-agenda 1) ; This is needed to allow two agendas at the same time.
  (org-agenda nil "w")
  (delete-other-windows)
  (org-agenda nil "A")
)
(my/leader-keys
  "os"  '("sprint planning" . my/work-diary-open-sprint-planning-windows)
)

(use-package org-tidy)

(use-package org-present
  :after (org visual-fill-column org-tidy)
  :bind (
    :map org-present-mode-keymap
           ("q" . org-present-quit)
  )
  :config

  ;; TODO: I should make it work with evil-mode.
  ;;   Then I could not probably even need to go read-only.

  (defun my/on-presentation-start ()
    (let ((restore-local-vars
	      (my/save-local-vars-state
	        '(visual-fill-column-width
		  visual-fill-column-center-text
		  org-tidy-properties-style
		  org-tidy-general-drawer-flag
		  org-tidy-general-drawer-name-whitelist)))
          (restore-modes
              (my/save-modes-state
                '(evil-local-mode
		  visual-line-fill-column-mode
                  org-tidy-mode)))
	 )

      (when (featurep 'evil)
        (evil-local-mode -1) ; Otherwise evil messes up org-present.
      )

      (org-present-big)
      (org-display-inline-images)
      (org-present-hide-cursor)
      (org-present-read-only)

      ;; Soft wraps the text at fixed width while also centering it.
      ;; TODO: I could get decent fixed width only with value of 20 when `(org-present-big)`
      ;;   is used above, while I would normally expect 80 to do it.
      ;;   Figure out why is that so -> does usage of `(text-scale-increase)` in `(org-present-big)`
      ;;   uses somehow mess things up? This is because it is after inline-images!
      ;; TODO: There also seems to be some weird interaction between this mode and (org-display-inline-images).
      ;;   If this happens before inlining images, then ATTR_ORG :width behaves weird.
      (setq-local visual-fill-column-width 20
                  visual-fill-column-center-text t)
      (visual-line-fill-column-mode 1)

      ;; Hide org drawers (:PROPERTY: and :NOTES:).
      (setq-local org-tidy-properties-style 'invisible
		  org-tidy-general-drawer-flag t
		  org-tidy-general-drawer-name-whitelist '("NOTES"))
      (org-tidy-mode 1)

      (defun my/on-presentation-quit ()
	(org-present-small)
	(org-remove-inline-images)
	(org-present-show-cursor)
	(org-present-read-write)

	(funcall restore-local-vars)
	(funcall restore-modes)

	(remove-hook 'org-present-mode-quit-hook 'my/on-presentation-quit)
      )
      (add-hook 'org-present-mode-quit-hook 'my/on-presentation-quit)
    )
  )
  (add-hook 'org-present-mode-hook 'my/on-presentation-start)

  (defun my/org-present-eval-print-last-sexp ()
    "Evaluate and print (in buffer) the last sexp while in presentation mode."
    (interactive)
    (org-present-read-write)
    (eval-print-last-sexp)
    (org-present-read-only)
  )
)

(with-eval-after-load 'org
  (defun my/org-babel-tangle-no-confirm ()
    (let ((org-confirm-babel-evaluate nil)) (org-babel-tangle))
  )
  (defun my/when-emacs-org-file-tangle-on-save ()
    (when (and buffer-file-name (file-equal-p buffer-file-name (my/emacs-org-file-path)))
      (add-hook 'after-save-hook 'my/org-babel-tangle-no-confirm nil 'local)
    )
  )
  (add-hook 'org-mode-hook 'my/when-emacs-org-file-tangle-on-save)
)

(use-package emacs
  :ensure nil
  :config
  (defun my/alternate-buffer (&optional window)
    "Switch back and forth between current and last buffer in the current window."
    (interactive)
    (cl-destructuring-bind (buf start pos)
      (or (cl-find (window-buffer window) (window-prev-buffers) :key #'car :test-not #'eq)
          (list (other-buffer) nil nil)
      )
      (if (not buf)
        (message "Last buffer not found.")
        (set-window-buffer-start-and-point window buf start pos)
      )
    )
  )

  (defun my/open-init-file ()
    "Open the init file."
    (interactive)
    (find-file user-init-file)
  )

  (defun my/open-emacs-org-file ()
    "Open the init file."
    (interactive)
    (find-file (my/emacs-org-file-path))
  )

  (defun my/emacs-org-file-path ()
    (expand-file-name (file-name-concat user-emacs-directory "Emacs.org"))
  )

  (defun my/switch-to-messages-buffer ()
    "Switch to the messages buffer."
    (interactive)
    (switch-to-buffer "*Messages*")
  )
)

;; Ivy is the main thing (nice search through list of stuff, in minibuffer and elsewhere),
;; while Counsel and Swiper extend its usage through more of the Emacs.

;; TODO: Check out Ivy hydra -> I saw it in Ivy manual but don't know how to use it (it doesn't seem to be installed?).
;;   Allegedly (use-package ivy-hydra :after (ivy hydra)) should do the job? Try it.
;; TODO: Should I set Ivy to use fuzzy search? Is that better or not?
;; TODO: In Spacemacs (helm), coloring of listed files on C-x C-f is richer than I have in Ivy here.
;; Directories have stronger contrast, hidden files are grey, symbolic links neon, ... .
;; I should also get Ivy to behave like this! Right now it shows dirs in too similar color uses the same
;; color for all the rest.
;;   TODO: Show this cheatsheet somehow as part of Ivy buffers? Kind of like Helm does in Spacemacs?
(use-package ivy
  :bind (
        ;; I define some evil-ish keybindings here since neither evil not evil-connection
        ;; define these specific ones for Ivy.
        :map ivy-minibuffer-map ;; When in the minibuffer.
              ("C-h" . ivy-backward-kill-word)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              ("C-l" . ivy-alt-done)
              ("TAB" . ivy-alt-done)
        :map ivy-switch-buffer-map ; When in the buffer switching mode.
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
        :map ivy-reverse-i-search-map ; When doing incremental search.
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-reverse-i-search-kill)
        )
  :custom
  (ivy-height 20)
  (ivy-use-virtual-buffers t)  ; Adds recent files and bookmarks and similar to results.
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")  ; (num listed / total num)
  (ivy-extra-directories nil)  ; Don't show ./ and ../
  :config
  ;; ivy-format-functions-alist determines for each place where ivy is used how the output should be formatted.
  ;; t stands for default case, if there was no more specific formatting function defined.

  ;; Here, we specify which formatting function to use as a default case (t).
  ;; We choose ivy-format-functon-line, that extends the higlight of selection to the edge of the window,
  ;; not just till the end of the selected word. This is one of default choices and it looks better.
  ;; This is recommended by ivy-rich, as a setting.
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow-line)

  ;; ivy-re-builders-alist defines which completion styles (fuzzy, in order, ...) to use for which ivy usage.
  ;; Here I define that we use ivy--regex-ignore-order for all situations, instead of the default ivy--regex-plus,
  ;; which is the same but cares about the order of words in the query, which I found to not be useful.
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  ;; ivy-initial-inputs-alist defines what to start specific searches with. Default is ^ for all searches,
  ;; which makes queries start from the start of each completion candidate, but I set it to nil to avoid that.
  (setq ivy-initial-inputs-alist nil)

  ;; This will enhance specific emacs commands with ivy automatically.
  (ivy-mode 1)
)

;; Counsel is a package that is part of Ivy ecosystem.
;; It brings enhanced versions of common emacs commands, powered by Ivy.
;; Ivy already offers some enhanced commands, but Counsel offers more and better.
(use-package counsel
  :config
  (setq counsel-describe-function-function 'helpful-callable)
  (setq counsel-describe-variable-function 'helpful-variable)
  (counsel-mode 1)  ; This will remap the built-in Emacs functions that have counsel replacements.
)

;; Swiper is a package that is part of Ivy ecosystem.
;; Better isearch (incremental search), powered by Ivy.
(use-package swiper
  :bind (("C-s" . swiper)
         :map evil-normal-state-map
         ("/" . swiper)
         ("*" . swiper-thing-at-point)
         :map evil-motion-state-map
         ("/" . swiper)
         ("*" . swiper-thing-at-point)
        )
)

;; Show more info for some usages of Ivy. Also allows easier customization of Ivy output.
(use-package ivy-rich
  :after (ivy counsel)
  :config
  ;; This is my custom function for how Ivy shows candidates when finding a file.
  ;; Unlike default function used by ivy(-rich), here I do some additional formatting:
  ;;  - I stylize dot(files/dirs).
  ;;  - I stylize executables.
  (defun my/ivy-read-file-transformer (filename)
    "Transform candidate STR when reading files."
    (let*
      ((current-dir (or (ivy-state-directory ivy-last) default-directory))
      (filepath (expand-file-name filename current-dir))
      (is-dir (ivy--dirname-p filename))
      (is-dotfile (string-prefix-p "." filename))
      (is-exec (file-executable-p filepath))
      )
      (cond
	((and is-dir is-dotfile) (propertize filename 'face '(:inherit (font-lock-comment-face ivy-subdir))))
	(is-dotfile (propertize filename 'face 'font-lock-comment-face))
	(is-dir (propertize filename 'face 'ivy-subdir))
	(is-exec (propertize filename 'face 'font-lock-keyword-face))
	(t filename)
      )
    )
  )

  ;; With ivy-rich-set-columns, you can add new ones or replace existing columns when ivy is used in specific commands.
  ;; For details check out ivy-rich docs and docs of ivy-rich-display-transformers-list .
  (ivy-rich-set-columns
    'counsel-find-file  ; Set columns for this command (therefore when finding file).
    '((my/ivy-read-file-transformer)  ; Use my function instead of default ivy-read-file-transformer.
      (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))  ; This I kept the same. It adds target for links.
     )
  )

  (ivy-rich-mode 1)
)

;; Projectile brings the concept of "Project" to emacs, as a project on the disk.
(use-package projectile
  :init
  ;; First thing that happens on switching to a new project.
  ;; TODO: Try without this, see if I like that better or not, or if I would like something else.
  (setq projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map) ; TODO: Get this behind SPC.
  :custom
  ((projectile-completion-system 'ivy))
  :config
  (projectile-mode)
)

;; Provides better integration of Projectile and Counsel.
(use-package counsel-projectile
  :config
  (defun counsel-projectile-rg-region-or-symbol ()
    "Search for selected region if active, otherwise search for symbol at point using `counsel-projectile-rg`."
    (interactive)
    (let ((counsel-projectile-rg-initial-input (projectile-symbol-or-selection-at-point)))
        (counsel-projectile-rg)
    )
  )

  (counsel-projectile-mode)
)

(my/leader-keys
  "g" '("git (version control)" . (keymap))
)

(use-package gitstatus
  :custom
  (gitstatusd-exe "~/.local/bin/gitstatusd")
  (gitstatus-prefix nil)
)

;; NOTE: I installed transient not because I use it directly, but because magit
;;   needs a newer version of it than what comes with emacs by default, and this
;;   takes care of it. If magit at some point stops needing it, I can remove this.
(use-package transient)

;; Magit is all you need to work with git.
;; TODO: I had to explicitly install new transient above to get magit to work because
;;   it expects a newer version than what emacs ships with. Once I update emacs
;;   or magit, drop the :after transient and remove (use-package transient) unless
;;   I also use it for something else.
(use-package magit
  :after transient
  :defer t
)
;; I define this outside of (use-package magit) because later is deferred.
(my/leader-keys
  "gg" 'magit
)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (my/leader-keys
    "gn" '("next change" . diff-hl-next-hunk)
    "gp" '("previous change" . diff-hl-previous-hunk)
    "gr" '("set ref rev (global)" . diff-hl-set-reference-rev) ;; NOTE: It sets this globally, so in other projects it will cause weird behaviour!
    "gR" '("reset ref rev (global)" . diff-hl-reset-reference-rev)
  )
)

;; TODO: Fix highlight and search faces in tooltip/popup, or have theme that makes them nice. Company has faces that we can customize.
;; TODO: Either make scroll more visible, or use lines instead.
(use-package company
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-format-margin-function 'company-text-icons-margin)
  (company-text-face-extra-attributes '(:weight semi-light :slant italic))
  ;; I found default icons (be it vscode or text) to be too hard to understand,
  ;; so I made my own mapping here that provides more info. For the context, icons are
  ;; short descriptions left of the completion candidates in the popup.
  (company-text-icons-mapping
  '((array          "   []" font-lock-type-face)
    (boolean        " bool" font-lock-builtin-face)
    (class          "class" font-lock-type-face)
    (color          "color" success)
    (constant       "const" font-lock-constant-face)
    (constructor    "cnstr" font-lock-function-name-face)
    (enum-member    "enumv" font-lock-builtin-face)
    (enum           " enum" font-lock-builtin-face)
    (field          "field" font-lock-variable-name-face)
    (file           " file" font-lock-string-face)
    (folder         "  dir" font-lock-doc-face)
    (interface      " intf" font-lock-type-face)
    (keyword        "  kwd" font-lock-keyword-face)
    (method         " mthd" font-lock-function-name-face)
    (function       " func" font-lock-function-name-face)
    (module         "  mdl" font-lock-type-face)
    (numeric        "  num" font-lock-builtin-face)
    (operator       "   op" font-lock-comment-delimiter-face)
    (property       " prop" font-lock-variable-name-face)
    (reference      "  ref" font-lock-doc-face)
    (snippet        " snip" font-lock-string-face)
    (string         "  str" font-lock-string-face)
    (struct         "strct" font-lock-variable-name-face)
    (text           " text" shadow)
    (type-parameter "typep" font-lock-type-face)
    (unit           " unit" shadow)
    (value          "  val" font-lock-builtin-face)
    (variable       "  var" font-lock-variable-name-face)
    (t              "    ." shadow))
  )
  :config
  (global-company-mode 1)
)

;; Requires some stuff like cmake, support for modules in emacs, libtool-bin, but most systems /
;; emacses have all those ready, so usually you don't have to think about it.
(use-package vterm
  ;; hl-line highlight flickers in vterm, so we turn it off.
  ;; Relevant github issue: https://github.com/akermu/emacs-libvterm/issues/432 .
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :config
  (defun my/vterm-new ()
    (interactive)
    (vterm t)
  )
  (my/leader-keys
    "\"" '("new terminal" . my/vterm-new)
  )
  (evil-define-key 'normal vterm-mode-map (kbd "C-p") 'vterm-send-up)
  (evil-define-key 'normal vterm-mode-map (kbd "C-n") 'vterm-send-down)
)

;; Allows easy toggling of terminal(vterm) window.
(use-package vterm-toggle
  :after (vterm)
  :config
  (my/leader-keys
    "'" '("toggle terminal" . vterm-toggle)
  )
  ;; If I press C-return after toggling to terminal window, it will insert `cd` command that takes
  ;; me to dir of previous buffer! Very useful.
  (define-key vterm-mode-map [(control return)] #'vterm-toggle-insert-cd)
)

(with-eval-after-load 'vterm
  (defvar my/vterm-prompt-hook nil "A hook that runs each time the prompt is printed in vterm.")

  (defun my/run-vterm-prompt-hooks ()
    "Runs my/vterm-prompt-hook hooks."
    (run-hooks 'my/vterm-prompt-hook)
  )

  (with-eval-after-load 'vterm
    ;; If OSC sequence "prompt" is printed in the terminal, `my/run-vterm-prompt-hook'
    ;; will be run.
    (add-to-list 'vterm-eval-cmds '("prompt" my/run-vterm-prompt-hooks))
  )
)

(with-eval-after-load 'vterm
  (defvar-local my/vterm-git-status-string nil
    "A pretty string that shows git status of the current working directory in vterm.")

  ;; TODO: Sometimes, vterm hides top line under the header-line. But not always. It starts in right
  ;; place, and commands like "go to first line" work correctly, but I press enter and new line in
  ;; vterm appears, whole buffer shifts for one line up and the first line becomes hidden. Figure
  ;; out how to fix this.
  (defun my/vterm-set-header-line ()
    "Display the header line that shows vterm's current dir and git status.
It gets git status string from `my/vterm-git-status-string' variable each time it renders."
    (setq header-line-format
          '((:eval (when my/vterm-git-status-string (concat " " my/vterm-git-status-string " ❯ ")))
            (:propertize
             (:eval (abbreviate-file-name default-directory))
             face font-lock-comment-face
            )
           )
    )
    ;; Setting :box of header line to have an "invisible" line (same color as background) is the trick
    ;; to add some padding to the header line.
    (face-remap-add-relative
     'header-line
     `(:box (:line-width 6 :color ,(face-attribute 'header-line :background nil t)))
    )
  )
  (add-hook 'vterm-mode-hook 'my/vterm-set-header-line)

  (with-eval-after-load 'gitstatus
    (defun my/obtain-vterm-git-status-string ()
      "Obtains the git status for the current directory of the vterm buffer.
It builds a pretty string based showing it and stores it in `my/vterm-git-status-string' var.
It uses external `gitstatusd' program to calculate the actual git status."
      (gitstatusd-get-status
       default-directory
       (lambda (res)
         (let ((status-string (gitstatus-build-str res)))
           (when (not (equal my/vterm-git-status-string status-string))
             (setq my/vterm-git-status-string (gitstatus-build-str res))
             (force-mode-line-update)
           )
         )
       )
      )
    )
    (add-hook 'my/vterm-prompt-hook 'my/obtain-vterm-git-status-string)
  )
)

(use-package jinx
  :config
  (setq jinx-languages "en_us")
  (my/leader-keys
    "tc"  '("spell checking" . jinx-mode)
  )
)

(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay 0) ; Default value is 0.9.
  :config
  (my/leader-keys
    "en"  '("next" . flycheck-next-error)
    "ep"  '("previous" . flycheck-previous-error)
    "el"  '("list" . flycheck-list-errors)
  )
)

;; Shows flycheck errors/warnings in a popup, instead of a minibuffer which is default.
;; I configured it so it doesn't show them on cursor, as is default, but on request.
(use-package flycheck-posframe
  :after flycheck
  :custom
  (flycheck-posframe-border-width 10)
  (flycheck-posframe-prefix nil)
  ;; Don't show errors automatically on cursor, since below we define a manual way to invoke
  ;; showing of errors at point. I do it this way because I configured automatic showing of first
  ;; line of errors in the sideline, so I don't need to also see the whole error all the time,
  ;; instead I will rather invoke it manually when I want to see the whole of it.
  (flycheck-display-errors-function nil)
  :config
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)
  ;; TODO: Make the posframe(popup) visually nicer.

  (defun my/show-flycheck-errors-posframe-at-point ()
    (interactive)
    (let* ((errs (flycheck-overlay-errors-at (point))))
      (flycheck-posframe-show-posframe errs)
    )
  )
  (my/leader-keys
    "ee" '("(un)expand" . my/show-flycheck-errors-posframe-at-point)
  )
)

(use-package sideline
  :hook ((flycheck-mode lsp-mode) . sideline-mode)
  :init
  ;; `up` means it is shown above the line where cursor is, `down` means beneath it.
  (setq sideline-backends-right '((sideline-flycheck . up)
				  (sideline-lsp . down)))
)

(use-package sideline-flycheck 
  :hook (flycheck-mode . sideline-flycheck-setup)
  :custom
  ;; I want to show only short version of errors, otherwise it becomes a mess.
  ;; If I need to see the full error, I have other methods to do that (e.g. flycheck-posframe).
  (sideline-flycheck-max-lines 1)
  :config
  (set-face-attribute 'sideline-flycheck-error nil
		      :slant 'italic
		      :background "black")
  (set-face-attribute 'sideline-flycheck-warning nil
		      :weight 'light
		      :slant 'italic
		      :background "black")
  (set-face-attribute 'sideline-flycheck-success nil
		      :weight 'light
		      :slant 'italic
		      :background "black")
  ;; TODO: Somehow define following prefixes ✖ ⓘ ⚠ for errors / warning / success?
)

;; From LSP directly, I show only code actions in the sideline.
;; The rest of the information I show in other way (via flycheck, via minibuffer, popups, ...).
;; Note that flycheck again sends some of that information to sideline though.
(use-package sideline-lsp
  :custom
  ;; By setting this to nil, lsp diagnostics (errors, warnings) get sent to flycheck, which we prefer since
  ;; it is a more standard / specific way to do it.
  (sideline-lsp-show-diagnostics nil)
  (sideline-lsp-show-hover nil) ; This I already show in minibuffer (eldoc) or in popup (lsp-ui-doc).
  (sideline-lsp-show-symbol nil) ; This I didn't find useful.
  (sideline-lsp-show-code-actions t) ; But I do find it useful to see code actions.
  (sideline-lsp-code-actions-prefix "✎ ")
  (sideline-lsp-actions-kind-regex "quickfix.*") ; Show only quickfix code actions, otherwise it is too much noise.
  :config
  (set-face-attribute 'sideline-lsp-code-action nil
                      :inherit 'shadow
                      :weight 'light
		      :slant 'italic
		      :background "black")
)

(use-package hideshow
  :ensure nil ; emacs built-in
  :hook (prog-mode . hs-minor-mode)
)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix ",")
  (setq lsp-use-plists t) ; Recommended performance optimization. Requires setting env var (check early-init.el block below).
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :custom
  ;; lsp-modeline is about showing "stats" in the modeline: number of errors, warnings, code actions.
  ;; Useful for languages where compilation might be broken due to errors in other files (e.g. Java, Haskell).
  (lsp-modeline-diagnostics-enable t) ; Show info about diagnostics (errors, warnings, ...) in the modeline.
  (lsp-modeline-diagnostics-scope :workspace) ; Whole project and not just this file.
  (lsp-modeline-code-actions-enable t) ; Show info about code actions in the modeline.

  ;; eldoc is the most "native" way for emacs to display docs for a thing under cursor.
  ;; It displays information about the thing under cursor/mouse in the minibuffer.
  ;; Here we tell lsp-mode to use eldoc to display "hover" lsp info (which is docs for function/symbol).
  (lsp-eldoc-enable-hover t)
  ;; Don't show all the info in minibuffer on hover, instead show only most basic info (what it is, type).
  ;; Otherwise, there is too much noise and jumping of minibuffer up and down.
  ;; If I want full docs for a thing under cursor, I will rather summon it manually: I use lsp-ui-docs-toggle for that and have bound it to "?" -> check lsp ui config for details.
  (lsp-eldoc-render-all nil)

  ;; At the top of the file, show info about the position of the cursor (path, module, symbol, ...).
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))

  (lsp-semantic-tokens-enable t) ; Richer highlighting (e.g. differentiates function symbol from var symbol).
  :config
  (general-define-key :states '(normal)
                      :keymaps 'lsp-mode-map
                      "," lsp-command-map
  )
)

(use-package lsp-ui
  :after (lsp-mode evil)
  :commands lsp-ui-mode
  :custom
  ;; Show "hover" documentation for a thing under cursor/pointer in a popup.
  (lsp-ui-doc-enable t)
  ;; I don't want it to popup constantly as I move around, instead I want to summon it when I need it.
  ;; That is why below I define "?" as a key for toggling it.
  (lsp-ui-doc-show-with-cursor nil)
  ;; If I hover with a mouse, then do show the docs, that is not too intrusive.
  (lsp-ui-doc-show-with-mouse t)
  ;; Show the docs next to the cursor/point.
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-height 30)

  ;; lsp-ui-sideline shows info that you want (e.g. diagnostics, code actions, ...)  on the right
  ;; side of the window, inline with the code.  We set it to nil here though, because we control
  ;; it from another package, sideline + sideline-lsp.
  (lsp-ui-sideline-enable nil)
  :config
  (general-define-key :states '(normal visual)
                      :keymaps 'lsp-mode-map
                      "?" 'lsp-ui-doc-glance ; TODO: This is sometimes being overshadowed with ? from evil mode, fix that.
                      "F" 'lsp-ui-doc-focus-frame
  ) 
  (setq evil-lookup-func 'lsp-ui-doc-glance)
)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
  (when (equal (following-char) ?#)
    (let ((bytecode (read (current-buffer))))
      (when (byte-code-function-p bytecode)
        (funcall bytecode))))
  (apply old-fn args)))
(advice-add (if (progn (require 'json)
                      (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
            (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
            lsp-use-plists
            (not (functionp 'json-rpc-connection))  ;; native json-rpc
            (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; Brings lsp-ivy-workspace-symbol that searches for a symbol in project as you type its name.
;; TODO: Add a keybinding, under lsp-keymap-prefix, for this command, I guess under goto? So ", g s"?
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; Shows list of all errors in a nice treemacs fashion.
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package treesit
  :ensure nil ; Because it is built-in package, this tells elpaca to not try to install it.
  :preface
  (defun my/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that with Emacs 29.
             ;; I picked those up from https://github.com/mickeynp/combobulate .
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")
	       (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	       (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	     ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it installed.
      ;; However, if we update a grammar version above then this won't update it since it is already installed,
      ;; I should instead run `treesit-install-language-grammar' manually for it.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar))))
  )
  :config
  (customize-set-variable 'treesit-font-lock-level 4) ; Use maximum details when doing syntax highlihting.
  (my/setup-install-grammars)
)

(defun my/lsp-haskell-local-face-setup ()
  ;; semhl stands for "semantic highlight" -> faces with "semhl" are faces for lsp semantic tokens.
  ;; By default, lsp-face-semhl-operator just inherits lsp-face-semhl-function, which I found to be a shame.
  ;; By setting them to keyword face, operators are nicely visible.
  (face-remap-add-relative 'lsp-face-semhl-operator '(:foregrund unspecified :inherit lsp-face-semhl-keyword))
  ;; I don't want to differentiate class methods from functions by color, that is just confusing.
  (face-remap-add-relative 'lsp-face-semhl-method '(:foreground unspecified :inherit lsp-face-semhl-function))
  ;; haskell-operator-face is used for stuff like `::`, `->` and similar. By default they were the same as
  ;; variables, I didn't like that so I made them same as other operators, which is keyword face.
  (face-remap-add-relative 'haskell-operator-face '(:foreground unspecified :inherit font-lock-keyword-face))
  ;; By default haskell keywords (import, where, ...) already are set to font-lock-keyword-face,
  ;; I just added :weight semi-bold to make them stand out a bit.
  (face-remap-add-relative 'haskell-keyword-face '(:weight semi-bold :inherit font-lock-keyword-face))
)

(defun my/haskell-mode-setup ()
  (lsp-deferred)
  (ormolu-format-on-save-mode)
  (face-remap-add-relative 'font-lock-operator-face '(:foreground unspecified :inherit font-lock-keyword-face))
  (add-hook 'lsp-after-open-hook 'my/lsp-haskell-local-face-setup nil t)
)

;; NOTE: Requires ormolu to be installed on the machine.
(use-package ormolu)

(use-package haskell-mode
  :hook
  (haskell-mode . my/haskell-mode-setup)
  (haskell-literate-mode . my/haskell-mode-setup)
)

;; ;; TODO: Some current problems:
;; ;;  - Doesn't highlight as much stuff as I would like it to (https://codeberg.org/pranshu/haskell-ts-mode/issues/7).
;; ;;    - actually it does apply font-lock-operator-face but I guess it is just white -> make that one interesting, e.g. use keyword face.
;; ;;    - I used treesit-explore-mode and it is great, I can see exactly how it understand the code, and it knows so much! So it does know a ton about the code, but we are not using it! WHy is that so? Beacause haskell-ts-mode is just not applying font lock faces to all these tokens, it seems so. It really should! Can I customize that myself, or do I need to make a PR on the haskell-ts-mode package?
;; ;;  - Is too smart while highlighting signature.
;; ;;  - Can't get it to be default mode becuase haskell-mode still gets pulled in with lsp-haskell.
;; ;;    I need to either make sure it doesn't get pulled in, or remove it from loading for .hs files.
;; ;;  - What is with literate mode?
;; (use-package haskell-ts-mode
;;   :load-path "~/git/haskell-ts-mode" ; NOTE: This is for using my local fork of the package, for dev purposes. Remove this line to use public version of the package.
;;   :mode (("\\.hs\\'" . haskell-ts-mode))
;;   :hook
;;   (haskell-ts-mode . my/haskell-mode-setup)
;;   ;;(haskell-literate-mode . my/haskell-mode-setup) ; What about literate mode?
;;   :config
;;   (setq haskell-ts-highlight-signature nil)
;;   (setq haskell-ts-font-lock-level 4) ; Maximum syntax highlighting.
;; )

;; Teaches lsp-mode how to find and launch HLS (Haskell Language Server).
(use-package lsp-haskell
  :after lsp-mode 
  :custom
  ;; This takes syntax highlighting to the maximum of detail. It is a bit slow though!
  (lsp-haskell-plugin-semantic-tokens-global-on t)
)

(defun my/add-jsdoc-in-typescript-ts-mode ()
  "Add jsdoc treesitter rules to typescript as a host language."
  ;; I copied this code from js.el (js-ts-mode), with minimal modifications.
  (when (treesit-ready-p 'typescript)
    (when (treesit-ready-p 'jsdoc t)
      (setq-local treesit-range-settings
                  (treesit-range-rules
                    :embed 'jsdoc
                    :host 'typescript
                    :local t
                    `(((comment) @capture (:match ,(rx bos "/**") @capture)))))
      (setq c-ts-common--comment-regexp (rx (or "comment" "line_comment" "block_comment" "description")))

      (defvar my/treesit-font-lock-settings-jsdoc
        (treesit-font-lock-rules
        :language 'jsdoc
        :override t
        :feature 'document
        '((document) @font-lock-doc-face)

        :language 'jsdoc
        :override t
        :feature 'keyword
        '((tag_name) @font-lock-constant-face)

        :language 'jsdoc
        :override t
        :feature 'bracket
        '((["{" "}"]) @font-lock-bracket-face)

        :language 'jsdoc
        :override t
        :feature 'property
        '((type) @font-lock-type-face)

        :language 'jsdoc
        :override t
        :feature 'definition
        '((identifier) @font-lock-variable-face)
        )
      )
      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings my/treesit-font-lock-settings-jsdoc))
    )
  )
)

;; This is a built-in package that brings major mode(s) that use treesitter for highlighting.
;; It defines typescript-ts-mode and tsx-ts-mode.
(use-package typescript-ts-mode
  :ensure nil ; Built-in, so don't install it via package manager.
  :mode (("\\.[mc]?[jt]s\\'" . typescript-ts-mode)
         ("\\.[jt]sx\\'" . tsx-ts-mode)
        )
  :hook (((typescript-ts-mode tsx-ts-mode) . lsp-deferred))
  :hook (((typescript-ts-mode tsx-ts-mode) . #'my/add-jsdoc-in-typescript-ts-mode))
)

(use-package lsp-eslint
  :ensure nil ;; Don't install since it comes built-in with lsp-mode.
  :after lsp-mode
)

;; Built-in YAML major mode with treesitter highlighting.
(use-package yaml-ts-mode
  :ensure nil ; Built-in, so don't install it via package manager.
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :hook (yaml-ts-mode . lsp-deferred)
)

(use-package markdown-mode
  :config
  ;; Set headers to have different sizes.
  (dolist (face '((markdown-header-face-1 . 1.5)
                  (markdown-header-face-2 . 1.3)
                  (markdown-header-face-3 . 1.2)
                  (markdown-header-face-4 . 1.1)
                  (markdown-header-face-5 . 1.1)
                  (markdown-header-face-6 . 1.1)))
    (set-face-attribute (car face) nil :height (cdr face))
  )
)

(use-package gptel
  :config
  (setq gptel-prompt-prefix-alist '((markdown-mode . "## You:\n")
                                    (org-mode . "** You:\n")
                                    (text-mode . "## You:\n")))
  (setq gptel-response-prefix-alist '((markdown-mode . "## AI:\n")
                                      (org-mode . "** AI:\n")
                                      (text-mode . "## AI:\n")))
  (setq gptel-default-mode 'org-mode)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response) ; On response, move cursor to the next prompt.
  (my/leader-keys
    "ii"  '("[gptel] menu" . gptel-menu)
    "ic"  '("[gptel] chat" . gptel)
    "is"  '("[gptel] send to chat" . gptel-send)
    "ir"  '("[gptel] rewrite" . gptel-rewrite)
    "ix"  '("[gptel] +/- ctxt" . gptel-add)
  )
)

(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (my/leader-keys
    "i TAB" '("toggle copilot" . copilot-mode)
  )
)

(use-package copilot-chat
  :config
  (my/leader-keys
    "i I" '("copilot chat" . copilot-chat-transient))
)

(use-package whitespace
  :ensure nil ; Don't install as it is built-in with emacs.
  :config
  ; Don't highlight too-long lines, because it is too noisy and we use another package for that anyway.
  (setq whitespace-style (delq 'lines whitespace-style))

  ; Default faces are not visible enough (grey), so I set all the faces to something more visible.
  (dolist (face '(whitespace-big-indent
                  whitespace-empty
                  whitespace-hspace
                  whitespace-indentation
                  whitespace-line
                  whitespace-missing-newline-at-eof
                  whitespace-newline
                  whitespace-space
                  whitespace-space-after-tab
                  whitespace-space-before-tab
                  whitespace-tab
                  whitespace-trailing))
    (set-face-attribute face nil :foreground "dark red")
  )

  (my/leader-keys
    "t w" '("whitespaces" . whitespace-mode)
  )
)

(use-package ethan-wspace
  :init
  (setq mode-require-final-newline nil)
  :config
  (global-ethan-wspace-mode 1)
  ;; There is ethan-wspace-face if I want to configure what it looks like.
)

(use-package column-enforce-mode
  :hook (prog-mode . column-enforce-mode)
  :config
  (setq column-enforce-column fill-column)
  (set-face-attribute 'column-enforce-face nil
                      :inherit nil
                      :background "black"
                      :underline '(:style wave :color "purple")
  )
)

(use-package recentf
  :ensure nil  ;; built-in
  :custom
  (recentf-max-saved-items 50)
  :config
  (recentf-mode 1)
)

;; Primarily supposed to be used with visual-line-mode (which is emacs builtin that soft wraps the line at window end).
;; visual-fill-column, when used with visual-line-mode, modifies the wrapping to happen at the fixed (by default fill-column) width,
;; instead of at the window end.
;; It can also center the text.
;; Useful for making the buffer look "document" like.
(use-package visual-fill-column)

;; This makes copy/paste properly work when emacs is running via the terminal.
(use-package xclip
  :config
  (xclip-mode 1)
)

;; Allows fast jumping inside the buffer (to word, to line, ...).
(use-package avy)

;; Allows jumping to any window by typing just a single letter.
(use-package ace-window)

;; This package gives me commands to jump to a window with specific number (ace-window doesn't do that).
(use-package winum
  :config
  (winum-mode)
)

;; Remembers last used commands and puts them on top of M-x's list of commands.
;; Integrates seamlessly with Ivy/Counsel, Ido and some other.
(use-package amx)

;; Highlight TODO and similar keywords in comments and strings.
(use-package hl-todo
  :config
  (global-hl-todo-mode)
)

;; Utility package that provides nice icons to be used in emacs, by other packages.
;; NOTE: The first time you load your config on a new machine, you'll have to
;; run the following command interactively:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; Enhances built-in Emacs help with more information: A "better" Emacs *Help* buffer.
(use-package helpful
  :defer t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  (([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ("C-h h" . helpful-at-point)
  )
)

;; Colorizes color names in buffers.
;; Works better than clasical rainbow-mode, which would mess up Help buffer for me.
(use-package colorful-mode
  :config
  (add-to-list 'global-colorful-modes 'help-mode)
  (global-colorful-mode)
)

;; It colors each pair of parenthesses into their own color.
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
)

;; Brings functions for converting buffer text and decorations to html.
(use-package htmlize)

;; This introduces redundancy, because we exactly replicate the content of dir local vars that we want to allow automatically,
;; but on the other hand it is the recommended/official way to be sure that we are ok with execution of that code, and not have
;; emacs keep asking us if we want to execute them.
(setq safe-local-variable-values
  '((eval with-eval-after-load 'lsp-mode
	  (add-to-list 'lsp-file-watch-ignored-directories "/e2e-test/test-outputs\\'")))
)
