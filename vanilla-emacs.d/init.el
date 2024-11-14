;;; -*- lexical-binding: t; -*-

;; NOTE: This file was generated from Emacs.org on 2024-11-14 21:59:42 CET, don't edit it manually.

;; Install and set up Elpaca. 
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
      (build (expand-file-name "elpaca/" elpaca-builds-directory))
      (order (cdr elpaca-order))
      (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                ,@(when-let ((depth (plist-get order :depth)))
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

(elpaca elpaca-use-package (elpaca-use-package-mode)) ; Install/setup use-package.
(setq use-package-always-ensure t) ; Tells use-package to have :ensure t by default for every package it manages.

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

(use-package emacs
  :ensure nil
  :config
  (setq inhibit-startup-message t)
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
)

;; doom-themes have nice, high quality themes.
(use-package doom-themes
  :ensure (:wait t) ; Too ensure theme gets loaded as early as possible, so there is no white scren.
  :config
  ;; I went with dracula for now. palenight is also nice.
  ;; TODO: Figure out where and how is the best way to do theme customization. I am guessing it shoudl be happening in a central place,
  ;;   even if it is about other packages faces, and that it should happen next to loading of the theme?
  (load-theme 'doom-dracula t)
)

;; TODO: Configure better or use some other modeline.
(use-package doom-modeline
  :config
  (setq doom-modeline-height 40)
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

  ;; general-create-definer allows you to create a function with some defaults set, that you can use to define more keys.
  ;; We use it here to create a definer that sets SPC as a prefix (leader key) for any key that is defined with it.
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs) ; NOTE: Doesn't work without this, but I am not sure why I need it, why can't it just be global (default).
    :prefix "SPC" ; This will be active only in "normal"-like states (so `normal` and `emacs`).
    :global-prefix "C-SPC" ; This will be always active.
  )

  ;; TODO: In general.el README, this way of defining keys is listed as ineficient (slows down startup time).
  ;;   I should check https://github.com/noctuid/general.el?tab=readme-ov-file#will-generalel-slow-my-initialization-time
  ;;   and possibly rewrite it to follow the advice there.
  ;;   Also, should I use :general keyword in use-package? Figure this out, the best way to define keybindings with SPC prefix,
  ;;   should they all be here, or in their respective packages, or what.
  (my/leader-keys
    "SPC" '(counsel-M-x :which-key "M-x (exec cmd)")
    "TAB" '(my/alternate-buffer :which-key "previous buffer")

    "0"   '(winum-select-window-0 :which-key "jump to window 0")
    "1"   '(winum-select-window-1 :which-key "jump to window 1")
    "2"   '(winum-select-window-2 :which-key "jump to window 2")
    "3"   '(winum-select-window-3 :which-key "jump to window 3")
    "4"   '(winum-select-window-4 :which-key "jump to window 4")

    ;; TODO: When inside counsel-projectile-rg, you can do C-c C-o to persist the search results in a special buffer,
    ;;   and then in that buffer you can press enter on any of them and jump to that location.
    ;;   This is awesome, but how will I remember this? Somehow help myself remember this. Another candidate for "hint"/"help" zone?
    ;;   Btw Helm (in Spacemacs) has this bar at the bottom where it shows which command was just run and some hints (C-z for actions, ...).
    ;;   Is this something I can replicate, at least for Ivy?
    "/"   '(counsel-projectile-rg :which-key "search in project")
    "*"   '(counsel-projectile-rg-region-or-symbol :which-key "search in project w/input")

    ;; TODO: Add "'" to open shell.

    "t"   '(:ignore t :which-key "toggles") ; This is how prefix is defined.
    "tt"  '(counsel-load-theme :which-key "choose theme")
    "ts"  '(hydra-text-scale/body :which-key "scale text")

    "a"   '(:ignore t :which-key "apps")
    "au"  '(vundo :which-key "undo tree")

    "af"   '(:ignore t :which-key "fun")
    "afa" '(animate-birthday-present :which-key "birthday")
    "afb" '(blackbox :which-key "blackbox")
    "afc" '(butterfly :which-key "butterfly")
    "afd" '(doctor :which-key "doctor")
    "afe" '(bubbles :which-key "bubbles")
    "aff" '(dunnet :which-key "dunnet")
    "afg" '(gnugo :which-key "gnugo")
    "afh" '(hanoi :which-key "hanoi")
    "afi" '(gomoku :which-key "gomoku")
    "afj" '(solitaire :which-key "solitaire")
    "afl" '(life :which-key "life")
    "afp" '(pong :which-key "pong")
    "afs" '(snake :which-key "snake")
    "aft" '(tetris :which-key "tetris")
    "afx" '(5x5 :which-key "5x5")
    "afz" '(zone :which-key "zone")

    "q"   '(:ignore t :which-key "quit")
    "qq"  '(save-buffers-kill-terminal :which-key "quit")
    "qr"  '(restart-emacs :which-key "restart")

    "w"   '(:ignore t :which-key "windows")
    "ww"  '(ace-window :which-key "other window")
    "wd"  '(delete-window :which-key "delete window")
    "w/"  '(split-window-right :which-key "split vertically")
    "w-"  '(split-window-below :which-key "split horizontally")
    "wr"  '(hydra-window-resize/body :which-key "resize window")
    "wm"  '(hydra-window-move/body :which-key "move window")

    "b"   '(:ignore t :which-key "buffers")
    "bb"  '(ivy-switch-buffer :which-key "switch buffer")
    "bd"  '(kill-this-buffer :which-key "kill buffer")
    "bs"  '(scratch-buffer :which-key "go to scratch")
    "bm"  '(my/switch-to-messages-buffer :which-key "go to messages")
    "bp"  '(hydra-buffer-next-prev/previous-buffer :which-key "previous buffer")
    "bn"  '(hydra-buffer-next-prev/next-buffer :which-key "next buffer")
    "br"  '(revert-buffer :which-key "reload buffer")

    "f"   '(:ignore t :which-key "files")
    "fj"  '(avy-goto-char-timer :which-key "jump in file")
    "ff"  '(counsel-find-file :which-key "find file")
    "fs"  '(save-buffer :which-key "save")
    "fr"  '(counsel-recentf :which-key "recent files")

    "fe"  '(:ignore t :which-key "emacs")
    "feo" '(my/open-emacs-org-file :which-key "open Emacs.org file")
    "fei" '(my/open-init-file :which-key "open init.el file")

    "v"   '(:ignore t :which-key "eval (elisp)")
    "v:"  '(eval-expression :which-key "expression")
    "vl"  '(eval-last-sexp :which-key "last-sexp")
    "vv"  '(eval-defun :which-key "top-level form")
    "vr"  '(eval-region :which-key "region")

    "o"   '(:ignore t :which-key "org")
    "oa"  '(org-agenda :which-key "agenda")
    "oc"  '(org-capture :which-key "capture")
    "ol"  '(org-store-link :which-key "store link")

    "p"   '(:ignore t :which-key "projects")
    "pf"  '(counsel-projectile-find-file :which-key "find file")
    "pd"  '(projectile-find-dir :which-key "find dir")
    "pb"  '(projectile-switch-to-buffer :which-key "switch buffer")
    "pp"  '(counsel-projectile-switch-project :which-key "switch project")
    "pr"  '(projectile-replace :which-key "find and replace")
    "p."  '(projectile-command-map :which-key "all commands")

    "g"   '(magit :which-key "magit")
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
  ;; scroll, which I use a lot.
  (setq evil-want-C-u-scroll t)
  :custom
  (evil-shift-width 2) ; When shifting text left or right with < or >, do it for 2 spaces.
  :config
  (evil-mode 1)
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
)

;; Replace stars (*) with nice bullets.
(use-package org-bullets
  :after (org)
  :defer t
  :hook (org-mode . org-bullets-mode)
)

;; Org Tempo expands snippets to structures defined in org-structure-template-alist and org-tempo-keywords-alist.
(use-package org-tempo
  :after (org)
  :ensure nil ; Comes with org already.
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
)

(use-package org-super-agenda
  :after org
  :config
  (org-super-agenda-mode)
)

(with-eval-after-load 'org
  (setq org-agenda-scheduled-leaders '("-> " "-%dd -> "))
  (setq org-agenda-deadline-leaders '("! " "+%dd ! " "-%dd ! "))
  (setq org-agenda-custom-commands
	'(("w" "Work Diary"
	   ((agenda ""
		    ((org-agenda-span 'day)
                     (org-agenda-prefix-format " %i %6c %8s")
		     (org-agenda-sorting-strategy '(habit-down category-keep todo-state-down time-up urgency-down))
                     (org-super-agenda-groups
		      '((:name "Daily Checklist"
                               :category "dc"
			)
                        (:name "To Do"
                               :and (:scheduled t :category "task")
			)
                        (:discard (:time-grid t))  ; Drop anything that is left to show on time-grid because we show time-grid below, separately.
		       )
                     )
		    )
            )
            (agenda ""
		    ((org-agenda-span 'day)
                     (org-agenda-overriding-header "")
                     (org-agenda-prefix-format " %i %6c %8s %?-12t")
                     (org-super-agenda-groups
		      '((:name "Time schedule"
                               :time-grid t
	                )
                        (:discard (:anything t))
		       )
                     )
		    )
            )
            (alltodo ""
                     ((org-agenda-overriding-header "")
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
	   ((org-agenda-files '("~/work-diary.org"))
	   )
	  )
	 )
  )
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
    (when (and buffer-file-name (string-equal buffer-file-name (my/emacs-org-file-path)))
      (add-hook 'after-save-hook 'my/org-babel-tangle-no-confirm nil t) ; t here makes this hook buffer local.
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
    (file-name-concat user-emacs-directory "Emacs.org")
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
  :bind
  (("C-s" . swiper)
    :map evil-normal-state-map
      ("/" . swiper)
      ("?" . swiper-backward)
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

;; TODO: Fix highlight and search faces in tooltip/popup, or have theme that makes them nice. Company has faces that we can customize.
;; TODO: Either make scroll more visible, or use lines instead.
(use-package company
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
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
