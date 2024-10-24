
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There are three important "packages" to understand here: package.el, use-package, and elpaca.
;; - package.el is a default package manager (like e.g. cabal for Haskell) that comes with emacs.
;; - use-package is a third-party package that allows for nicer, declarative and performant way to define your package configurations.
;;   It uses package.el in the background to install packages, but it is not a package manager, instead it is a layer
;;   above it that is focused on loading and configuring packages (and not installing, that it delegates to package manager).
;; - elpaca is a third-party package that is a package manager, a direct alternative to package.el, but more powerful / performant.
;;
;;   In this config I use elpaca + use-package (package.el is not even loaded).
;;
;; Installing vs loading vs configuring packages:
;; - Installing package means downloading (and building) the package to your machine, so it can be loaded when you want.
;;   This is what package manager does for you.
;; - Loading package means importing/requiring it in our emacs config (init.el), so it can be used. It needs to be installed
;;   first in order to be loaded (unless it comes with emacs already).
;;   This is what use-package takes care of.
;; - Configuring means exactly that: adapting the package to your needs by defining variables, key bindings, hooks, ... .
;;   use-package also helps with this, giving us structure and reducing boilerplate.
;;
;; Lifecycle of a package:
;; 1. Installation typically happens when you start emacs for first time on new machine, or when you update packages or add new package.
;; 2. Loading typically happens each time you start emacs, and it can happen immediately on init, or it can happen later, on demand,
;;    when package is needed (in lazy fashion), depending on how you set it up for that package.
;; 3. Configuration typically happens right after loading.
;;
;; # Use-package
;;
;; The idea is that you organize all your code in `(use-package <package-name> ...)` declarations.
;; There is even `(use-package emacs ...)` where `emacs` is a special "package" that won't trigger any installation or loading,
;; allowing you to put general emacs config under it (e.g. `(column-number-mode)`).
;;
;; use-package has a bunch of keywords that you can use to configure your package or define how it is loaded / installed.
;; Mostly you will use keywords that come with use-package, but other packages can also extend use-package with their own keywords.
;; I will quickly explain some of the more important/confusing ones:
;; - :ensure -> This is really an interface toward package manager (who does package installation). Any value passed to
;;     :ensure is passed to the package manager as instructions for installation, e.g. `:ensure (:host "github" :repo "user/repo")`.
;;     Therefore, its behaviour depends on the package manager used. But generally, `:ensure nil` will not allow package installation,
;;     while `:ensure t` or any other value will try to do install package if not installed yet. Should be really named :ensure-installed hah.
;;     Normally you will want `:ensure t` for most of the packages, in which case you can (I do) configure use-package to have it be `t` by default.
;;     When using elpaca, you can pass elpaca recipe to :ensure.
;; - :defer -> `:defer t` tells use-package to postpone loading of the package till it is needed (lazy). See "deferred loading" below.
;;     Alternatively, you can pass it a number of seconds emacs needs to be idle before package is loaded.
;; - :demand -> Opposite of defer, `:demand t` tells use-package to load package now. Useful when using autoload keywords (see "deferred loading" below).
;; - :init -> happens right before package is loaded.
;; - :config -> happens right after package is loaded.
;; - ...
;; 
;; ## Deferred loading 
;; It is generally recommended to postpone/defer loading of packages till they are needed (if applicable), for faster emacs startup.
;; use-package will defer loading of packages if `:defer <non-nil>` is set, or if any of the keywords with the "autoloading side effect" are used
;; (e.g. `bind`, `command`, `hook`, ... : these all will make use-package defer loading and then autoload package once
;; corresponding binding / command / hook / ... has happeend). If you use any of the "autoload" keywords but don't want
;; use-package to be "smart" and assume you want autoloading, you can use `:demand t`.
;; It can be good practice to add `:defer t` even when autoloading keywords are used, to be explicit about the intention.
;;
;; # Elpaca
;; Elpaca can install package directly: `(elpaca some-package (message "this happens after package is installed"))`, but we use it via use-package.
;; In the background, a call to `(use-package some-package :ensure t :some-keyword <smth>)` becomes `(elpaca some-package (use-package some-package :some-keyword <smth>))`.
;;
;; ## Recipes
;; Elpaca allows you to define how the package should be installed via a `recipe`, which is provided as an argument to `:ensure` if using `use-package`.
;; Example of a recipe: `(some-package :host github :repo "user/example")`.
;;
;; ## Deferred installing
;; Elpaca, unlike package.el, doesn't install packages immediately, instead it waits for emacs init to finish, and then installs them asynchronously, in parallel for fast, non-blocking installations.
;; To prevent this behaviour and make elpaca install package right now, when the corresponding `use-package` declaration is evaluated, one can pass `:wait t` keyword do the elpaca recipe: `:ensure (:wait t ...)`. This can be useful for packages that e.g. add keywords to use-package, since you need those installed and loaded first.
;;
;; Due to elpaca's deffered installing, instead of installing and loading happening during init phase (evaluation of init.el), we have following order of events: init -> installation of packages -> loading packages.
;; Exception are packages with `:wait t` in their recipe, those will get installed / loaded at init.
;; That also means that `after-init-hook` is often not the right choice any more, and we should instead use `elpaca-after-init-hook`, which will guarantee packages are installed / loaded.

;; Install and set up Elpaca. Also, in early-init.el, we disable package.el.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;; Start in fullscreen.
  (add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
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

;; This makes copy/paste properly work when emacs is running via the terminal.
(use-package xclip
  :config
  (xclip-mode 1)
)

;;;;;; UNDO ;;;;;;;;

(use-package emacs
  :ensure nil
  :config
  ;; Default emacs undo limits are quite low so we increase them here.
  (setq undo-limit         50000000) ; ~50mb.
  (setq undo-strong-limit 100000000) ; ~100mb.
  (setq undo-outer-limit  300000000) ; ~300mb.
)

;; Simple package that brings undo/redo commands that behave in a simple, linear
;; fashion, like you would expect. However, it still keeps emacs' undo/redo
;; complex system intact with all the state it keeps, these commands just serve
;; as a simpler interface toward it, so you can still interact with it if you
;; wish (e.g. with vundo which visualizes the undo state as tree).
;; I don't set any keybindings here because it is enough to set undo-fu as
;; evil's undo system (check my evil config) and then evil uses it.
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
    ; Error in post-command-hook (vundo-live-diff-post-command): (error "No possible route")
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

;; TODO: Install undo-fu-session? Do I need persistent undo between the emacs sessions? I think not?

;; TODO: Install undo-hl (highlights changes by undo in buffer)?
;;   Seems to not be on any package manager though, so I need straight.el probably.

;;;;;;;;;;;;;;;;;;;;;


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
    "fei" '(my/open-init-file :which-key "open init file")

    "v"   '(:ignore t :which-key "eval (elisp)")
    "v:"  '(eval-expression :which-key "expression")
    "vl"  '(eval-last-sexp :which-key "last-sexp")
    "vv"  '(eval-defun :which-key "top-level form")
    "vr"  '(eval-region :which-key "region")

    "p"   '(:ignore t :which-key "projects")
    "pf"  '(counsel-projectile-find-file :which-key "find file")
    "pd"  '(projectile-find-dir :which-key "find dir")
    "pb"  '(projectile-switch-to-buffer :which-key "switch buffer")
    "pp"  '(counsel-projectile-switch-project :which-key "switch project")
    "pr"  '(projectile-replace :which-key "find and replace")
    "p."  '(projectile-command-map :which-key "all commands")

    "g"   '(magit :which-key "magit")
  )
)

;; Hydra enables you to define a small "menu", which when you activate, activates
;; transient unique keybindings (which you also defined) that you can use only
;; then, and lists them in the minibuffer in a nice menu.
;; It is convenient when you need to spam a lot of very specific commands,
;; e.g. scale text (in / out), or resize window (left / right / up / down), or
;; iterate through kill ring, or something like that. So then you go into
;; "text scale resizing mode" to put it that way, and you can easily resize it
;; with e.g. one letter commands.
(use-package hydra
  :config
  (defhydra hydra-text-scale ()
    "Scale text"
    ("j" text-scale-decrease "out")
    ("k" text-scale-increase "in")
    ("q" nil "quit" :exit t)
  )

  (defhydra hydra-buffer-next-prev ()
    "Next/previous buffer"
    ("p" previous-buffer "previous")
    ("n" next-buffer "next")
    ("q" nil "quit" :exit t)
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

;;;;;;;;;;;;
;;; Evil ;;;
;;;;;;;;;;;;

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

;;;;;;;;;;;;

;;;;;;;;;;;;;; ORG MODE ;;;;;;;;;;;;;;;

;; CHEATSHEET
;; - Shift-Tab -> cycles through expanding headers.
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
  (dolist (face '((org-level-1 . 1.2)
	          (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.0)
		  (org-level-6 . 1.0)
		  (org-level-7 . 1.0)
		  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :height (cdr face))
  )
)

;; Replace stars (*) with nice bullets.
(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  (defun my/switch-to-messages-buffer ()
    "Switch to the messages buffer."
    (interactive)
    (switch-to-buffer "*Messages*")
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ivy, Counsel and Swiper ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ivy is the main thing (nice search through list of stuff, in minibuffer and elsewhere),
;; while Counsel and Swiper extend its usage through more of the Emacs.

;; TODO: Check out Ivy hydra -> I saw it in Ivy manual but don't know how to use it (it doesn't seem to be installed?).
;;   Allegedly (use-package ivy-hydra :after (ivy hydra)) should do the job? Try it.
;; TODO: Should I set Ivy to use fuzzy search? Is that better or not?
;; TODO: In Spacemacs (helm), coloring of listed files on C-x C-f is richer than I have in Ivy here.
;; Directories have stronger contrast, hidden files are grey, symbolic links neon, ... .
;; I should also get Ivy to behave like this! Right now it shows dirs in too similar color uses the same
;; color for all the rest.
;; CHEATSHEET:
;; - M-o when in an Ivy buffer shows extra commands that can be run on selected completion item.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Projectile brings the concept of "Project" to emacs, as a project on the disk.
;; CHEATSHEET:
;; - Projectile recognizes projects with its heuristics (.git/, maven files, ...), but you can
;;   add .projectile file to the project root to explicitly mark it as a project.
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

;; Magit is all you need to work with git.
;; TODO: Version pulled in is too new for my version of emacs, so elpaca throws errors.
;;   Either use older version of magit, or upgrade emacs version.
(use-package magit
  :defer t
)

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

;; TODO: Configure more or use some other modeline.
(use-package doom-modeline
  :config
  (setq doom-modeline-height 40)
  (doom-modeline-mode 1)
)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-add-column-padding 2)
  (setq which-key-min-display-lines 5)
  (which-key-mode)
)

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

;; CHEATSHEET:
;;  - C-s to search among the candidates. C-M-s to not just highlight but also filter. C-g to quit search mode.
;;   TODO: Can I somehow show this "cheatsheet" info when candidate popup pops up? Maybe in the header/footer of the popup?
;;     Seems there is no directly supported way to do that, and would be too
;;     complex.  Ok, maybe then I can print it in the echo buffer, or somewhere
;;     else, when popup shows up (this I can do via company-provided hooks). I
;;     could maybe have dedicated space somewhere to show help / cheatsheet
;;     info?
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

;; It colors each pair of parenthesses into their own color.
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
)

;; TODO: Set up shell, below is my spacemacs setup:
;; ;; Make it so that in shell (vterm) C-p acts as "up" and C-n acts as "down", same like in external terminals.
;; ;; NOTE: For insert mode it already works like this, but I also wanted it to work the same way for the normal mode.
;; (with-eval-after-load 'vterm
;;   (evil-define-key 'normal vterm-mode-map (kbd "C-p") 'vterm-send-up)
;;   (evil-define-key 'normal vterm-mode-map (kbd "C-n") 'vterm-send-down)
;; )
;; (shell :variables
;;     shell-default-shell 'vterm ;; Fastest and best terminal emulator currently avaiable for emacs.
;;     shell-default-width 50
;;     shell-default-position 'right
;;     spacemacs-vterm-history-file-location "~/.bash_history"
;;     )

;; TODO: Enable that new smooth/pixel scroll setting in emacs?

;; TODO: Go through my spacemacs config and copy stuff I liked from there.

;; TODO: Use native installation of emacs.

;; TODO: Set up Company, Flycheck, LSP and LSP-UI. Flycheck and LSP-UI overlap a bit, so I will likely want to configure them so they don't display same stuff -> confiugre just one of them to display LSP diagnostics. I can maybe start with flycheck, and then add LSP-UI and see who I like better doing what.

;; TODO: Take care of the temporary files being created by emacs and undo.

;; TODO: Add some of the temporary files to the .gitignore.

;; TODO: Add a nice splash screen with recent projects and recent files and maybe an inspirational quote? Check out emacs-dashboard.

;; TODO: I will want some way to easily restore where I stopped working. Maybe some presets -> e.g. quick loading of waspc project with certain file opened. Or maybe just from where I stopped.

;; TODO: Figure out how to pin down package versions -> lockfile / freezing. But also search a bit how others do package version pinning down. There is :pin for use-package, but I am nto sure if that is what I need.

;; TODO: Use smartparens or electric-pair-mode?

;; TODO: Set up AI support. GPTel, Elysium, Aider.el (https://www.reddit.com/r/emacs/comments/1fwwjgw/introduce_aider_ai_programming_in_terminal_and/) , chatgpt-shell, evedel, copilot.el .

;; TODO: Try Nano theme.

;; TODO: Implement transient yanking, so I can go through the kill ring, like in spacemacs. counsel-yank-pop might be useful? Or should I implement my own hydra?

;; TODO: Stop that custom block from appearing at the end of this file.

;; TODO: Use emacs-lsp-booster with lsp-mode, to speed it up / avoid freezes.
;; TODO: Write down following next to lsp-mode: I investigated lsp-mode vs eglot. Eglot natively comes with emacs and is alternative to lsp-mode. Claims to have better code and be faster, but lsp-mode seems to be bigger and more featureful, so it is really not clear at all which is better. I think I will be sticking to lsp-mode for now, people seemed to report more issues with eglot, and lsp-mode I know works well. I can try eglot at some point.

;; TODO: I would love to show some kind of hints in certain situations. Couple of useful keybindings when I am in some context. Kind of like Helm in Spacemacs shows it at that footer it has.
;;   But for Ivy. Also, for stuff like company when in completion popup, to remind me of C-s and C-M-s. What if I had an area in my emacs where contextual tips are shown as you do some actions?
;;   For example they get shown when you open completion popup, or when you start using Ivy? Could I make this a universal mechanism? Maybe I can expand it then for a bit more info if I want,
;;   for my custom cheasheet? It would be kind of a "publish" mechanism I guess.

;; TODO: Try replacing Ivy, Counsel, Swiper, and Company even, with Vertico, Marginalia, Orderless, Embark, Corfu, ...
;;   Vertico is alternative to Ivy, the rest are supporting packages for it same like Counsel and Swiper are for Ivy, and then Corfu is a replacement for Company.
;;   Seems like a lot of people like Vertico, ... , Corfu and the rest. Allegedly they using more of native Emacs stuff, so are simpler but also make more sense? Hm.
;;   Embark seems great, it is a quite unique package: the idea is that you have a keybinding for it and then whenever you are doing something, you hit that keybinding and Embark
;;   offers you actions that you can do in that situation / with that thing. It does however work best if Marginalia is used, because it gives it more context on stuff, and Marginalia
;;   works best with the rest of the stuff above, so it kind of pulls one another hm, so it probably makes sense to try to go for all of it at once.

;; TODO: To figure out what packages to install, I should take a look at what Doomemacs, Spacemacs (and their layers), Emacs-bedrock, and others, use, for inspiration, and how they have it configured.
;;   Recommendation by user: projectile, helm or ivy, company (or other auto-completion package), lsp mode, which-key. Don't forget those that come with emacs: org, dired, eshell, magit, ... .
;;   I can also look at Melpa to see which are the most used packages.
;; TODO: Check out bedrock emacs, simple starting config but has good stuff allegedly: https://sr.ht/~ashton314/emacs-bedrock/ .
;; TODO: Check out config by this Prot guy, people say it is good: https://protesilaos.com/emacs/dotemacs .
;; TODO: Another emacs config to check out, kickstarter for neovimers, might have some good stuff for evil: https://github.com/LionyxML/emacs-kick . I also saw it uses vertico, marginalia, ... .

;; TODO: Learn more about Avy: https://karthinks.com/software/avy-can-do-anything/ .

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; CHEATSHEET ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-: -> eval in echo buffer
;;
;; check-parens -> find unbalanced parenthesses in the buffer
;;
;; C-h -> help! find out about v (variable), f (function), face, ... .
;; C-h h -> help for symbola at point.
;;
;; m <char> -> set mark
;; ` <char> -> go to mark
;; ` ` -> go to last mark
;;
;;
;; Troubleshooting:
;; - package couldn't be found (upon install) -> local packages archive is old, run `list-packages` to update it.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
