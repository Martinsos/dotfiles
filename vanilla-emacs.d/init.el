;;;;;;;;;;
;;; UI ;;;
;;;;;;;;;;

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)

(set-fringe-mode 10)

(setq-default fill-column 80)

(column-number-mode) ; Show row:column in mode line.

;; Start in fullscreen.
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; We define our own hook that runs after any call to enable-theme.
(defvar after-enable-theme-hook nil
  "Hook run after a theme is enabled using `enable-theme'.")
(advice-add 'enable-theme :after (lambda (&rest _) (run-hooks 'after-enable-theme-hook)))

;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)  ;; Load package.el, emacs's built-in package system.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
;; Initializes the package.el by loading and activating all installed packages.
(package-initialize)
;; Downloads the package list if it hasn't been downloaded yet.
(unless package-archive-contents (package-refresh-contents))
;; Install use-package (advanced package management for emacs) if not installed yet.  
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)  ; Tells use-package to have :ensure t by default for every package it manages.

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; doom-themes have nice, high quality themes.
(use-package doom-themes
  :config
  ;; I went with dracula for now. palenight is also nice.
  ;; TODO: I don't quite like though how it colors the ivy selection. I will want to customize ivy-current-match face.
  ;;   I want it to not modify the foreground of the ivy match, right now it loses coloring from it.
  ;; I run this as after-init-hook because that gives time during init to register any after-enable-theme-hooks.
  ;; If I decide to not use after-enable-theme-hook, I should just make this run here and now, no hook.
  ;; TODO: Figure out where and how is the best way to do theme customization. I am guessing it shoudl be happening in a central place,
  ;;   even if it is about other packages faces, and that it should happen next to loading of the theme?
  (add-hook 'after-init-hook (lambda () (load-theme 'doom-dracula t)))
)

;; general.el provides convenient, unified interface for key definitions.
;; It can do many cool things, one of them is specifying leader key and prefixes.
;; For best results, you should do all/most of the key defining via general (e.g. `general-define-key`).
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs) ; TODO: Do I need this? Understand it.
    :prefix "SPC"
    :global-prefix "C-SPC" ; TODO: Understand why this?
  )

  ;; TODO: In general.el README, this way of defining keys is listed
  ;;   as ineficient (slows down startup time).
  ;;   I should check https://github.com/noctuid/general.el?tab=readme-ov-file#will-generalel-slow-my-initialization-time
  ;;   and rewrite it to follow the advice there.
  (my/leader-keys
    "t"  '(:ignore t :which-key "toggles") ; TODO: This is prefix? There is no nicer way to define it?
    "tt" '(counsel-load-theme :which-key "choose theme")
  )
)


;;;;;;;;;;;;
;;; Evil ;;;
;;;;;;;;;;;;

;; CHEATSHEET: C-z puts us into `emacs` mode, which is normal situation without evil.
(use-package evil
  :init
  (setq evil-want-integration t)  ; Required by evil-collection.
  (setq evil-want-keybinding nil) ; Required by evil-collection.
  ;; C-u-scroll needs explicit enabling because in Emacs C-u is important, it is
  ;; universal argument. But I don't use it much, so I rather go with vi's
  ;; scroll, which I use a lot.
  (setq evil-want-C-u-scroll t)
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


;; Delight is used to hide/edit information about major or minor modes from the modeline.
(use-package delight)

;; TODO: Make it open nicely (undo-tree-visualize) on SPC a u.
;;   Also, tie `u` keybinding to use undo-tree, not normal undo (should I do that?).
(use-package undo-tree
  :delight
  :custom
  (undo-tree-visualizer-diff t)  ; Display diff in undo-tree visualizer.
  :config
  (global-undo-tree-mode)
)

;; Hydra enables you to define a small "menu", which when you activate, activates
;; transient unique keybindings (which you also defined) that you can use only
;; then, and lists them in the minibuffer in a nice menu.
;; It is convenient when you need to spam a lot of very specific commands,
;; e.g. scale text (in / out), or resize window (left / right / up / down), or
;; iterate through kill ring, or something like that. So then you go into
;; "text scale resizing mode" to put it that way, and you can easily resize it
;; with e.g. one letter commands.
(use-package hydra)

(defhydra hydra-text-scale ()
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t)
)
(my/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ivy, Counsel and Swiper ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ivy is the main thing (nice search through list of stuff, in minibuffer and elsewhere),
;; while Counsel and Swiper extend its usage through more of the Emacs.

;; TODO: Should I set Ivy to use fuzzy search? Is that better or not?
;; TODO: In Spacemacs (helm), coloring of listed files on C-x C-f is richer than I have in Ivy here.
;; Directories have stronger contrast, hidden files are grey, symbolic links neon, ... .
;; I should also get Ivy to behave like this! Right now it shows dirs in too similar color uses the same
;; color for all the rest.
;; CHEATSHEET: M-o when in an Ivy buffer shows extra commands that can be run on selected completion item.
;;   TODO: Show this cheatsheet somehow as part of Ivy buffers? Kind of like Helm does in Spacemacs?
(use-package ivy
  :delight
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

  ;; This will enhance specific emacs commands with ivy automatically.
  (ivy-mode 1)
)

;; Counsel is a package that is part of Ivy ecosystem.
;; It brings enhanced versions of common emacs commands, powered by Ivy.
;; Ivy already offers some enhanced commands, but Counsel offers more and better.
(use-package counsel
  :delight
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
	      ("?" . swiper-backward)
	)
)

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

;; Show more info for some usages of Ivy. Also allows easier customization of Ivy output.
(use-package ivy-rich
  :after (ivy counsel)
  :config
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

;; TODO: Configure or use some other modeline.
(use-package doom-modeline
  :config
  (setq doom-modeline-height 40)
  (doom-modeline-mode 1)
)

;; TODO: Comes packaged with emacs 30! So I don't need to install it any more. Does that mean I need to change something here? Or use-package just won't install it and all good?
(use-package which-key
  :diminish
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode)
)

;; Highlights the line in which cursor is.
(use-package hl-line
  :config
  (global-hl-line-mode)
)

;; Enhances built-in Emacs help with more information: A "better" Emacs *Help* buffer.
(use-package helpful
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
  (add-hook 'after-init-hook 'global-company-mode)
)

;; It colors each pair of parenthesses into their own color.
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
)

;; TODO: Enable that new smooth/pixel scroll setting in emacs?

;; TODO: Sometimes I use :config in use-package, sometimes :init, how do I know which one to use when and what goes where?

;; TODO: How should I proerly format parenthesses in elisp? Allegedly I should use something like paredit or lispy. Bah maybe I don't need anything.

;; TODO: Use native installation of emacs.

;; TODO: Set up Company, Flycheck, LSP and LSP-UI. Flycheck and LSP-UI overlap a bit, so I will likely want to configure them so they don't display same stuff -> confiugre just one of them to display LSP diagnostics. I can maybe start with flycheck, and then add LSP-UI and see who I like better doing what.

;; TODO: Take care of the temporary files being created by emacs and undo-tree.

;; TODO: Add some of the temporary files to the .gitignore.

;; TODO: Add a nice splash screen with recent projects and recent files and maybe an inspirational quote?

;; TODO: I will want some way to easily restore where I stopped working. Maybe some presets -> e.g. quick loading of waspc project with certain file opened. Or maybe just from where I stopped.

;; TODO: To figure out what packages to install, I should take a look at what Doomemacs, Spacemacs (and their layers), Emacs-bedrock, and others, use, for inspiration, and how they have it configured.
;;   Recommendation by user: projectile, helm or ivy, company (or other auto-completion package), lsp mode, which-key. Don't forget those that come with emacs: org, dired, eshell, magit, ... .
;;   I can also look at Melpa to see which are the most used packages.

;; TODO: How do I pin down package version? What if one of them introduces a breaking change and emacs breaks? I need to have a way to pin them down / freeze them.

;; TODO: Use smartparens or electric-pair-mode?

;; TODO: Set up AI support. GPTel, Elysium, Aider.el (https://www.reddit.com/r/emacs/comments/1fwwjgw/introduce_aider_ai_programming_in_terminal_and/) , chatgpt-shell, evedel, copilot.el .

;; TODO: Add keybinding for counsel-load-theme -> that is the fucntion I want to ineractively use to load themes, not load-theme, because counsel also removes old themes which is great.

;; TODO: Can I make it so that in M-x, I get, somewhere on top maybe, a list of recent commands I ran? I guess "just" sorting commands by when they were used last would do it?

;; TODO: Use emacs-lsp-booster with lsp-mode, to speed it up / avoid freezes.

;; TODO: Write down following next to lsp-mode: I investigated lsp-mode vs eglot. Eglot natively comes with emacs and is alternative to lsp-mode. Claims to have better code and be faster, but lsp-mode seems to be bigger and more featureful, so it is really not clear at all which is better. I think I will be sticking to lsp-mode for now, people seemed to report more issues with eglot, and lsp-mode I know works well. I can try eglot at some point.

;; TODO: Try replacing Ivy, Counsel, Swiper, and Company even, with Vertico, Marginalia, Orderless, Embark, Corfu, ...
;;   Vertico is alternative to Ivy, the rest are supporting packages for it same like Counsel and Swiper are for Ivy, and then Corfu is a replacement for Company.
;;   Seems like a lot of people like Vertico, ... , Corfu and the rest. Allegedly they using more of native Emacs stuff, so are simpler but also make more sense? Hm.

;; TODO: Check out bedrock emacs, simple starting config but has good stuff allegedly: https://sr.ht/~ashton314/emacs-bedrock/ .

;; TODO: Check out config by this Prot guy, people say it is good: https://protesilaos.com/emacs/dotemacs .

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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(hydra which-key undo-tree rainbow-mode rainbow-delimiters ivy-rich hl-todo helpful general evil-escape evil-collection doom-themes doom-modeline delight counsel company command-log-mode colorful-mode all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
