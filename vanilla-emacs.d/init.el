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

(load-theme 'tango-dark)

;; TODO: Find a theme that has highlight and region faces that don't change foreground: I don't want
;; foreground just in one color, then we loose all the nice coloring! Instead, highlight and region
;; should not touch foreground but be dark enough to work with any foreground.
;; Region is usually already dark enough so I just make sure that foreground is turned off here,
;; while highlight is often too bright so I make sure to darken it.
;; Spacemacs has this done nicely btw with its spacemacs theme.
;; So, I am not sure if I should have this code here for any theme, or just use themes that get this
;; right out of the box, or should I instead customize this per theme, ... . But for now I will have this
;; general code that takes care of it.
;; TODO: I should make sure I do it also on any theme load, not just startup.
(set-face-attribute 'highlight nil
		    :foreground nil
		    :background (color-darken-name (face-attribute 'highlight :background) 80)
)
(set-face-attribute 'region nil
		    :foreground nil
		    :background (face-attribute 'region :background)
		    :extend t
)

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
(setq use-package-always-ensure t)  ;; Tells use-package to have :ensure t by default for every package it manages.

;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;
;;; Evil ;;;
;;;;;;;;;;;;

(use-package evil
  :custom
  (evil-want-integration t)  ;; Required by evil-collection.
  (evil-want-keybinding nil) ;; Required by evil-collection.
  (evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
)

(use-package evil-escape
  :custom
  (evil-escape-key-sequence "fd")
  :config
  (evil-escape-mode)
)

;; Sets evil keybindings in many more parts of emacs than evil-mode does by default.
(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-setup-minibuffer nil)  ;; If set to `t` it messes up / overrides my custom keybindings for Ivy (e.g. C-k).
  :init (evil-collection-init)
)

;;;;;;;;;;;;


;; Delight is used to hide/edit information about major or minor modes from the modeline.
(use-package delight)

(use-package undo-tree
  :delight
  :custom
  (undo-tree-visualizer-diff t)  ;; Display diff in undo-tree visualizer.
  :config
  (global-undo-tree-mode)
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
	 :map ivy-switch-buffer-map ;; When in the buffer switching mode.
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line)
	      ("C-l" . ivy-done)
	      ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map ;; When doing incremental search.
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line)
	      ("C-l" . ivy-done)
	      ("C-d" . ivy-reverse-i-search-kill)
	 )
  :custom
  (ivy-height 20)
  (ivy-use-virtual-buffers t)  ;; Adds recent files and bookmarks and similar to results.
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")  ;; (num listed / total num)
  (ivy-extra-directories nil)  ;; Don't show ./ and ../
  :config
  ;; ivy-format-functions-alist determines for each place where ivy is used how the output should be formatted.
  ;; t stands for default case, if there was no more specific formatting function defined.

  ;; Here, we specify which formatting function to use as a default case (t).
  ;; We choose ivy-format-functon-line, that extends the higlight of selection to the edge of the window,
  ;; not just till the end of the selected word. This is one of default choices and it looks better.
  ;; This is recommended by ivy-rich, as a setting.
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow-line)

  ;; By default ivy changes the foreground to single color which looses the visual information,
  ;; so here I make sure it doesn't change the foreground and tell it to use `highlight` face for background.
  (set-face-attribute 'ivy-current-match nil
		      :foreground nil
		      :background (face-attribute 'highlight :background)
		      :extend t
  )

  ;; This will enhance specific emacs commands with ivy automatically.
  (ivy-mode 1)
)

;; Counsel brings enhanced versions of common emacs commands, powered by Ivy.
;; Ivy already offers some enhanced commands, but Counsel offers more and better.
(use-package counsel
  :delight
  :config
  (counsel-mode 1)  ;; This will remap the built-in Emacs functions that have counsel replacements.
)

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
    'counsel-find-file  ;; Set columns for this command (therefore when finding file).
    '((my/ivy-read-file-transformer)  ;; Use my function instead of default ivy-read-file-transformer.
      (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))  ;; This I kept the same. It adds target for links.
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

;; TODO: Configure or use some other modeline.
(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
)

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode)
)

(use-package hl-line
  :config
  (global-hl-line-mode)
)

;; TODO: Sometimes I use :config in use-package, sometimes :init, how do I know which one to use when?

;; TODO: How should I proerly format parenthesses in elisp?

;; TODO: Take care of the temporary files being created by emacs and undo-tree.

;; TODO: Add some of the temporary files to the .gitignore.

;; TODO: Add a nice splash screen with recent projects and recent files and maybe an inspirational quote?

;; TODO: Try out helm instead of ivy.
