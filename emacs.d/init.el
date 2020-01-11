;;; Code:

;;; I don't use "customize" system, instead I define everything here in my init.el.
;;; Therefore I send anything that emacs tries to write (and it does sometimes) in custom file to /dev/null.
;;; I also don't load custom file, since there is none.
(setq custom-file "/dev/null")

;;; Add package archives from which packages will be installed.
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

(defun load-emacs-file (path-in-emacs-dir)
  "Take relative path of file (PATH-IN-EMACS-DIR) in user Emacs directory and load it."
  (load-file (expand-file-name path-in-emacs-dir user-emacs-directory))
  )

;; TODO: Use use-package intead of req-package. It also has :require, and it has :after which might be even better fit.

;; TODO: Check this init.el: https://github.com/bbatsov/emacs.d/blob/master/init.el, see if I can get some inspiration.

;; TODO: Use custom-set-variable(s) for variables instead of setq, it is more correct (although rarely matters in practice).
;;   use-package has :custom, that should be one good way to go about it.

;; TODO: Check out some possibly useful packages: helm-mini, avy, ivy.

;; TODO: Consider doing a org file for keybindings that I have hard time forgetting. Actually there is even idea
;;   of organizing the whole init.el as org file(s), I could also check that out.

;; TODO: - Use `(unless a b...)` instead of `(if (not a) (progn b...))`.
;;       - Eliminate lines with only closing parenthesses..
;;       - Use a prefix (e.g. martinsos-) for defuns and defvars, even in my own config.
;;       - Bundle sequential setqs together (see the Haskell file for example).


(load-emacs-file "init-general.el")

(load-emacs-file "init-ui.el")

(load-emacs-file "init-power.el")

(load-emacs-file "init-ide-common.el")

(load-emacs-file "init-ide-web.el")

(load-emacs-file "init-ide-haskell.el")

(load-emacs-file "init-ide-c-cpp.el")

(load-emacs-file "init-ide-other.el")


(req-package-finish) ; Load packages in right order.


;;;; List of especially useful keybindings that are not easy to remember (may be outdated) ;;;;
;; C-c p f -> Search through project for a file by name.
;; C-c p s s -> Search through project for a file by content.
;; C-x x s -> create or open perspective by name.
;; C-space -> set a mark.
;; C-u + C-space -> jump to last mark.
;;;;;;;;


(provide 'init)
;;; init.el ends here
