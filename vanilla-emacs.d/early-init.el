;; NOTE: This file was generated from Emacs.org on 2025-11-21 02:22:36 CET, don't edit it manually.

(setq package-enable-at-startup nil) ; Elpaca requires this.

;; I had emacs window appearing at the very start, for a short time (~0.5-1s), as a small window with white background,
;; before the config from init.el (or even early-init.el) is applied.
;; Usual advice of setting the `(background . "black")` in early-init.el didn't help, that would happen after it.
;; From what I got, the problem is that window is white because Emacs doesn't have control yet, GTK does, and
;; by default it shows white window.
;; I solved that at the end by setting `(visibility . nil)`, and then when emacs progresses enough, by explicitly
;; calling `make-frame-visible` (otherwise it would remain invisible forever).
;; This hides the window until init.el is loaded, which means now wait time is longer before first render, ~2s,
;; but there is no flashing and sudden changes.
(setq initial-frame-alist '(
  (visibility . nil)
  (undecorated . t)
  (menu-bar-lines . 0)
  (tool-bar-lines . 0)
  (fullscreen . maximized)
))
(add-hook 'emacs-startup-hook (lambda () (make-frame-visible (selected-frame))))

(setq use-package-compute-statistics 1)

(setenv "LSP_USE_PLISTS" "true") ; Recommended performance optimization. Also neccessary for lsp-booster below to work.
