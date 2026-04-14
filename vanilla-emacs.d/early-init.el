;; NOTE: This file was generated from Emacs.org on 2026-04-14 21:44:07 CEST, don't edit it manually.

(setq package-enable-at-startup nil) ; Elpaca requires this.

;; When running emacs in non-daemon mode, I had emacs window appearing at the very start,
;; for a short time (~0.5-1s), as a small window with white background,
;; before the config from init.el (or even early-init.el) is applied.
;; Usual advice of setting the `(background . "black")` in early-init.el didn't help, that would happen after it.
;; From what I got, the problem is that window is white because Emacs doesn't have control yet, GTK does, and
;; by default it shows white window.
;; The best solution I found was setting `(visibility . nil)` in initial-frame-alist, and then
;; calling `make-frame-visible` on selected frame from `emacs-startup-hook` (otherwise it would remain invisible forever).
;; This hides the window until init.el is loaded, so there is no flashing and sudden changes, but it also means there is no
;; screen till init finishes, which can be long if packages are being installed, or never if something goes wrong too early,
;; so I gave up on that approach also at the end.
;; Now I just accept there will be some white screen and that is it.

(setq default-frame-alist '(
  (menu-bar-lines . 0)
  (tool-bar-lines . 0)
))

;; If first visible frame, make it full screen.
;; I don't just use 'initial-frame-alist' because I also want it to work
;; when using emacs daemon (which uses initial frame for its internal frame).
;; By doing it this way, it works in all cases.
(add-hook
 'after-make-frame-functions
 (lambda (frame)
   ;; I check for 'display' in order to eliminate emacs daemon's internal initial frame,
   ;; which claims it is visible even though it is not, but it does have 'display' set to nil.
   (when (= 1 (length (filtered-frame-list (lambda (f) (frame-parameter f 'display)))))
     (toggle-frame-maximized frame)
     (toggle-frame-fullscreen frame)
   )
 )
)

(setq use-package-compute-statistics 1)

(setenv "LSP_USE_PLISTS" "true") ; Recommended performance optimization. Also neccessary for lsp-booster below to work.
