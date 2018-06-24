;; init-frames.el --- Emacs frames -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default
 ;; do not mess with frame size when altering fonts or settings.
 frame-inhibit-implied-resize t)

;;; Transparent titlebar
;; https://github.com/d12frosted/homebrew-emacs-plus/blob/master/Formula/emacs-plus.rb#L98
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/55
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Properties-in-Mode.html#Properties-in-Mode
(when (memq window-system '(mac ns))
  ;;(add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
  ;;(add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil)))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light)))

;;------------------------------------------------------------------------------
;; `frames-only-mode': Use frames only, no windows (use tiling managers).
;; http://compsoc.man.ac.uk/~shep/tearing-out-the-emacs-window-manager.html
;;------------------------------------------------------------------------------
(use-package frames-only-mode
  :disabled
  :init
  (add-hook 'emacs-startup-hook (lambda () (frames-only-mode t))))
  ;;:config (frames-only-mode t))

;;------------------------------------------------------------------------------
;; `nameframe': Use named frames.
;;------------------------------------------------------------------------------
(use-package nameframe
  :init
  (after! projectile
    (use-package nameframe-projectile
      :init (nameframe-projectile-mode t))))

(provide 'init-frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-frames.el ends here
