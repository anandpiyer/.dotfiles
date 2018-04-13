;; init-frames.el --- Emacs frames -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Defaults.
;;------------------------------------------------------------------------------
(setq-default
 ;; do not mess with frame size when altering fonts or settings.
 frame-inhibit-implied-resize t)

;;------------------------------------------------------------------------------
;; `frames-only-mode': Use frames only, no windows (use tiling managers).
;; http://compsoc.man.ac.uk/~shep/tearing-out-the-emacs-window-manager.html
;;------------------------------------------------------------------------------
(use-package frames-only-mode
  :config (frames-only-mode t))

;;------------------------------------------------------------------------------
;; `nameframe': Use named frames.
;;------------------------------------------------------------------------------
(use-package nameframe
  :config
  (after! projectile
    (use-package nameframe-projectile
      :config (nameframe-projectile-mode t))))

(provide 'init-frames)
;;; init-frames.el ends here
