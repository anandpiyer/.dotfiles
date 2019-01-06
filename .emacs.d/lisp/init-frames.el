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
  :commands (nameframe-frame-alist
             nameframe-get-frame
             nameframe-make-frame)
  :config (require 'nameframe))

;;------------------------------------------------------------------------------
;; `yequake': Drop-down emacs frames
;;------------------------------------------------------------------------------
;; (use-package yequake
;;   :commands (yequake-toggle
;;              yequake-org-capture)
;;   :config
;;   (setq yequake-frames
;;    (list (cons "Org"
;;                (list (cons 'name "Org")
;;                      (cons 'buffer-fns
;;                            '((lambda nil
;;                                (or (get-buffer "*Org Agenda*")
;;                                    (save-excursion
;;                                      (org-agenda-list)
;;                                  (current-buffer))))
;;                              split-window-horizontally
;;                              "~/Dropbox/org-mode/organizer.org"))
;;                              ;;(lambda nil
;;                              ;;  (select-window
;;                              ;;   (get-buffer-window
;;                              ;;    (find-buffer-visiting "~/Dropbox/org-mode/organizer.org"))))))
;;                      (cons 'width 0.9)
;;                      (cons 'height 0.8)
;;                      (cons 'alpha 0.95)
;;                      (cons 'frame-parameters
;;                            (list (cons 'undecorated t)
;;                                  (cons 'skip-taskbar t)
;;                                  (cons 'sticky t))))))))

(use-package yequake
  :commands (yequake-toggle
             yequake-org-capture
             yequake-retoggle)
  :config
  (setq yequake-frames
        '(("Org" .
           ((name . "Org")
            (width . 0.9)
            (height . 0.8)
            (alpha . 0.95)
            (buffer-fns . ((lambda nil
                             (or (get-buffer "*Org Agenda*")
                                 (save-excursion
                                   (org-agenda-list)
                                   (current-buffer))))
                           split-window-horizontally
                           "~/iCloud/beorg/org-mode/organizer.org"))
            (frame-parameters . ((undecorated . t)
                                 (skip-taskbar . t)
                                 (sticky . t)))))
          ("mu4e" .
           ((name . "mu4e")
             (width . 0.9)
             (height . 0.8)
             (alpha . 0.95)
             (buffer-fns . ((lambda nil
                              (or (get-buffer "*mu4e-headers*")
                                  (get-buffer "*mu4e-main*")
                                  (save-excursion
                                    (mu4e)
                                    (current-buffer))))))
             (frame-parameters . ((undecorated . t)
                                  (skip-taskbar . t)
                                  (sticky . t)))))
          ("Capture" .
           ((name . "Capture")
             (width . 0.75)
             (height . 0.5)
             (alpha . 0.95)
             (buffer-fns . (yequake-org-capture))
             (frame-parameters . ((undecorated . t)
                                  (skip-taskbar . t)
                                  (sticky . t)))))
          )))

(provide 'init-frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-frames.el ends here
