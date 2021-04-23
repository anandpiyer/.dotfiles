;;; setup-git.el --- git related packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :after evil
  :commands (magit-status magit-blame)
  :config
  (setq magit-auto-revert-mode nil)
  ;; Switch to emacs state only while in `magit-blame-mode', then back when
  ;; its done (since it's a minor-mode).
  (add-hook 'magit-blame-mode-hook
            (evil-local-mode (if magit-blame-mode -1 +1))))

;; (use-package evil-magit
;;   :after magit
;;   :commands (evil-magit-init)
;;   :init
;;   ;; optional: this is the evil state that evil-magit will use
;;   ;; (setq evil-magit-state 'normal)
;;   ;; optional: disable additional bindings for yanking text
;;   ;; (setq evil-magit-use-y-for-yank nil)
;;   (require 'evil-magit))

;; Show git status in the fringe.
(use-package git-gutter-fringe
  :disabled
  :ensure git-gutter
  :commands (git-gutter-mode)
  :hook ((prog-mode . api|git-gutter-maybe)
         (latex-mode . api|git-gutter-maybe)
         (text-mode . api|git-gutter-maybe)
         (conf-mode . api|git-gutter-maybe))
  :preface
  (defun api|git-gutter-maybe ()
    "Enable `git-gutter-mode' in non-remote buffers."
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (progn
        (require 'git-gutter-fringe)
        (git-gutter-mode +1))))
  ;; (dolist (hook '(prog-mode-hook
  ;;                 latex-mode-hook
  ;;                 text-mode-hook
  ;;                 conf-mode-hook))
  ;;   (add-hook hook #'api|git-gutter-maybe))

  :config
  (defhydra api@git-gutter (:body-pre (git-gutter-mode 1)
                                      :hint nil)
    "
                                     ╭─────────────────┐
  Movement   Hunk Actions     Misc.  │ gg: +%-4s(car (git-gutter:statistic))/ -%-3s(cdr (git-gutter:statistic)) │
  ╭──────────────────────────────────┴─────────────────╯
     ^_g_^       [_s_] stage        [_R_] set start Rev
     ^_k_^       [_r_] revert
     ^↑ ^      [_m_] mark
     ^↓ ^      [_p_] popup          ╭──────────────────────
     ^_j_^                          │[_q_] quit
     ^_G_^                          │[_Q_] Quit and disable"
    ("j" (progn (git-gutter:next-hunk 1)
                (recenter)))
    ("k" (progn (git-gutter:previous-hunk 1)
                (recenter)))
    ("g" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("G" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("m" git-gutter:mark-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (git-gutter-mode -1) :color blue))

  (setq-default fringes-outside-margins t)

  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX....")

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (defun api|update-git-gutter ()
    "Refresh git-gutter on `ESC'. Return nil to prevent shadowing other
 `+evil-esc-hook' hooks."
    (when git-gutter-mode
      (git-gutter)
      nil))
  (add-hook '+evil-esc-hook #'api|update-git-gutter t))

(provide 'setup-git)
;;; setup-git.el ends here
