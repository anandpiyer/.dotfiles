;;; init-evil.el --- Evil settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------------
;; `evil': Extensible vi layer.
;;------------------------------------------------------------------------------
(use-package evil
  :demand t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-visual-char-semi-exclusive t
        evil-want-Y-yank-to-eol t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil)

  :config
  (evil-mode +1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Set cursor colors later, once theme is loaded
  (defun +evil*init-cursors (&rest _)
    (setq evil-default-cursor (face-background 'cursor nil t)
          evil-normal-state-cursor 'box
          evil-emacs-state-cursor  `(,(face-foreground 'warning) box)
          evil-insert-state-cursor 'bar
          evil-visual-state-cursor 'hollow))
  (advice-add #'load-theme :after #'+evil*init-cursors)

  ;; default modes
  (dolist (mode '(tabulated-list-mode view-mode comint-mode
                  term-mode calendar-mode Man-mode grep-mode))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode '(help-mode debugger-mode))
    (evil-set-initial-state mode 'normal)))

;;------------------------------------------------------------------------------
;; `evil-collection': Set of keybindings for evil.
;;------------------------------------------------------------------------------
;; (use-package evil-collection
;;   :disabled ;; TODO: Too many bugs, wait for stabilization.
;;   :requires (helm) ; if evil-collection is installed before helm, helm craps out.
;;   :after (evil)
;;   :preface
;;   (setq evil-want-integration nil ; must be set before evil is loaded
;;         evil-collection-company-use-tng nil)
;;   :config
;;   (evil-collection-init))

;;------------------------------------------------------------------------------
;; `evil-escape': Key sequence to escape from insert state and everything else.
;;------------------------------------------------------------------------------
(use-package evil-escape
  :commands evil-escape-mode
  :after evil
  :init
  (add-hook 'after-init-hook #'evil-escape-mode)
  :config
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode)
        evil-escape-key-sequence "fd"
        evil-escape-delay 0.25))

;;------------------------------------------------------------------------------
;; `evil-goggles': Displays a visual hint when editing with evil.
;;------------------------------------------------------------------------------
(use-package evil-goggles
  :after evil
  :init
  (add-hook 'after-init-hook #'evil-goggles-mode)
  :config
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil
        evil-goggles-enable-delete nil))

;;------------------------------------------------------------------------------
;; `evil-matchit': Press “%” to jump between matched tags in Emacs.
;;------------------------------------------------------------------------------
(use-package evil-matchit
  :after evil
  :init
  (add-hook 'emacs-startup-hook #'global-evil-matchit-mode))

;;------------------------------------------------------------------------------
;; `evil-mc': Multiple cursors implementation for evil-mode.
;;------------------------------------------------------------------------------
(use-package evil-mc
  :after hydra
  :commands (evil-mc-make-all-cursors
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-here
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor
             evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-skip-and-goto-prev-match)
  :init
  ;(defvar evil-mc-key-map (make-sparse-keymap))
  ;; remove emc prefix when there is not multiple cursors
  (setq evil-mc-undo-cursors-on-keyboard-quit t
        evil-mc-mode-line
        `(:eval (when (> (evil-mc-get-cursor-count) 1)
                  (format ,(propertize " %s:%d" 'face 'cursor)
                          evil-mc-mode-line-prefix
                          (evil-mc-get-cursor-count)))))
  (defhydra api@multiple-cursors (:hint nil)
   "
      ^Up^            ^Down^        ^Miscellaneous^
 ----------------------------------------------
 [_p_]   Next    [_n_]   Next    [_a_] Match all
 [_P_]   Skip    [_N_]   Skip    [_q_] Quit
"
   ("a" evil-mc-make-all-cursors)
   ("n" evil-mc-make-and-goto-next-match)
   ("N" evil-mc-skip-and-goto-next-match)
   ("p" evil-mc-make-and-goto-prev-match)
   ("P" evil-mc-skip-and-goto-prev-match)
   ("q" nil))

  :config
  (global-evil-mc-mode 1)

  (defun api|escape-multiple-cursors ()
    "Clear evil-mc cursors and restore state."
    (when (evil-mc-has-cursors-p)
      (evil-mc-undo-all-cursors)
      (evil-mc-resume-cursors)
      t))
  (advice-add #'evil-force-normal-state :after #'api|escape-multiple-cursors))

;;------------------------------------------------------------------------------
;; `evil-multiedit': Multiedit capability in evil.
;;------------------------------------------------------------------------------
(use-package evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-toggle-marker-here
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-restore
             evil-multiedit-abort
             evil-multiedit-ex-match))

;;------------------------------------------------------------------------------
;; `evil-nerd-commenter': Block commenting.
;;------------------------------------------------------------------------------
(use-package evil-nerd-commenter)

(provide 'init-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
