;;; init-editor.el --- Editor related settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default fill-column 80
              word-wrap t
              sentence-end-double-space nil

              ;; use spaces, not tabs
              indent-tabs-mode nil
              tab-always-indent t
              tab-width 4

              ;;
              auto-save-default nil
              create-lockfiles nil
              history-length 500
              make-backup-files nil

              ;; Scrolling
              hscroll-margin 1
              hscroll-step 1
              scroll-conservatively 101
              scroll-margin 0
              scroll-preserve-screen-position t

              size-indication-mode t
             ; line-number-mode t
              display-line-numbers-width 3
              column-number-mode t
              delete-selection-mode t)

;; Show matching paranthesis.
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'after-init-hook #'show-paren-mode)

;; turn on auto-fill mode
(use-package auto-fill-mode
  :ensure nil ;; in-built
  :commands (auto-fill-mode)
  :init (add-hook 'text-mode-hook #'auto-fill-mode))

;; whitespace
(use-package whitespace
  :ensure nil ;; in-built
  :commands (whitespace-cleanup whitespace-mode)
  :init
  (dolist (hook '(prog-mode-hook
                  tex-mode-hook
                  latex-mode-hook
                  text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face tabs empty trailing lines-tail)))

;; Highlight text that extends beyond a certain column.
;; (use-package column-enforce-mode
;;   :config
;;   (setq column-enforce-comments nil
;;         column-enforce-column 80)

;;   (dolist (hook '(prog-mode-hook
;;                   latex-mode-hook
;;                   text-mode-hook))
;;     (add-hook hook 'column-enforce-mode)))

;; Highlight the current line.
(use-package hl-line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

;; Recent files
(use-package recentf
  :hook (emacs-startup-hook . recentf-mode)
  :config
  (setq recentf-save-file (concat user-emacs-cache-directory "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$" "COMMIT_EDITMSG\\"
              (concat "^" (file-truename user-emacs-local-directory)))))

;; Smart paranthesis
(use-package smartparens
  :config
  (smartparens-global-mode +1)
  (require 'smartparens-config)
  (setq sp-autowrap-region t
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3)

  ;; disable smartparens in evil-mode's replace state (they conflict)
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook #'turn-on-smartparens-mode))

;; Make smart paranthesis play nice with evil.
;; (use-package evil-smartparens
;;   :ensure smartparens
;;   :config (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;; Highlight indentations
(use-package highlight-indent-guides
  :commands (highlight-indentation-mode
             highlight-indentation-current-column-mode)
  :init (setq highlight-indent-guides-method 'character))

;; visualize for better undo
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (add-hook 'emacs-startup-hook #'global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        (list (cons "." (concat user-emacs-cache-directory "undo-tree-hist/")))
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

;; Highlight all symbols that match the selected.
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("C-h" . highlight-symbol))

(use-package evil-collection
  :disabled
  :requires (helm) ; if evil-collection is installed before helm, helm craps out.
  :after (evil)
  :preface
  (setq evil-want-integration nil ; must be set before evil is loaded
        evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

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

;; Customizable key sequence to escape from insert state and everything else.
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

;; Displays a visual hint when editing with evil.
(use-package evil-goggles
  :after evil
  :init
  (add-hook 'after-init-hook #'evil-goggles-mode)
  :config
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil
        evil-goggles-enable-delete nil))

;; Commenting blocks of code.
(use-package evil-nerd-commenter :defer t)

;;
;; Line numbers
;;
(defvar api-line-numbers-style 'relative
  "The style to use for the line number display that emulates
 `display-line-numbers' styles, which are:
t           Ordinary line numbers
'relative   Relative line numbers")

;; (defun api|enable-line-numbers ()
;;   (interactive)
;;   (if (boundp 'display-line-numbers)
;;       (setq display-line-numbers api-line-numbers-style)
;;   (if (eq api-line-numbers-style 'relative)
;;         (linum-relative-mode)
;;       (linum-mode))))

;; (defun api|disable-line-numbers ()
;;   (interactive)
;;   (when (boundp 'display-line-numbers)
;;     (setq display-line-numbers nil))
;;   (linum-mode 0))
(defun api/enable-line-numbers ()
  "Show line number display."
  (interactive)
  (if (boundp 'display-line-numbers)
      (setq display-line-numbers api-line-numbers-style)))

(defun api/disable-line-numbers ()
  "Disable line number display."
  (interactive)
  (if (boundp 'display-line-numbers)
      (setq display-line-numbers nil)))

;; (use-package linum-relative
;;   :disabled
;;   :unless (boundp 'display-line-numbers)
;;   :diminish linum-relative-mode
;;   :commands linum-relative-mode
;;   :init
;;   (linum-mode 0)
;;   (setq linum-format "%4d"
;;         linum-relative-current-symbol ""))

(dolist (hook '(prog-mode-hook
                text-mode-hook))
  (add-hook hook #'api/enable-line-numbers))

;; show fill column
;; (use-package fill-column-indicator
;;   :diminish (fci-mode . " ⓕ")
;;   :defer t
;;   :init
;;   (dolist (hooks '(prog-mode-hook
;;                    text-mode-hook
;;                    latex-mode-hook))
;;     (add-hook hooks (lambda () (fci-mode 1)))))

;; Expand selection region by semantic units.
(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

;; Multiple cursors.
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

;; Multiple cursors implementation for evil-mode.
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

(provide 'init-editor)
;;; init-editor.el ends here
