;;; init-editor.el --- Editor related settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Defaults.
;;------------------------------------------------------------------------------
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

;;------------------------------------------------------------------------------
;; Show matching parenthesis.
;;------------------------------------------------------------------------------
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'after-init-hook #'show-paren-mode)

;;------------------------------------------------------------------------------
;; Line numbers.
;;------------------------------------------------------------------------------
(defvar api-line-numbers-style 'relative
  "Style to use for `display-line-numbers' styles, which are:
t           Ordinary line numbers
'relative   Relative line numbers")

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

;; enable line numbers by default in some modes.
(dolist (hook '(prog-mode-hook
                text-mode-hook))
  (add-hook hook #'api/enable-line-numbers))

;;------------------------------------------------------------------------------
;; `auto-fill-mode':
;;------------------------------------------------------------------------------
(use-package auto-fill-mode
  :ensure nil ;; in-built
  :commands (auto-fill-mode)
  :init (add-hook 'text-mode-hook #'auto-fill-mode))

;;------------------------------------------------------------------------------
;; `column-enforce-mode': highlight text that extends beyond a certain column.
;; Main difference from whitespace mode is that this allows skipping comments.
;;------------------------------------------------------------------------------
(use-package column-enforce-mode
  ;;:disabled ;; whitespace-mode is enough for now.
  :config
  (setq column-enforce-comments nil
        column-enforce-column 80)

  (dolist (hook '(prog-mode-hook
                  latex-mode-hook
                  text-mode-hook))
    (add-hook hook 'column-enforce-mode)))

;;------------------------------------------------------------------------------
;; `expand-region': Expand region by semantic units.
;;------------------------------------------------------------------------------
(use-package expand-region
  :commands (er/expand-region
             er/contract-region
             er/mark-symbol
             er/mark-word))

;;------------------------------------------------------------------------------
;; `highlight-indent-guides': highlight indentations.
;;------------------------------------------------------------------------------
(use-package highlight-indent-guides
  :commands (highlight-indentation-mode
             highlight-indentation-current-column-mode)
  :init (setq highlight-indent-guides-method 'character))

;;------------------------------------------------------------------------------
;; `highlight-symbol': highlight symbol at point throughout the current buffer.
;;------------------------------------------------------------------------------
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol)

;;------------------------------------------------------------------------------
;; `hl-line': highlight current line.
;;------------------------------------------------------------------------------
(use-package hl-line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

;;------------------------------------------------------------------------------
;; `rainbow-delimiters': Manage delimiter explosion.
;;------------------------------------------------------------------------------
(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'latex-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;------------------------------------------------------------------------------
;; `recentf': Manage recent files.
;;------------------------------------------------------------------------------
(use-package recentf
  :hook (emacs-startup-hook . recentf-mode)
  :config
  (setq recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$" "COMMIT_EDITMSG\\"
              no-littering-var-directory
              no-littering-etc-directory)))

;;------------------------------------------------------------------------------
;; `smartparens': Smarter parenthesis matching.
;;------------------------------------------------------------------------------
(use-package smartparens
  :config
  (smartparens-global-mode +1)
  (require 'smartparens-config)
  (setq sp-autowrap-region t
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3)

  ;; Make smart paranthesis play nice with evil.
  ;; (use-package evil-smartparens
  ;;   :after smartparens
  ;;   :config (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

  ;; disable smartparens in evil-mode's replace state (they conflict)
  (after! evil
    (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
    (add-hook 'evil-replace-state-exit-hook #'turn-on-smartparens-mode)))

;;------------------------------------------------------------------------------
;; `undo-tree': visualize undo operations.
;;------------------------------------------------------------------------------
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (add-hook 'emacs-startup-hook #'global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

;;------------------------------------------------------------------------------
;; `whitespace': show and clean unnecessary whitespace.
;;------------------------------------------------------------------------------
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

(provide 'init-editor)
;;; init-editor.el ends here
