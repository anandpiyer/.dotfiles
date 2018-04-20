;;; init-ui.el --- UI settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Defaults.
;;------------------------------------------------------------------------------
(setq-default inhibit-startup-message t
              inhibit-startup-echo-area-message user-login-name
              inhibit-default-init t
              initial-major-mode 'fundamental-mode
              initial-scratch-message nil

              ;; don't use visual bells or dialog boxes.
              use-dialog-box nil
              ring-bell-function #'ignore
              visible-bell nil

              ;; update ui less often.
              idle-update-delay 2

              ;; don't show advice warnings.
              ad-redefinition-action 'accept

              ;; make 'apropos' useful.
              apropos-do-all t

              ;; confirm before visiting non-existing file/buffer.
              confirm-nonexistent-file-or-buffer t

              ;; don't allow minibuffer commands while in minbuffer.
              enable-recursive-minibuffers nil

              ;; disable bidirectional text for tiny performance boost
              bidi-display-reordering nil

              ;; remove continuation arrow on right fringe
              fringe-indicator-alist (delq
                                      (assq 'continuation fringe-indicator-alist)
                                      fringe-indicator-alist)

              ;; keep the point out of the minibuffer
              minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

              ;; don't show buffer boundaries visually.
              indicate-buffer-boundaries nil

              ;; don't show empty lines.
              indicate-empty-lines nil

              ;; no need to make cursor overly visible.
              visible-cursor nil

              ;; don't stretch block cursor.
              x-stretch-cursor nil

              ;; don't ask for save confirmation before compilation, just do it.
              compilation-ask-about-save nil

              ;; scroll compilation output.
              compilation-scroll-output t

              ;; kill existing compilation before starting new, don't ask.
              compilation-always-kills t)

;;------------------------------------------------------------------------------
;; Avoid showing ugly stuff.
;;------------------------------------------------------------------------------
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

;; standardize default fringe width
(if (fboundp 'fringe-mode) (fringe-mode 4))

;; Make mouse less jumpy
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Enable y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; be quiet at startup; don't load or display anything unnecessary
(advice-add #'display-startup-echo-area-message :override #'ignore)

;;------------------------------------------------------------------------------
;; `all-the-icons': icons for many ui related packages.
;;------------------------------------------------------------------------------
(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
             all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
             all-the-icons-install-fonts all-the-icons-icon-for-buffer))

;;------------------------------------------------------------------------------
;; `dashboard': show a dashboard at start.
;;------------------------------------------------------------------------------
(use-package dashboard
  :disabled ;; too heavy.
  :config
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (dashboard-setup-startup-hook))

;;------------------------------------------------------------------------------
;; `dimmer': Interactively highlight which buffer is active by dimming others.
;;------------------------------------------------------------------------------
(use-package dimmer
  :disabled
  :init
  (setq-default dimmer-fraction 0.2)
  (add-hook 'after-init-hook 'dimmer-mode))

;;------------------------------------------------------------------------------
;; `fringe-helper': helper functions for fringe bitmaps.
;;------------------------------------------------------------------------------
(use-package fringe-helper
  :commands (fringe-helper-define
	     fringe-helper-convert)
  :init
  (unless (fboundp 'define-fringe-bitmap)
    ;; doesn't exist in terminal Emacs; define it to prevent errors
    (defun define-fringe-bitmap (&rest _))))

;;------------------------------------------------------------------------------
;; `solaire-mode': distinguish file-visiting buffers from other types of buffers.
;;------------------------------------------------------------------------------
(use-package solaire-mode
  :disabled ;; not very useful.
  :init
  ;; brighten buffers (that represent real files)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; itself off every time Emacs reverts the file
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)

  ;; highlight the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

  ;; if the bright and dark background colors are the wrong way around, use this
  ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
  ;; This should be used *after* you load the active theme!
  ;;
  ;; NOTE: This is necessary for themes in the doom-themes package!
  (solaire-mode-swap-bg))

;;------------------------------------------------------------------------------
;; `which-key': interactively show available commands.
;;------------------------------------------------------------------------------
(use-package which-key
  :diminish which-key-mode
  :init
  (add-hook 'emacs-startup-hook #'which-key-mode)
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-min-display-lines 5
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom))

(provide 'init-ui)
;;; init-ui.el ends here
