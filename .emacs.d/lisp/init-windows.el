;;; init-windows.el --- Emacs windows -*- lexical-binding: t; -*-

;;; Commentary:

;; These are settings related to Emacs windows (not Windows as in OS)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default highlight-nonselected-windows nil

              ;; hide cursors in other windows
              cursor-in-non-selected-windows nil

              ;; let minibuffer windows resize automatically.
              resize-mini-windows 'grow-only

              ;; maximum size for mini-windows.
              max-mini-window-height 0.4

              ;; lean towards horizontal splits.
              ;;split-width-threshold 999)
)
;;------------------------------------------------------------------------------
;; set inter-window bordto be minimal,
;;------------------------------------------------------------------------------
(setq-default window-divider-default-places t
              window-divider-default-bottom-width 0
              window-divider-default-right-width 1)
(add-hook 'after-init-hook #'window-divider-mode)

;;------------------------------------------------------------------------------
;; Winner mode for quick window configurations.
;;------------------------------------------------------------------------------
(when (fboundp 'winner-mode) (winner-mode 1))

;;------------------------------------------------------------------------------
;; Allow switching between active windows using Shift + arrow keys.
;;------------------------------------------------------------------------------
(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

;;------------------------------------------------------------------------------
;; `ace-window': Visually select windows.
;;------------------------------------------------------------------------------
(use-package ace-window
  ;;:diminish ace-window-display-mode
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'global
        aw-background t))

;;------------------------------------------------------------------------------
;; `eyebrowse': Save and retrieve window layouts.
;;------------------------------------------------------------------------------
(use-package eyebrowse
  ;;:disabled
  :init
  (add-to-list 'window-persistent-parameters '(window-side . writable))
  (add-to-list 'window-persistent-parameters '(window-slot . writable))
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-mode-line-left-delimiter " "
        eyebrowse-mode-line-right-delimiter " "
        eyebrowse-wrap-around t
        eyebrowse-new-workspace t)
  (eyebrowse-mode))

;;------------------------------------------------------------------------------
;; `golden-ratio': Automatically resize windows.
;;------------------------------------------------------------------------------
(use-package golden-ratio
  :config
  (progn
    ;; golden-ratio-exclude-modes
    (dolist (m '("bs-mode"
                 "calc-mode"
                 "ediff-mode"
                 "dired-mode"
                 "gud-mode"
                 "gdb-locals-mode"
                 "gdb-registers-mode"
                 "gdb-breakpoints-mode"
                 "gdb-threads-mode"
                 "gdb-frames-mode"
                 "gdb-inferior-io-mode"
                 "gdb-disassembly-mode"
                 "gdb-memory-mode"
                 "speedbar-mode"
                 ))
      (add-to-list 'golden-ratio-exclude-modes m))

    (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

    ;; golden-ratio-extra-commands
    (dolist (f '(ace-window
                 ace-delete-window
                 ace-select-window
                 ace-swap-window
                 ace-maximize-window
                 avy-pop-mark
                 buf-move-left
                 buf-move-right
                 buf-move-up
                 buf-move-down
                 evil-avy-goto-word-or-subword-1
                 evil-avy-goto-line
                 evil-window-delete
                 evil-window-split
                 evil-window-vsplit
                 evil-window-left
                 evil-window-right
                 evil-window-up
                 evil-window-down
                 evil-window-bottom-right
                 evil-window-top-left
                 evil-window-mru
                 evil-window-next
                 evil-window-prev
                 evil-window-new
                 evil-window-vnew
                 evil-window-rotate-upwards
                 evil-window-rotate-downwards
                 evil-window-move-very-top
                 evil-window-move-far-left
                 evil-window-move-far-right
                 evil-window-move-very-bottom
                 quit-window
                 winum-select-window-0-or-10
                 winum-select-window-1
                 winum-select-window-2
                 winum-select-window-3
                 winum-select-window-4
                 winum-select-window-5
                 winum-select-window-6
                 winum-select-window-7
                 winum-select-window-8
                 winum-select-window-9
                 windmove-left
                 windmove-right
                 windmove-up
                 windmove-down))
      (add-to-list 'golden-ratio-extra-commands f))

    ;; golden-ratio-exclude-buffer-names
    (dolist (n '(" *NeoTree*"
                 "*LV*"
                 " *which-key*"))
      (add-to-list 'golden-ratio-exclude-buffer-names n))))

;;------------------------------------------------------------------------------
;; `shackle': Tame pop-up windows.
;;------------------------------------------------------------------------------
(use-package shackle
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 8
        helm-display-function 'pop-to-buffer
        shackle-rules
        '(("^\\*eww" :regexp t :size 0.5 :select t :autokill t :noesc t)
          ("^\\*ftp " :noselect t :autokill t :noesc t)
          ("^\\*pdf" :noselect t :align right)
          ;;(pdf-view-mode :noselect t :align right)
          ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.3)
          ;; built-in (emacs)
          ("*compilation*" :size 0.25 :noselect t :autokill t :autoclose t)
          ("*ert*" :same t :modeline t)
          ("*info*" :size 0.5 :select t :autokill t)
          ("*undo-tree*" :size 0.25 :align right)
          ("*Backtrace*" :size 20 :noselect t)
          ("*Warnings*"  :size 12 :noselect t :autofit t)
          ("*Messages*"  :size 12 :noselect t)
          ("*Help*" :size 0.3 :autokill t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 20 :noselect t :autokill t)
          (apropos-mode :size 0.3 :autokill t :autoclose t)
          (Buffer-menu-mode :size 20 :autokill t)
          (comint-mode :noesc t)
          (grep-mode :size 25 :noselect t :autokill t)
          (profiler-report-mode :size 0.3 :regexp t :autokill t :modeline minimal)
          (tabulated-list-mode :noesc t)
          ("^ ?\\*" :regexp t :size 15 :noselect t :autokill t :autoclose t)))
  (shackle-mode))

;;------------------------------------------------------------------------------
;; `winum': Allow window selection by number.
;;------------------------------------------------------------------------------
(use-package winum
  :init
  (add-hook 'emacs-startup-hook 'winum-mode)
  :config
  (setq winum-scope 'global
        winum-auto-setup-mode-line nil))

;;------------------------------------------------------------------------------
;; `zoom': Automatically zoom active window.
;;------------------------------------------------------------------------------
(use-package zoom
  :disabled ;; doesn't play well with which-key
  :config
  ;(setq which-key-popup-type 'minibuffer)
  (setq zoom-ignored-major-modes '(calc-mode
                                   dired-mode
                                   ediff-mode
                                   markdown-mode
                                   speedbar-mode
                                   which-key-mode)
        zoom-ignored-buffer-name-regexps '("^*calc"
                                           "^\\*[hH]elm.*")
        zoom-ignored-buffer-names '(" *NeoTree*"
                                    " *which-key*")))

(provide 'init-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-windows.el ends here
