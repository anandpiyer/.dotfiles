;;; core.el --- Core Emacs stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; `benchmark-init': Benchmark init files.
;;------------------------------------------------------------------------------
(use-package benchmark-init
  :init
  (require 'benchmark-init)
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;------------------------------------------------------------------------------
;; `desktop': Allow saving sessions.
;;------------------------------------------------------------------------------
(use-package desktop
  :disabled
  :ensure nil ;; in-built
  :init
  (progn
    (desktop-save-mode 1)
    (setq desktop-auto-save-timeout 60
          desktop-globals-to-save
          (append '((file-name-history . 100)
                    (compile-history . 100)
                    (command-history . 100)
                    (extended-command-history . 100)
                    (shell-command-history . 100)
                    (query-replace-history . 100)
                    (regexp-history . 100)
                    (grep-history . 100)
                    (minibuffer-history . 100))
                  desktop-globals-to-save))))

;;------------------------------------------------------------------------------
;; `fullscreen': Advice commands to execute fullscreen, restoring original
;; window setup when exiting.
;;------------------------------------------------------------------------------
(use-package fullframe
  :init
  (defun api|init-fullframe ()
    (fullframe list-package quit-window)))

;;------------------------------------------------------------------------------
;; `no-littering': do not litter emacs.d.
;;------------------------------------------------------------------------------
(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "data/" user-emacs-directory))
  (require 'no-littering)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;;------------------------------------------------------------------------------
;; `terminal-here': launch terminals at current file location.
;;------------------------------------------------------------------------------
(use-package terminal-here
  :commands (terminal-here-launch terminal-here-project-launch))

(provide 'init-core)
;;; init-core.el ends here
