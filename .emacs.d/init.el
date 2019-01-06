;;; init.el --- Emacs configuration of Anand Iyer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst user-emacs-config-directory
  (expand-file-name (concat user-emacs-directory "config/"))
  "Directory for storing user's local files.")

(defconst user-emacs-modules-directory
  (expand-file-name (concat user-emacs-directory "lisp/"))
  "Directory for storing modules.")

(defconst user-emacs-data-directory
  (expand-file-name (concat user-emacs-directory "data/"))
   "Directory where cache files are stored.")

(defconst user-emacs-temp-directory
   (expand-file-name (concat user-emacs-directory "tmp/"))
    "Directory where temp files are stored.")

(defconst org-root-directory
  "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org-mode/"
  "Directory where org files are stored.")

(setq-default load-prefer-newer t)

(eval-and-compile

  ;; Temporarily change GC threshold and file handler alist.
  (when (version< emacs-version "27.0")
    (unless (or after-init-time noninteractive)
      (defvar api--file-name-handler-alist file-name-handler-alist)
      (setq gc-cons-threshold (* 512 1024 1024)
            gc-cons-percentage 1.0
            file-name-handler-alist nil)
      (defun api|reset-gc()
        (setq gc-cons-threshold (* 16 1024 1024)
              gc-cons-percentage 0.1
              file-name-handler-alist api--file-name-handler-alist))
      (add-hook 'emacs-startup-hook #'api|reset-gc)))

  (add-to-list 'load-path user-emacs-modules-directory)

  (require 'cl-lib)

  ;; useful functions and macros.
  (require 'init-utils)
  ;; calls (package-initialize)
  (require 'init-package)
  (require 'init-core)
  (require 'init-theme)
  (require 'init-fonts)
  (require 'init-ui)
  (require 'init-frames)
  (require 'init-windows)
  (require 'init-modeline)
  (require 'init-mac)
  (require 'init-editor)
  (require 'init-evil)
  (require 'init-keybindings)

  ;;(require 'setup-helm)
  (require 'setup-ivy)

  ;; Packages.
  (require 'setup-company)
  (require 'setup-flycheck)
  (require 'setup-yasnippet)
  (require 'setup-projectile)
  (require 'setup-spellcheck)
  (require 'setup-jump)
  (require 'setup-git)
  (require 'setup-org)
  (require 'setup-pdf)
  (require 'setup-tex)
  (require 'setup-scala)
  (require 'setup-email)
  ;;(require 'setup-music)

  ;; Let emacsclients connect.
  (require 'server)
  (unless (server-running-p) (server-start))

  (when (file-exists-p custom-file)
    (load custom-file))

  (require 'init-locales))

(provide 'init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
