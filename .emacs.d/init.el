;;; init.el --- Personal emacs configuration of Anand Iyer -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(defvar org-root-directory
  "~/Dropbox/org-mode/"
  "Directory where org files are stored.")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq-default load-prefer-newer t)

;;------------------------------------------------------------------------------
;; Bootstrap configuration.
;;------------------------------------------------------------------------------
(eval-and-compile

  ;; Temporarily change GC threshold and file handler alist.
  (defvar api--file-name-handler-alist file-name-handler-alist)
  (unless (or after-init-time noninteractive)
    (progn
      (setq gc-cons-threshold (* 512 1024 1024)
            gc-cons-percentage 0.6
            file-name-handler-alist nil)
      (add-hook 'after-init-hook
                (lambda ()
                  (setq gc-cons-threshold (* 16 1024 1024)
                        gc-cons-percentage 0.1
                        file-name-handler-alist api--file-name-handler-alist)))))

  (require 'cl-lib)

  ;; useful functions and macros.
  (require 'init-utils)
  ;; calls (package-initialize)
  (require 'init-package)
  (require 'init-core)
  (require 'init-mac)
  (require 'init-ui)
  (require 'init-modeline)
  (require 'init-workspace)
  (require 'init-editor)
  (require 'init-keybindings)

  ;; Packages.
  (require 'setup-company)
  (require 'setup-flycheck)
  (require 'setup-helm)
  (require 'setup-ivy)
  (require 'setup-yasnippet)
  (require 'setup-projectile)
  (require 'setup-spellcheck)
  (require 'setup-treemacs)
  (require 'setup-jump)

  (require 'setup-git)
  (require 'setup-org)
  (require 'setup-tex)
  (require 'setup-scala)

  ;; Let emacsclients connect.
  (require 'server)
  (unless (server-running-p) (server-start))

  (require 'init-locales)

  ;;(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

  ;; (defun api|finalize ()
  ;;   "Finalize everything and reset GC."
  ;;   (setq gc-cons-threshold 16777216
  ;;         gc-cons-percentage 0.1
  ;;         file-name-handler-alist api--file-name-handler-alist)
  ;;   t)

  ;;(add-hook 'emacs-startup-hook #'api|finalize))
  )

(provide 'init)

;;; init.el ends here
