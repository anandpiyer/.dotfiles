;;; init.el --- Personal emacs configuration of Anand Iyer -*- lexical-binding: t; -*-

;;; Commentary:
;; https://github.com/wasamasa/dotemacs/blob/master/init.org
;; https://github.com/bling/dotemacs
;; https://github.com/tonini/emacs.d

;;; Code:

;; The following comment avoids emacs automatically adding (package-initialize)
;; (package-initialize)

(defconst user-emacs-local-directory
  (expand-file-name (concat user-emacs-directory ".local/"))
  "Directory for storing user's local files.")

(defconst user-emacs-modules-directory
  (expand-file-name (concat user-emacs-directory "lisp/"))
  "Directory for storing modules.")

(defconst user-emacs-cache-directory
  (expand-file-name (concat user-emacs-local-directory "cache/"))
   "Directory where cache files are stored.")

(defconst user-emacs-temp-directory
   (expand-file-name (concat user-emacs-local-directory "tmp/"))
    "Directory where temp files are stored.")

(defvar org-root-directory
  "~/Dropbox/org-mode/"
  "Directory where org files are stored.")

;; Prefer UTF-8 everywhere.
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Defaults
(setq-default
  ; always prefer newer byte code.
  load-prefer-newer t

  abbrev-file-name       (concat user-emacs-local-directory "abbrev.el")
  auto-save-list-file-name (concat user-emacs-cache-directory "autosave")
  backup-directory-alist (list (cons "." (concat user-emacs-cache-directory "backup/")))
  pcache-directory (concat user-emacs-cache-directory "pcache/")
  url-cache-directory (concat user-emacs-cache-directory "url/")
  url-configuration-directory (concat user-emacs-local-directory "url/"))

; dont litter my init file!
(setq custom-file (concat user-emacs-local-directory "custom.el"))
(load custom-file t t)

(defun api//byte-compile ()
  "Byte compile all user init files."
  (interactive)
  (let (compile-targets)
    (setq compile-targets
          (nconc
           (nreverse (directory-files-recursively user-emacs-modules-directory "\\.el$"))))
    (push (expand-file-name "init.el" user-emacs-directory) compile-targets)

    (dolist (target compile-targets)
      (byte-compile-file target)
      (load target t t))))

(defun remove-elc-on-save ()
  "If we're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(defun api//clean-byte-compiled-files ()
  "Delete all compiled elc files excluding packages."
  (interactive)
  (let ((targets (append (list (expand-file-name "init.elc" user-emacs-directory))
                         (directory-files-recursively user-emacs-modules-directory "\\.elc$")))
        (default-directory user-emacs-directory))
    (unless (cl-loop for path in targets
                     if (file-exists-p path)
                     collect path
                     and do (delete-file path)
                     and do (message "✓ Deleted %s" (file-relative-name path)))
      (message "Everything is clean"))))

(eval-and-compile
  (defvar api--file-name-handler-alist file-name-handler-alist)

  (unless (or after-init-time noninteractive)
    (setq gc-cons-threshold 402653184
          gc-cons-percentage 0.6
          file-name-handler-alist nil))

  (require 'cl-lib)

  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  (require 'init-package)
;  (require 'init-core)
  (require 'init-ui)
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
  ;(require 'setup-treemacs)

  (require 'setup-git)
  (require 'setup-org)
  (require 'setup-tex)
  (require 'setup-scala)

  (require 'server)
  (unless (server-running-p) (server-start))

  (add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

  (defun api|finalize ()
    "Finalize everything and reset GC."
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist api--file-name-handler-alist)
    t)

  (add-hook 'emacs-startup-hook #'api|finalize))

(provide 'init)

;;; init.el ends here
