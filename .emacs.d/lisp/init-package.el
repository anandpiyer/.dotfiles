;;; init-package.el --- Package related things -*- lexical-binding: t; -*-
;;; Commentary:
;; Declare archives and bootstrap use-package.
;;; Code:

;;--------------------------------------------------
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("melpa-stable" . "https://stable.melpa.org/packages/")
;;                          ("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")))

;; ; Disable package initialize after us.  We either initialize it
;; ;; anyway in case of interpreted .emacs, or we don't want slow
;; ;; initizlization in case of byte-compiled .emacs.elc.
;; (setq package-enable-at-startup nil)
;; ;; Ask package.el to not add (package-initialize) to .emacs.
;; (setq package--init-file-ensured t)
;; ;; set use-package-verbose to t for interpreted .emacs,
;; ;; and to nil for byte-compiled .emacs.elc
;; (eval-and-compile
;;   (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; ;; Add the macro generated list of package.el loadpaths to load-path.
;; (mapc #'(lambda (add) (add-to-list 'load-path add))
;;       (eval-when-compile
;;         ;; (require 'package)
;;         (package-initialize)
;;         ;; Install use-package if not installed yet.
;;         (unless (package-installed-p 'use-package)
;;           (package-refresh-contents)
;;           (package-install 'use-package))
;;         ;; (require 'use-package)
;;         (setq use-package-always-ensure t
;;               use-package-always-defer t)
;;         (let ((package-user-dir-real (file-truename package-user-dir)))
;;           ;; The reverse is necessary, because outside we mapc
;;           ;; add-to-list element-by-element, which reverses.
;;           (nreverse (apply #'nconc
;;                            ;; Only keep package.el provided loadpaths.
;;                            (mapcar #'(lambda (path)
;;                                        (if (string-prefix-p package-user-dir-real path)
;;                                            (list path)
;;                                          nil))
;;                                    load-path))))))

;; ;; use-package for the case when init.el is byte-compiled
;; (use-package diminish)
;; (use-package bind-key)
;; ;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
;; (use-package use-package :commands use-package-autoload-keymap)
;;--------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/"))
      package-enable-at-startup nil)

(unless package--initialized (package-initialize t))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-minimum-reported-time 0.01
      use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-always-defer t)

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;------------------------------------------------------------------------------
;; Allow loading packages after some other packages.
;;------------------------------------------------------------------------------
(defmacro after! (feature &rest body)
  "After FEATURE is loaded, evaluate BODY. Supress warnings during compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@body)))

(provide 'init-package)
;;; init-package.el ends here
