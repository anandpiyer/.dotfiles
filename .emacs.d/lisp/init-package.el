;;; init-package.el --- Package related things -*- lexical-binding: t; -*-

;;; Commentary:

;; Declare archives and bootstrap use-package.

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      package-enable-at-startup nil
      package--init-file-ensured t)

(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        ;; (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        (setq use-package-always-ensure t)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

(setq use-package-always-defer t)

;; (if api-debug-enabled
;;     (setq use-package-expand-minimally nil
;;           use-package-compute-statistics t
;;           use-package-minimum-reported-time 0.01
;;           use-package-verbose (not (bound-and-true-p byte-compile-current-file)))
;;   (setq use-package-verbose nil
;;         use-package-expand-minimally t))

(eval-when-compile (require 'use-package))
;;(require 'diminish)
(require 'bind-key)
;;(use-package diminish)
;;(use-package bind-key)
;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
;;(use-package use-package :commands use-package-autoload-keymap)

;;------------------------------------------------------------------------------
;; Allow loading packages after some other packages.
;;------------------------------------------------------------------------------
;; (defmacro after! (feature &rest body)
;;   "After FEATURE is loaded, evaluate BODY. Supress warnings during compilation."
;;   (declare (indent defun) (debug t))
;;   `(,(if (or (not (bound-and-true-p byte-compile-current-file))
;;              (if (symbolp feature)
;;                  (require feature nil :no-error)
;;                (load feature :no-message :no-error)))
;;          #'progn
;;        #'with-no-warnings)
;;     (with-eval-after-load ',feature ,@body)))

;;------------------------------------------------------------------------------
;; `paradox': Project for modernizing Emacs' Package Menu.
;;------------------------------------------------------------------------------
(use-package paradox
  :hook ((emacs-startup . paradox-enable)))

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
