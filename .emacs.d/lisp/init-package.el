;;; init-package.el --- Initialize package related things -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Declare archives and bootstrap use-package.
;;
;;; Code:
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/"))
      package-enable-at-startup nil)

(package-initialize)

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t
      use-package-verbose t)

(require 'diminish)
(require 'bind-key)

;; Full frame
(use-package fullframe
  :config (fullframe list-packages quit-window))

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
