;;; init-utils.el --- helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Byte compilation helper functions.
;;------------------------------------------------------------------------------
(defun api/byte-compile ()
  "Byte compile all user init files."
  (interactive)
  (let (compile-targets)
    (setq compile-targets
          (nconc
           (nreverse (directory-files-recursively user-emacs-modules-directory "\\.el$"))))
    (push (expand-file-name "init.el" user-emacs-directory) compile-targets)

    (let ((byte-compile-warnings nil))
      (dolist (target compile-targets)
        (byte-compile-file target)))
    ;;(load target t t)
    (message "Compilation complete. Restart Emacs to load.")))

(defun remove-elc-on-save ()
  "If we're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(defun api/clean-byte-compiled-files ()
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

(provide 'init-utils)
;;; init-utils.el ends here
