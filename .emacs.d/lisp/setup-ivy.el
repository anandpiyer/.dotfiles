;;; setup-ivy.el --- Ivy setup -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Setup ivy and related packages.
;;
;;; Code:

(defconst ivy-views-file
  (expand-file-name (concat user-emacs-cache-directory "ivy-views"))
  "File in which ivy-views will be saved.")

(use-package ivy
  :diminish ivy-mode
  :init
  (add-hook 'emacs-startup-hook #'ivy-mode)
  :config
  (setq ivy-height 12
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        smex-completion-method 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil)

  (defun api/save-ivy-views ()
    "Save ivy-views to file."
    (interactive)
    (with-temp-file ivy-views-file
      (prin1 ivy-views (current-buffer))
      (message "Saved ivy-views to file %s." ivy-views-file)))

  (defun api/load-ivy-views ()
    "Load ivy-views from file."
    (interactive)
    (setq ivy-views
          (with-temp-buffer (insert-file-contents ivy-views-file)
                            (read (current-buffer))))
    (message "Loaded ivy-views from file %s." ivy-views-file))

  (api/load-ivy-views))

(use-package swiper
  :commands (swiper swiper-all)
  :defer t)

(use-package counsel
  :defer t
  :requires ivy
  :ensure counsel-projectile
  :config
  (require 'counsel-projectile))

;; Used by `counsel-M-x'
(use-package smex
  :commands (smex smex-major-mode-commands)
  :defer t
  :config
  (setq smex-save-file (concat user-emacs-cache-directory "/smex-items"))
  (smex-initialize))

(provide 'setup-ivy)
;;; setup-ivy.el ends here
