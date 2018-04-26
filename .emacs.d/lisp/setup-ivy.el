;;; setup-ivy.el --- Ivy setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Setup ivy and related packages.

;;; Code:

;;------------------------------------------------------------------------------
;; Allow persisting ivy-views.
;;------------------------------------------------------------------------------
(defconst ivy-views-file
  (expand-file-name (concat user-emacs-data-directory "ivy/ivy-views.el"))
  "File in which ivy-views will be saved.")

;;------------------------------------------------------------------------------
;; `ivy':
;;------------------------------------------------------------------------------
(use-package ivy
  :diminish ivy-mode
  :ensure ivy-hydra
  :init
  (add-hook 'emacs-startup-hook #'ivy-mode)
  :config
  (setq ivy-height 12
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers t
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
    (if (file-exists-p ivy-views-file)
        (progn (setq ivy-views
                     (with-temp-buffer (insert-file-contents ivy-views-file)
                                       (read (current-buffer))))
               (message "Loaded ivy-views from file %s." ivy-views-file))
      (message "File %s does not exist!" ivy-views-file)))

  (api/load-ivy-views))

;;------------------------------------------------------------------------------
;; `ivy-rich': Show more info in ivy-switch-buffer.
;;------------------------------------------------------------------------------
(use-package ivy-rich
  :commands (ivy-rich-switch-buffer-transformer)
  :init
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev)
  (after! ivy
    (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)))

;;------------------------------------------------------------------------------
;; `swiper':
;;------------------------------------------------------------------------------
(use-package swiper
  :commands (swiper
             swiper-all)
  :defer t)

;;------------------------------------------------------------------------------
;; `counsel':
;;------------------------------------------------------------------------------
(use-package counsel
  :defer t
  :commands (counsel-ag counsel-rg counsel-pt counsel-apropos counsel-bookmark
             counsel-describe-function counsel-describe-variable
             counsel-describe-face counsel-M-x counsel-file-jump
             counsel-find-file counsel-find-library counsel-info-lookup-symbol
             counsel-imenu counsel-recentf counsel-yank-pop
             counsel-descbinds counsel-org-capture counsel-grep-or-swiper))

;;------------------------------------------------------------------------------
;; `smex': Used by counsel-M-x
;;------------------------------------------------------------------------------
(use-package smex
  :commands (smex smex-major-mode-commands)
  :defer t
  :config
  (smex-initialize))

(provide 'setup-ivy)
;;; setup-ivy.el ends here
