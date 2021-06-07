;;; setup-ivy.el --- Ivy setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Setup ivy and related packages.

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  :ensure ivy-hydra
  :hook (emacs-startup . ivy-mode)
  :config
  (setq ivy-height 12
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers t
        smex-completion-method 'ivy
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; Do not show ^
        ivy-initial-inputs-alist nil)

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
;; `ivy-posframe':
;;------------------------------------------------------------------------------
(use-package ivy-posframe
  ;;:disabled
  :hook (ivy-mode . ivy-posframe-mode)
  :config
  (defun api/ivy-posframe-get-size ()
    "Set the ivy-posframe size according to the current frame."
    (let ((height (or ivy-posframe-height (or ivy-height 10)))
          (width (min (or ivy-posframe-width 200) (round (* 0.99 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))

  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left))
        ivy-fixed-height-minibuffer nil
        ivy-posframe-size-function 'api/ivy-posframe-get-size
        ;;ivy-posframe-width '(frame-width)
        ivy-posframe-border-width 10))

;;------------------------------------------------------------------------------
;; `ivy-rich': Show more info in ivy-switch-buffer.
;;------------------------------------------------------------------------------
(use-package ivy-rich
  :commands (ivy-rich-switch-buffer-transformer)
  :hook (ivy-mode . ivy-rich-mode)
  :init
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev))

;;------------------------------------------------------------------------------
;; `swiper':
;;------------------------------------------------------------------------------
(use-package swiper
  :commands (swiper
             swiper-all))

;;------------------------------------------------------------------------------
;; `counsel':
;;------------------------------------------------------------------------------
(use-package counsel
  :commands (counsel-ag counsel-rg counsel-pt counsel-apropos counsel-bookmark
             counsel-describe-function counsel-describe-variable
             counsel-describe-face counsel-M-x counsel-file-jump
             counsel-find-file counsel-find-library counsel-info-lookup-symbol
             counsel-imenu counsel-recentf counsel-yank-pop
             counsel-descbinds counsel-org-capture counsel-grep-or-swiper)
  :config
  ;; Counsel resets the ivy-initial-inputs-alist to default, so
  ;; set it not to show ^.
  (setq ivy-initial-inputs-alist nil))

;;------------------------------------------------------------------------------
;; `smex': Used by counsel-M-x
;;------------------------------------------------------------------------------
(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (smex-initialize))

(provide 'setup-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-ivy.el ends here
