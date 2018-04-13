;;; setup-helm.el --- helm setup -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; setting related to Helm.
;;
;;; Code:

(use-package helm
  :diminish helm
  :ensure helm-projectile
  :init
  (setq helm-quick-update t
        ;; Speedier without fuzzy matching
        helm-mode-fuzzy-match nil
        helm-buffers-fuzzy-matching nil
        helm-apropos-fuzzy-match nil
        helm-M-x-fuzzy-match nil
        helm-recentf-fuzzy-match nil
        helm-projectile-fuzzy-match nil
        helm-bookmark-show-location t
        ;; Display extraineous helm UI elements
        helm-display-header-line nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; Don't override evil-ex's completion
        helm-mode-handle-completion-in-region nil
        helm-candidate-number-limit 50
        ;; Don't wrap item cycling
        helm-move-to-line-cycle-in-source t)
  (add-hook 'emacs-startup-hook #'helm-mode)

  :config
  (require 'helm-config)
  (setq helm-autoresize-min-height 20
        helm-autoresize-max-height 0
        projectile-completion-system 'helm)

  (load "helm-autoloads" nil t)

  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  (require 'helm-projectile)
  (set-keymap-parent helm-projectile-find-file-map helm-map)

  ;; helm is too heavy for find-file-at-point
  (after! helm-mode
    (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil)))

  (defun +helm*hide-header (&rest _)
    "Hide header-line & mode-line in helm windows."
    (setq mode-line-format nil))
  ;;(advice-add #'helm-display-mode-line :override #'+helm*hide-header)

;;   (map! :map global-map
;;         [remap apropos]                   #'helm-apropos
;;         [remap find-file]                 #'helm-find-files
;;         [remap recentf-open-files]        #'helm-recentf
;;         [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
;;         [remap projectile-recentf]        #'helm-projectile-recentf
;;         [remap projectile-find-file]      #'helm-projectile-find-file
;;         [remap imenu]                     #'helm-semantic-or-imenu
;;         [remap bookmark-jump]             #'helm-bookmarks
;;         [remap noop-show-kill-ring]       #'helm-show-kill-ring
;;         [remap projectile-switch-project] #'helm-projectile-switch-project
;;         [remap projectile-find-file]      #'helm-projectile-find-file
;;         [remap imenu-anywhere]            #'helm-imenu-anywhere
  ;; [remap execute-extended-command] #'helm-M-x))
)
;; (use-package helm
;;   :diminish helm-mode
;;   :defer t
;;   :commands (helm-M-x
;;              helm-find-files
;;              helm-buffers-list)
;;   :bind
;;   (("M-x" . helm-M-x)
;;    ("C-x C-f" . helm-find-files)
;;    ("C-x C-b" . helm-buffers-list)
;;    :map helm-map
;;    ("<tab>" . helm-execute-persistent-action)
;;    ("TAB" . helm-execute-persistent-action)
;;    ("C-i" . helm-execute-persistent-action)
;;    ("C-z" . helm-select-action))

;;   :init
;;   (progn
;;     (setq helm-prevent-escaping-from-minibuffer t
;;           helm-bookmark-show-location t
;;           helm-display-header-line nil
;;           helm-split-window-in-side-p t
;;           helm-always-two-windows t
;;           helm-echo-input-in-header-line t)

;;     ;; fuzzy matching setting
;;     (setq helm-M-x-fuzzy-match t
;;           helm-apropos-fuzzy-match t
;;           helm-file-cache-fuzzy-match t
;;           helm-imenu-fuzzy-match t
;;           helm-lisp-fuzzy-completion t
;;           helm-locate-fuzzy-match t
;;           helm-recentf-fuzzy-match t
;;           helm-semantic-fuzzy-match t
;;           helm-buffers-fuzzy-matching t)

;;     ;; hide minibuffer and use header line
;;     (defun helm-hide-minibuffer-maybe ()
;;       (when (with-helm-buffer helm-echo-input-in-header-line)
;;         (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;           (overlay-put ov 'window (selected-window))
;;           (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
;;                                   `(:background ,bg-color :foreground ,bg-color)))
;;           (setq-local cursor-type nil))))

;;     (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe))

;;   :config
;;   (progn
;;     (require 'helm-config)
;;     (helm-mode 1)
;;     (setq helm-ff-skip-boring-files t
;;           helm-move-to-line-cycle-in-source t
;;           helm-ff-search-library-in-sexp t
;;           helm-scroll-amount 8
;;           helm-ff-file-name-history-use-recentf t)
;;     (setq helm-autoresize-min-height 20
;;           helm-autoresize-max-height 0)
;;     (setq helm-grep-ag-command "rg --smart-case --no-heading --line-number %s %s %s")
;;     (helm-autoresize-mode 1)))

;(use-package helm-swoop
;  :defer t
;  :commands (helm-swoop)
                                        ;  :bind (("C-c /" . helm-swoop)))

(use-package swiper-helm
  :defer t
  :commands (swiper-helm)
  :bind (("C-s" . swiper-helm)))

(provide 'setup-helm)
;;; setup-helm.el ends here
