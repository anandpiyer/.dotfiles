;;; setup-treemacs.el --- treemacs related setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package treemacs
  :defer t
  :commands (treemacs-toggle
             treemacs-projectile-toggle)
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (use-package treemacs-evil
    :ensure t
    :demand t)
  (setq treemacs-change-root-without-asking nil
        treemacs-collapse-dirs              (if (executable-find "python") 3 0)
        treemacs-file-event-delay           5000
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-never-persist              nil
        treemacs-no-png-images              nil
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      35)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package treemacs-projectile
    :after projectile
    :config
    (setq treemacs-header-function #'treemacs-projectile-create-header)))

(use-package dired-sidebar
  :disabled
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-width 32)
  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :commands (all-the-icons-dired-mode)))

(provide 'setup-treemacs)
;;; setup-treemacs ends here
