;;; setup-treemacs.el --- treemacs related setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-position                   'left
          treemacs-is-never-other-window      nil
          treemacs-silent-refresh             nil
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-collapse-dirs              3)))

(use-package treemacs-evil
  :after evil
  :demand t)

(use-package treemacs-projectile
  :defer t
  :after projectile
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(provide 'setup-treemacs)
;;; setup-treemacs ends here
