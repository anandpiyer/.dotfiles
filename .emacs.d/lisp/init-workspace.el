;;; init-workspace.el --- Workspaces -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Save and retrieve window layouts, a simpler alternative to registers.
(use-package eyebrowse
  :init
  (add-to-list 'window-persistent-parameters '(window-side . writable))
  (add-to-list 'window-persistent-parameters '(window-slot . writable))
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-mode-line-left-delimiter " "
        eyebrowse-mode-line-right-delimiter " "
        eyebrowse-wrap-around t
        eyebrowse-new-workspace t)
  (eyebrowse-mode))

(provide 'init-workspace)
;;; init-workspace.el ends here
