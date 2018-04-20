;;; setup-flycheck.el --- Flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :diminish (flycheck-mode . " ⓢ")
  :commands (flycheck-mode
             flycheck-list-errors
             flycheck-buffer)
  :init
   ;; Turn on `flycheck-mode' for programming modes.
  (add-hook 'prog-mode-hook #'flycheck-mode)

  ;; Check the buffer when  `ESC' is pressed.
  (defun api|flycheck-buffer ()
  (when flycheck-mode
    (ignore-errors (flycheck-buffer))
    nil))

  (after! evil
    (add-hook '+evil-esc-hook #'api|flycheck-buffer t))

  :config
  (setq flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save idle mode-enabled))

  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    "...X...."
    "..XX...."
    ".XXX...."
    "XXXX...."
    ".XXX...."
    "..XX...."
    "...X...."))

(provide 'setup-flycheck)

;;; setup-flycheck.el ends here
