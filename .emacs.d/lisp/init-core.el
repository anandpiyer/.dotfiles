;;; core.el --- Core Emacs stuff -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

;; save sessions
(use-package desktop
  :ensure nil
  :init
  (progn
    (desktop-save-mode 1)
    (setq desktop-auto-save-timeout 60
          desktop-globals-to-save
          (append '((file-name-history . 100)
                    (compile-history . 100)
                    (command-history . 100)
                    (extended-command-history . 100)
                    (shell-command-history . 100)
                    (query-replace-history . 100)
                    (regexp-history . 100)
                    (grep-history . 100)
                    (minibuffer-history . 100))
                  desktop-globals-to-save))))

(provide 'init-core)

;;; init-core.el ends here
