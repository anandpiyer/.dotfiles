;;; setup-spellcheck.el -*- lexical-binding: t; -*-

(use-package flyspell
  :ensure nil ;; in-built
  :commands flyspell-mode
  :config
  (setq ;ispell-program-name (executable-find "aspell")
   ispell-program-name "/usr/local/bin/aspell"
        ispell-list-command "--list"
        ispell-extr-args '("--dont-tex-check-comments")))

(use-package flyspell-correct
  :requires (flyspell-correct-helm flyspell-correct-popup)
  :after (flyspell helm)
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :config
  (require 'flyspell-correct-helm)
  (require 'flyspell-correct-popup)
  (setq flyspell-popup-correct-delay 0.8))

(provide 'setup-spellcheck)
