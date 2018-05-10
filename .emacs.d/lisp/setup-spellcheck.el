;;; setup-spellcheck.el --- Spell check -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; `ispell':
;;------------------------------------------------------------------------------
(use-package ispell
  :ensure nil ; in-built
  :init
  (setq ispell-program-name (executable-find "aspell")
        ispell-extra-args '("--sug-mode=ultra"
                            "--lang=en_US")))

;;------------------------------------------------------------------------------
;; `flyspell':
;;------------------------------------------------------------------------------
(use-package flyspell
  :ensure nil ; in-built
  :commands (flyspell-mode
             turn-on-flyspell)
  :init
  (dolist (hook '(prog-mode-hook
                  latex-mode-hook
                  text-mode-hook))
    (add-hook hook #'turn-on-flyspell))
  :config
  (setq flyspell-sort-corrections nil
        flyspell-issue-message-flag nil))

;;------------------------------------------------------------------------------
;; `flyspell-correct':
;;------------------------------------------------------------------------------
(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic))

;; (use-package flyspell-correct
;;   :requires (flyspell-correct-helm flyspell-correct-popup)
;;   :after (flyspell helm)
;;   :commands (flyspell-correct-word-generic
;;              flyspell-correct-previous-word-generic)
;;   :config
;;   (require 'flyspell-correct-helm)
;;   (require 'flyspell-correct-popup)
;;   (setq flyspell-popup-correct-delay 0.8))

(provide 'setup-spellcheck)
;;; setup-spellcheck.el ends here
