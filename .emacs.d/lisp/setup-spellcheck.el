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
  :defer t
  :commands (flyspell-mode
             turn-on-flyspell)
  :init
  (dolist (hook '(prog-mode-hook
                  latex-mode-hook
                  text-mode-hook))
    (add-hook hook #'turn-on-flyspell))
  :bind (:map flyspell-mouse-map
              ("RET" . flyspell-correct-word)
              ([mouse-1] . flyspell-correct-word))
  :config
  (setq flyspell-sort-corrections nil
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

;;------------------------------------------------------------------------------
;; `flyspell-correct':
;;------------------------------------------------------------------------------
(use-package flyspell-correct
  :disabled
  :after (flyspell avy)
  :commands (flyspell-correct-next
             flyspell-correct-previous
             flyspell-correct-wrapper
             flyspell-correct-at-point))

(use-package flyspell-correct-popup
  :disabled
  :defer t
  :after flyspell-correct)
  ;:commands (flyspell-correct-word-generic
  ;           flyspell-correct-previous-word-generic))

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
