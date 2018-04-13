;;; setup-jump.el -*- lexical-binding: t; -*-

(use-package dumb-jump
  :commands (dumb-jump-go
             dumb-jump-go-other-window
             dumb-jump-go-prefer-external
             dumb-jump-go-prefer-external-other-window
             dumb-jump-go-prompt
             dumb-jump-quick-look
             dumb-jump-back)
  :init
  (after! hydra
    (defhydra api@dumb-jump (:color blue :columns 3)
      "Dumb Jump"
      ("j" dumb-jump-go "Go")
      ("o" dumb-jump-go-other-window "Other window")
      ("e" dumb-jump-go-prefer-external "Go external")
      ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
      ("i" dumb-jump-go-prompt "Prompt")
      ("l" dumb-jump-quick-look "Quick look")
      ("b" dumb-jump-back "Back")))
  :config
  (setq dumb-jump-selector 'helm))

(use-package avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config
  (setq avy-all-windows nil
        avy-background t))

(use-package ace-jump-helm-line
  :defer t
  :after helm
  :init
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-'") 'ace-jump-helm-line))

  ;; or if using key-chord-mode
  ;; (eval-after-load "helm"
  ;;  '(key-chord-define helm-map "jj" 'ace-jump-helm-line))
  ;;(setq ace-jump-helm-line-style 'pre)
  ;;(setq ace-jump-helm-line-background t)
  (setq ace-jump-helm-line-default-action 'select)
  (setq ace-jump-helm-line-select-key ?e) ;; this line is not needed
  ;; Set the move-only and persistent keys
  (setq ace-jump-helm-line-move-only-key ?o)
  (setq ace-jump-helm-line-persistent-key ?p)
  ;; enable hints preview
  ;;(ace-jump-helm-line-autoshow-mode +1)
  ;; use `linum-mode' to show
  (setq ace-jump-helm-line-autoshow-mode-use-linum t))

(provide 'setup-jump)
