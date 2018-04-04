;;; setup-yasnippet.el --- yanippet related setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-minor-mode-on yas-expand yas-expand-snippet
             yas-lookup-snippet yas-insert-snippet yas-new-snippet
             yas-visit-snippet-file snippet-mode)
  :defer t
  :preface (defvar yas-minor-mode-map (make-sparse-keymap))
  :init
  (add-hook 'yas-minor-mode-hook #'yas-reload-all)
  (add-hook 'text-mode-hook #'yas-minor-mode-on)
  (add-hook 'prog-mode-hook #'yas-minor-mode-on)
  (add-hook 'latex-mode-hook #'yas-minor-mode-on)
  (add-hook 'snippet-mode-hook #'yas-minor-mode-on)
  (add-hook 'org-mode-hook #'yas-minor-mode-on)

  :config
  (setq yas-verbosity 0
        yas-also-auto-indent-first-line t
        yas-prompt-functions (delq 'yas-dropdown-prompt yas-prompt-functions)
        yas-triggers-in-field t)

  ;; fix an error caused by smartparens interfering with yasnippet bindings
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)

  ;; Exit snippets on ESC from normal mode
  (add-hook '+evil-esc-hook #'yas-exit-all-snippets))

(use-package yasnippet-snippets :defer t)
  
(provide 'setup-yasnippet)
;;; setup-yasnippet ends here
