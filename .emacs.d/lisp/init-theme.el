;; init-theme.el --- Theme related settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

;;------------------------------------------------------------------------------
;; Allow clean loading of themes by not propagating leftovers.
;;------------------------------------------------------------------------------
(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;;------------------------------------------------------------------------------
;; seoul256:
;;------------------------------------------------------------------------------
(use-package seoul256-theme
  ;;:disabled
  :ensure nil
  :load-path "~/Code/seoul256-emacs"
  :config
  (setq seoul256-background 236
        seoul256-alternate-background 253)
  (load-theme 'seoul256 t)
  (after! seoul256-theme
    (custom-theme-set-faces
     'seoul256
     '(font-lock-comment-delimiter-face ((t (:foreground "gray45"))))
     '(font-lock-comment-face ((t (:foreground "gray45"))))
     '(font-lock-doc-face ((t (:foreground "gray70")))))))

;;------------------------------------------------------------------------------
;; zenburn-theme:
;;------------------------------------------------------------------------------
(use-package zenburn-theme
  :disabled
  :config
   (load-theme 'zenburn t)

   (after! zenburn-theme
     (custom-theme-set-faces
      'zenburn

      '(region ((t (:background "#007475"))))
      '(font-lock-comment-delimiter-face ((t (:foreground "gray55"))))
      '(font-lock-comment-face ((t (:foreground "gray55"))))
      '(font-lock-doc-face ((t (:foreground "gray70"))))
      '(shm-current-face ((t (:background "gray27"))))
      '(linum ((t (:foreground "gray37"))))
      '(fringe ((t (:background "#3f3f3f"))))

      ;; column-enforce-mode
      '(column-enforce-face ((t (:foreground "#DC8CC3"))))

      ;; eyebrowse
      `(eyebrowse-mode-line-active ((t (:foreground "#F0DFAF"))))
      `(eyebrowse-mode-line-inactive ((t (:foreground "gray37"))))

      ;; ace-window
      `(aw-leading-char-face ((t (:foreground "#F0DFAF"
                                              :weight bold
                                              :height 1.0))))

      `(winum-face ((t (:foreground "#F0DFAF" :height 1.3))))

      ;; solaire-mode
      `(solaire-default-face  ((t (:background "#383838"))))
      `(solaire-hl-line-face ((t (:background "#2B2B2B"))))

      ;; strike through unmatched parenthesis
      '(rainbow-delimiters-unmatched-face ((t (:foreground "red"
                                               :inherit unspecified
                                               :strike-through t)))))))

(provide 'init-theme)
;;; init-theme.el ends here
