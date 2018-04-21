;; init-theme.el --- Theme related settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Defaults
;;------------------------------------------------------------------------------
(setq custom-safe-themes t)

(defvar before-load-theme-hook nil
  "Hooks to run before `load-theme'.")

(defvar after-load-theme-hook nil
  "Hooks to run after `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defadvice load-theme (before run-before-load-theme-hook activate)
  "Disable any custom themes, and run `before-load-theme-hook'."
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'before-load-theme-hook))

;;------------------------------------------------------------------------------
;; `anti-zenburn':
;;------------------------------------------------------------------------------
(use-package anti-zenburn-theme
  :disabled)

;;------------------------------------------------------------------------------
;; `seoul256':
;;------------------------------------------------------------------------------
(use-package seoul256-theme
  ;;:disabled
  :ensure nil
  :init
  (add-to-list 'custom-theme-load-path "~/Code/seoul256-emacs")
  (setq seoul256-background 236
        seoul256-alternate-background 253
        seoul256-override-colors-alist
        '((65 . "#a6a6a6")))

  (load-theme 'seoul256 t))

  ;;(add-hook 'emacs-startup-hook (lambda ()
;;                                (load-theme 'seoul256 t))))

;;------------------------------------------------------------------------------
;; `solarized-theme': https://github.com/bbatsov/solarized-emacs
;;------------------------------------------------------------------------------
(use-package solarized-theme
  :disabled
  :init
  (defun api|customize-solarized ()
    "Customizations for solarized."
    ;; make the fringe stand out from the background
    (setq solarized-distinct-fringe-background t)

    ;; make the modeline high contrast
    (setq solarized-high-contrast-mode-line t)
    (setq x-underline-at-descent-line t)

    ;; Use less bolding
    (setq solarized-use-less-bold t)

    ;; Use more italics
    (setq solarized-use-more-italic t))

  (add-hook 'before-load-theme-hook #'api|customize-solarized)
  (load-theme 'solarized-dark t))

;;------------------------------------------------------------------------------
;; zenburn-theme:
;;------------------------------------------------------------------------------
(use-package zenburn-theme
  :disabled
  :init

   (defun api|customize-zenburn ()
     "Customize `zenburn'."
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

      `(ivy-current-match ((t (:background "#4F4F4F"))))
      `(ivy-minibuffer-match-face-1  ((t (:inherit match))))
      `(ivy-minibuffer-match-face-2  ((t (:inherit match))))
      `(ivy-minibuffer-match-face-3  ((t (:inherit match))))
      `(ivy-minibuffer-match-face-4  ((t (:inherit match))))

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
                                               :strike-through t))))))

   (add-hook 'after-load-theme-hook #'api|customize-zenburn)
   (load-theme 'zenburn t))

(provide 'init-theme)
;;; init-theme.el ends here
