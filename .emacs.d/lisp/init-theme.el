;; init-theme.el --- Theme related settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Defaults & helpers.
;;------------------------------------------------------------------------------
(setq custom-safe-themes t)

(defvar api-theme-hooks nil
  "((theme-id . function) ...).")

(defvar before-load-theme-hook nil
  "Hooks to run before `load-theme'.")

(defvar after-load-theme-hook nil
  "Hooks to run after `load-theme'.")

(defun api/disable-all-themes ()
  "Disable any custom themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun api|add-theme-hook (theme-id hook-func)
  "Associate `THEME-ID' with `HOOK-FUNC'."
  (add-to-list 'api-theme-hooks (cons theme-id hook-func)))

(defun api*load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `api|add-theme-hook'."
  (unless no-enable
    (api/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id api-theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'api*load-theme-advice)

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defadvice load-theme (before run-before-load-theme-hook activate)
  "Disable any custom themes, and run `before-load-theme-hook'."
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'before-load-theme-hook))

;;------------------------------------------------------------------------------
;; `circadian': Automatically change theme on sunrise and sunset.
;;------------------------------------------------------------------------------
(use-package circadian
  :commands (circadian-setup)
  :init
  (add-hook 'emacs-startup-hook #'circadian-setup)
  :config
  (setq calendar-latitude 37.87
        calendar-longitude -122.27
        calendar-location-name "Berkeley, CA")
  (setq circadian-themes '((:sunrise . solarized-light)
                           (:sunset  . zenburn))))

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

  ;;(load-theme 'seoul256 t)
  )

  ;;(add-hook 'emacs-startup-hook (lambda ()
;;                                (load-theme 'seoul256 t))))

;;------------------------------------------------------------------------------
;; `solarized-theme': https://github.com/bbatsov/solarized-emacs
;;------------------------------------------------------------------------------
(use-package solarized-theme
  ;;:disabled
  :init
  (setq solarized-use-less-bold t
        solarized-use-more-italic t

        ;; make the fringe stand out from the background
        ;; solarized-distinct-fringe-background t

        ;; Don't change the font for some headings and titles
        solarized-use-variable-pitch nil

        ;; make the modeline high contrast
        ;; solarized-high-contrast-mode-line t
        x-underline-at-descent-line t

        ;; Use less colors for indicators such as git:gutter, flycheck and similar
        ;; solarized-emphasize-indicators nil

        ;; Don't change size of org-mode headlines (but keep other size-changes)
        ;; solarized-scale-org-headlines nil

        ;; Avoid all font-size changes
        solarized-distinct-doc-face t
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0))

;;------------------------------------------------------------------------------
;; zenburn-theme:
;;------------------------------------------------------------------------------
(use-package zenburn-theme
  ;;:disabled
  :init

   (defun api|customize-zenburn ()
     "Customize `zenburn'."
     (custom-theme-set-faces
      'zenburn

      ;;'(region ((t (:background "#007475"))))
      '(font-lock-comment-delimiter-face ((t (:foreground "gray55"))))
      '(font-lock-comment-face ((t (:foreground "gray55"))))
      '(font-lock-doc-face ((t (:foreground "gray70"))))
      '(shm-current-face ((t (:background "gray27"))))
      '(linum ((t (:foreground "gray37"))))
      '(fringe ((t (:background "#3f3f3f"))))

      `(isearch ((t (:background "#385f38" :foreground "#f8f893"))))
      `(lazy-highlight ((t (:foreground "#ffffe0" :background "#284f28"))))

      ;; column-enforce-mode
      '(column-enforce-face ((t (:foreground "#DC8CC3"))))

      ;; eyebrowse
      `(eyebrowse-mode-line-active ((t (:foreground "#F0DFAF"))))
      `(eyebrowse-mode-line-inactive ((t (:foreground "gray37"))))

      `(ivy-current-match ((t (:background "#4F4F4F"))))
      ;;`(ivy-current-match ((t (:inherit lazy-highlight))))
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

   (api|add-theme-hook 'zenburn #'api|customize-zenburn)
   ;;(add-hook 'after-load-theme-hook #'api|customize-zenburn)
   (load-theme 'zenburn t)
)

(provide 'init-theme)
;;; init-theme.el ends here
