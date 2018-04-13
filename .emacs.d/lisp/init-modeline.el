;;; init-modeline.el --- Modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Custom faces

(defgroup +api-modeline nil
  ""
  :group 'api)

(defface api-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group '+api-modeline)

(defface api-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group '+api-modeline)

(defface api-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+api-modeline)

(defface api-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group '+api-modeline)

(defface api-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group '+api-modeline)

(defface api-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `+api-modeline--anzu', `+api-modeline--evil-substitute' and
`iedit'"
  :group '+api-modeline)

(defface api-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+api-modeline)

(defface api-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+api-modeline)

(defface api-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+api-modeline)

(defsubst api--window-active ()
  (powerline-selected-window-active))

;; Macros
(defmacro def-modeline-segment! (name &rest forms)
  "Defines a modeline segment `NAME' with body `FORMS' and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "api-modeline-segment--%s" name))))
    `(progn
       (defun ,sym () ,@forms)
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

;; Segments
(defun api--mode-icon ()
  "Major mode icon, if available."
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (propertize (format "%s" icon)
                  'help-echo (format "Major-mode: `%s`" major-mode)
                  'display '(raise -0.15)
                  'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer) :inherit)))))

(defun api--mode-icon-o ()
  "An `all-the-icons' segment indicating the current buffer's mode with an icon"
  (let ((icon (all-the-icons-icon-for-mode major-mode)))
    (unless (symbolp icon)
      (propertize icon
                  'help-echo (format "Major-mode: `%s'" major-mode)
                  'display '(raise 0)
                  'face `(:height 1.1
                          :family ,(all-the-icons-icon-family-for-mode major-mode)
                          :inherit)))))

(defun powerline-modeline-vc ()
  "Show vc."
  (when vc-mode
    (let* ((text-props (text-properties-at 1 vc-mode))
           (vc-without-props (substring-no-properties vc-mode))
           (new-text (concat
                      " "
                      (all-the-icons-faicon "code-fork"
                                            :v-adjust -0.1)
                      vc-without-props
                      " "))
           )
      (apply 'propertize
             new-text
             'face (when (powerline-selected-window-active) 'success)
             text-props
             ))))

(defun api--vc ()
  "Displays the current branch, colored based on its state."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (api--window-active))
            (all-the-icons-default-adjust -0.1))
        (concat "  "
                (cond ((memq state '(edited added))
                       (if active (setq face 'success))
                       (all-the-icons-octicon
                        "git-compare"
                        :face face
                        :v-adjust -0.05))
                      ((eq state 'needs-merge)
                       (if active (setq face 'success))
                       (all-the-icons-octicon "git-merge" :face face))
                      ((eq state 'needs-update)
                       (if active (setq face 'warning))
                       (all-the-icons-octicon "arrow-down" :face face))
                      ((memq state '(removed conflict unregistered))
                       (if active (setq face 'error))
                       (all-the-icons-octicon "alert" :face face))
                      (t
                       (if active (setq face 'font-lock-doc-face))
                       (all-the-icons-faicon
                        "code-fork"
                        :face face
                        :v-adjust -0.05)))
                " "
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face))
                " ")))))

 (defun api--window-number ()
   (when (and (bound-and-true-p winum-mode)
              (> (length (window-list)) 1))
        (propertize (format "%c" (+ 10121 (winum-get-number)))
                    ;;'face `winum-face)))
                    'face `(:height 1.3 :inherit))))

(defun api--buffer-info ()
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat (cond (buffer-read-only
                 (concat (all-the-icons-octicon
                          "lock"
                          :face 'warning
                          :v-adjust -0.05)
                         " "))
                ((buffer-modified-p)
                 (concat (all-the-icons-faicon
                          "floppy-o"
                          :face 'error
                          :v-adjust -0.0575)
                         " "))
                ((and buffer-file-name
                      (not (file-exists-p buffer-file-name)))
                 (concat (all-the-icons-octicon
                          "circle-slash"
                          :face 'error
                          :v-adjust -0.05)
                         " "))
                ((buffer-narrowed-p)
                 (concat (all-the-icons-octicon
                          "fold"
                          :face 'warning
                          :v-adjust -0.05)
                         " ")))
          (if buffer-file-name
              (shrink-path-file (buffer-file-name))
            "%b")))

(defun powerline-buffer-info ()
    (let ((proj (projectile-project-name)))
      (if (string= proj "-")
          (buffer-name)
        (concat
         (propertize (concat
                      proj)
                     'face 'warning)
         " "
         (buffer-name)))))

(defun api--ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat (if vc-mode " " "  ")
          (when icon
            (concat
             (all-the-icons-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text (propertize " " 'face 'variable-pitch))))
          (when text
            (propertize text 'face face))
          (if vc-mode "  " " ")))

(defun api--flycheck ()
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      ('finished (if flycheck-current-errors
                     (let-alist (flycheck-count-errors flycheck-current-errors)
                       (let ((sum (+ (or .error 0) (or .warning 0))))
                         (api--ml-icon "do_not_disturb_alt"
                                        (number-to-string sum)
                                        (if .error 'api-modeline-urgent 'api-modeline-warning)
                                        -0.25)))
                   (api--ml-icon "check" nil 'api-modeline-info)))
      ('running     (api--ml-icon "access_time" nil 'font-lock-doc-face -0.25))
      ('no-checker  (api--ml-icon "sim_card_alert" "-" 'font-lock-doc-face))
      ('errored     (api--ml-icon "sim_card_alert" "Error" 'api-modeline-urgent))
      ('interrupted (api--ml-icon "pause" "Interrupted" 'font-lock-doc-face)))))

(use-package shrink-path
  :commands (shrink-path-file))

(use-package evil-anzu
  :after evil
  :init (global-anzu-mode t)
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-search-threshold 999))

(use-package smart-mode-line
  :disabled
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'nil)
  (add-hook 'after-init-hook #'sml/setup))

;; Telephone line.
(use-package telephone-line
  :disabled
  :after evil
  ;:ensure nil
  ;:load-path "/Users/api/Code/telephone-line"
  :init

  ;;(setq telephone-line-height (truncate (* 2.1 (frame-char-height))))
  (setq telephone-line-height 29)

  ;; Bunch of mode line segments. Most of them taken from:
  ;; https://github.com/domtronn/all-the-icons.el/wiki/Spaceline

  (telephone-line-defsegment api--workspace-segment ()
  (when (and
             (bound-and-true-p persp-mode)
             ;; There are multiple implementations of
             ;; persp-mode with different APIs
             (fboundp 'safe-persp-name)
             (fboundp 'get-frame-persp))
    (let ((name (safe-persp-name (get-frame-persp))))
      (concat " "
              (all-the-icons-octicon "versions" :v-adjust -0.05)
              " "
      (propertize
       (if (file-directory-p name)
           (file-name-nondirectory (directory-file-name name))
         name)
       'face 'bold)))))

  (defun api--github-vc ()
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (all-the-icons-alltheicon "git")
                   'face '(:height 1.1 :inherit)
                   'display '(raise 0.1))
       ;; (propertize " · ")
       ;;  (propertize (format "%s" (all-the-icons-octicon "git-merge"))
       ;;              'face `(:family ,(all-the-icons-octicon-family)
       ;;                              :height 1.1 :inherit)
       ;;                              'display '(raise 0.1))
       (propertize (format " %s" branch)
                   'face `(:inherit)
                   'display '(raise 0.0)))))

  ;; Shows version control and branch.
  (telephone-line-defsegment api--vc-segment ()
    (when vc-mode
      (cond ((string-match "Git[:-]" vc-mode) (api--github-vc))
            (t (propertize (format "%s" vc-mode))))))

  ;; Shows all-the-icon icon for the current major mode.
  (telephone-line-defsegment api--mode-icon-segment ()
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
        (propertize (format "%s" icon)
                    'help-echo (format "Major-mode: `%s`" major-mode)
                    'display '(raise -0.15)
                    'face `(:height 1.2
                            :family ,(all-the-icons-icon-family-for-buffer)
                            )))))

  ;; Shows the current buffer id. If projectile-mode is active, shows the
  ;; relative path to the file from the project root.
  (telephone-line-defsegment api--buffer-id-segment ()
    (if (fboundp 'projectile-project-root)
        (let* ((buf (or (buffer-file-name) (buffer-name)))
               (proj (ignore-errors (projectile-project-root)) )
               (name (if (buffer-file-name)
                         (or (cadr (split-string buf proj))
                             (format-mode-line "%b"))
                       (format-mode-line "%b"))))
          (propertize (format "%s" name)
                      'face `(:inherit)
                      'display '(raise 0.0)
                      'help-echo (format "Major-mode: `%s`" major-mode)))
      (propertize (format-mode-line "%b ")
                  'face '(:inherit)
                  'display '(raise 0.0))))

  ;; Shows status of the current buffer using all-the-icons.
  (telephone-line-defsegment api--buffer-modified-segment ()
    (let* ((config-alist
            '(("*" all-the-icons-faicon-family all-the-icons-faicon
               "chain-broken" :height 1.2 :v-adjust -0.0)
              ("-" all-the-icons-faicon-family all-the-icons-faicon
               "link" :height 1.2 :v-adjust -0.0)
              ("%" all-the-icons-octicon-family all-the-icons-octicon
               "lock" :height 1.2 :v-adjust 0.1)))
           (result (cdr (assoc (format-mode-line "%*") config-alist))))

      (propertize (format "%s" (apply (cadr result) (cddr result)))
                  'face `(:family ,(funcall (car result)) :inherit ))))

  ;; Shows the buffer size.
  (telephone-line-defsegment api--buffersize-segment ()
    (propertize (format-mode-line " %I ")
                'face `(:height 1.0 :inherit)
                'display '(raise 0.0)))

  ;; Shows a bunch of buffer related things.
  (telephone-line-defsegment api--buffer-segment ()
    `(""
      mode-line-mule-info
      mode-line-client
      mode-line-remote
      mode-line-frame-identification
      ,(telephone-line-raw (format-mode-line " %I "))
      ,(telephone-line-raw mode-line-buffer-identification t)))

  ;; Shows the current pointer position in the buffer.
  (telephone-line-defsegment api--position-segment ()
    (if (telephone-line-selected-window-active)
        (if (eq major-mode 'paradox-menu-mode)
            (telephone-line-trim (format-mode-line mode-line-front-space))
          '("%3l:%2c %p"))))

  (telephone-line-defsegment* api--spacer-segment ()
    " ")

  ;; (setq telephone-line-lhs
  ;;       '((evil   . (telephone-line-window-number-segment
  ;;                    api--spacer-segment
  ;;                    api--persp-name-segment))
  ;;         (accent . (api--vc-segment
  ;;                    telephone-line-process-segment))
  ;;         (nil    . (telephone-line-projectile-segment
  ;;                    api--spacer-segment
  ;;                    api--buffer-segment
  ;;                    api--spacer-segment
  ;;                    api--buffer-modified-segment))))

  (setq telephone-line-lhs
        '((evil   . (telephone-line-window-number-segment))
          (accent . (api--workspace-segment))
          (nil    . (
                     api--spacer-segment
                     api--vc-segment
                     telephone-line-process-segment
                     telephone-line-projectile-segment
                     api--spacer-segment
                     api--buffer-segment
                     api--spacer-segment
                     api--buffer-modified-segment))))

  (setq telephone-line-rhs
        '(;(nil    . (telephone-line-misc-info-segment))
          (nil . (api--mode-icon-segment
                     api--spacer-segment
                     telephone-line-major-mode-segment))
          (evil   . (api--spacer-segment
                     api--position-segment
                     api--spacer-segment))))

  (setq telephone-line-primary-left-separator 'telephone-line-nil
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-nil)

  (telephone-line-mode t))

;; Powerline
;; https://github.com/MaxSt/dotfiles/blob/master/emacs.d/config.org#powerline
(use-package powerline
  :disabled
  :config
  (add-hook 'desktop-after-read-hook 'powerline-reset)

  (defun make-rect (color height width)
    "Create an XPM bitmap."
    (when window-system
      (propertize
       " " 'display
       (let ((data nil)
             (i 0))
         (setq data (make-list height (make-list width 1)))
         (pl/make-xpm "percent" color color (reverse data))))))

  (defun powerline-ace-window () (propertize (or (window-parameter (selected-window) 'my-ace-window-path) "") 'face 'error))

   (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (modified (buffer-modified-p))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active1 'powerline-inactive1))
                          (bar-color (cond ((and active modified) (face-foreground 'error))
                                           (active (face-background 'cursor))
                                           (t (face-background 'tooltip))))
                          (lhs (list
                                (make-rect bar-color 29 3)
                                ;; (when modified
                                ;;   (concat
                                ;;    " "
                                ;;    (all-the-icons-faicon "floppy-o"
                                ;;                          :face (when active 'error)
                                ;;                          :v-adjust -0.01)))
                                ;; " "
                                ;;(powerline-buffer-info)
                                " "
                                (api--buffer-info)
                                " "
                                (api--vc)
                                ))
                          (center (list
                                   " "
                                   (api--mode-icon)
                                   " "
                                   ;;major-mode
                                   (powerline-major-mode)
                                   " "))
                          (rhs (list
                                (api--window-number)
                                " | "
                                (format "%s" (eyebrowse--get 'current-slot))
                                " | "
                                (powerline-raw "%l:%c" face1 'r)
                                " | "
                                (powerline-raw "%6p" face1 'r)
                                ;;(powerline-hud 'highlight 'region 1)
                                " "
                                (api--flycheck)
                                "  "
                                ))
                          )
                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                      (powerline-render center)
                      (powerline-fill face2 (powerline-width rhs))
                      (powerline-render rhs))))))
  )

;;(use-package spaceline)

;;(use-package spaceline-all-the-icons
(use-package spaceline
  ;;:after spaceline
  ;;:disabled
  :config
  (require 'spaceline-segments)
  ;;(require 'spaceline-all-the-icons-segments)

  (setq ns-use-srgb-colorspace nil)
  (setq spaceline-window-numbers-unicode 'nil
        powerline-height 29
        powerline-default-separator 'zigzag)

  ;;(setq spaceline-all-the-icons-icon-set-window-numbering 'square)

  (spaceline-define-segment mode-icon
    (api--mode-icon))

  (spaceline-define-segment window-number
    (api--window-number))

  (spaceline-define-segment buffer-info
    (api--buffer-info))

  (spaceline-define-segment vc
    (api--vc))

  (spaceline-define-segment flycheck
    (api--flycheck))

  (spaceline-compile
    `(
      ((anzu selection-info) :face highlight)

      (window-number :face highlight-face)

      (;;projectile-root
       buffer-info)
      " "
      ;;((buffer-size line-column buffer-position))
      vc
      )

    `(((;;vc
        mode-icon
        buffer-size
        line-column
        buffer-position
       ) :separator " ")
      (flycheck-error flycheck-info)
      ""
      ))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
  (spaceline-helm-mode +1)

 )

(provide 'init-modeline)
;;; init-modeline.el ends here
