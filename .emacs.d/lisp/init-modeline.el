;;; init-modeline.el --- Modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Defaults.
;;------------------------------------------------------------------------------
(setq mode-line-format nil)

(defconst api--ati-height 1.0)

(defun ati-height (&optional height)
  "Scale `powerline-text-scale-factor' by HEIGHT."
  (if (bound-and-true-p powerline-text-scale-factor)
      (* (or height api--ati-height) (or powerline-text-scale-factor 1))
    (or height api--ati-height)))

(defun api--face-foreground (face)
  "Get the foreground of FACE or `default' face."
  (or (face-foreground face nil t)
      (face-foreground 'default)))

(defun api--face-background (face)
  "Get the background of FACE or `default' face."
  (or (face-background face nil t)
      (face-background 'default)))

;;------------------------------------------------------------------------------
;; Custom segments
;; Sourced from `powerline', `spaceline', `doom' & `spaceline-all-the-icons'.
;;------------------------------------------------------------------------------
(defun api--window-number ()
  "Window number in unicode format."
  (when (and (bound-and-true-p winum-mode)
             (> (length (window-list)) 1))
    (propertize (format "%c" (+ 10121 (winum-get-number)))
                'face `(:height ,(ati-height 1.2) :inherit))))

(defun api--mode-icon ()
  "An `all-the-icons' mode icon based on buffer."
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (propertize icon;(format "%s" icon)
                  'help-echo (format "Major-mode: `%s`" major-mode)
                  'display '(raise -0.1)
                  'face `(:height ,(ati-height 1.1)
                                  :family ,(all-the-icons-icon-family-for-buffer)
                                  :inherit)))))

(defun api--major-mode-icon ()
  "An `all-the-icons' segment indicating the current buffer's mode with an icon."
  (let ((icon (all-the-icons-icon-for-mode major-mode)))
    (unless (symbolp icon)
      (propertize icon
                  'help-echo (format "Major-mode: `%s'" major-mode)
                  'display '(raise 0)
                  'face `(:height ,(ati-height 1.1)
                          :family ,(all-the-icons-icon-family-for-mode major-mode)
                          :inherit)))))

(defun api--vc ()
  "Show an `all-the-icons' icon to indicate vc status, along with branch name."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend))
           (branch  (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))
           (icon
             (cond ((memq state '(edited added))
                    (all-the-icons-octicon "git-compare"))
                   ((eq state 'needs-merge)
                    (all-the-icons-octicon "git-merge"))
                   ((eq state 'needs-update)
                    (all-the-icons-octicon "arrow-down"))
                   ((memq state '(removed conflict unregistered))
                    (all-the-icons-octicon "alert"))
                   (t
                    (all-the-icons-octicon "git-branch")))))
      (propertize
       (concat
        (propertize icon
                    'face `(:family ,(all-the-icons-octicon-family)
                                    :height ,(ati-height)
                                    :inherit)
                    'display '(raise 0.1))
        (propertize (format " %s" branch)
                    'face `(:height ,(ati-height 0.9) :inherit)
                    'display '(raise 0.1))))
      )))

(defun api--mu4e ()
  "Show mu4e number of unread messages."
 (when (featurep 'mu4e-alert)
   mu4e-alert-mode-line))

;;
;; Flycheck
;;
(defun api--flycheck-finished ()
  "Get the string for finished status of Flycheck."
  (let* ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                  (+ (or .warning 0) (or .error 0)))))
    (if flycheck-current-errors (format "✖ %s" count) "✔")))

(defun api--flycheck-status (background)
  "Render the mode line for Flycheck Status in a more verbose fashion."
  (when (boundp 'flycheck-last-status-change)
    (let* ((text (cl-case flycheck-last-status-change
                   (finished    (api--flycheck-finished))
                   (running     (concat (all-the-icons-faicon "refresh") " Running"))
                   (no-checker  "⚠ No Checker")
                   ;;(not-checked "✖ Disabled")
                   (not-checked "")
                   (errored     "⚠ Error")
                   (interrupted "⛔ Interrupted")))
           (face (cond
                  ((string-match "✔" text)
                   `(:height ,(ati-height 0.9)
                             :background ,background
                             :foreground ,(api--face-foreground 'success)))
                  ((string-match "⚠" text)
                   `(:height ,(ati-height 0.9)
                             :background ,background
                             :foreground ,(api--face-foreground 'warning)))
                  ((string-match "✖ [0-9]" text)
                   `(:height ,(ati-height 0.9)
                             :foreground ,(api--face-foreground 'error)
                             :background ,background
                             ))
                  ((string-match "✖ Disabled" text)
                   `(:height ,(ati-height 0.9)
                             :background ,background
                             :foreground ,(api--face-foreground 'font-lock-comment-face)))
                  (t `(:height ,(ati-height 0.9) :inherit)))))

      ;;(propertize text 'face face 'display '(raise 0.1))
      (propertize text 'face face)
      )))

(defun api--buffer-info ()
  "Display buffer id and state."
  (let ((icon (cond (buffer-read-only "lock")
                     ((buffer-modified-p) "unlock")
                     ((and buffer-file-name
                           (not (file-exists-p buffer-file-name))) "ban")
                     ((buffer-narrowed-p) "fold"))))

    (concat
    (if icon (propertize (all-the-icons-faicon icon :v-adjust 0.0)
                         'face `(:family ,(all-the-icons-faicon-family) :height ,(ati-height 1.1) :inherit)))
    " "
    (if buffer-file-name
        ;;(propertize (shrink-path-file (buffer-file-name))
        (propertize (buffer-name)
                    'face `(:inherit)
                    'help-echo (format "%s" buffer-file-name))
      "%b"))))

;;
;; selection information.
;;
(defsubst column-at-pos (pos)
  "Column at POS."
  (save-excursion (goto-char pos)
                  (current-column)))

(defun api--selection-info ()
  "Information about the current selection."
  (when mark-active
    (cl-destructuring-bind (beg . end)
        (if (eq evil-state 'visual)
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))

      (propertize
       (let ((lines (count-lines beg (min (1+ end) (point-max)))))
         (concat (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (eq 'block evil-visual-selection))
                        (let ((cols (abs (- (column-at-pos end)
                                            (column-at-pos beg)))))
                          (format "%dx%dB" lines cols)))
                       ((eq 'line evil-visual-selection)
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- (1+ end) beg) lines))
                       (t
                        (format "%dC" (- (1+ end) beg))))
                   (format " %dW" (count-words beg end))))
       'face 'region))))

;;
;; anzu
;;
(defun api--anzu-update-func (here total)
  "Update function to be set as `anzu-mode-line-update-function'.
Displays HERE and TOTAL to indicate how many search results have been found."
  (let* ((status (cl-case anzu--state
                   (search (format "(%s/%d%s)"
                                   (anzu--format-here-position here total)
                                   total (if anzu--overflow-p "+" "")))
                   (replace (format "(%d/%d)" here total))
                   (replace-query (format "(%d replace)" total))))
         (icon (cl-case anzu--state
                 (search "search")
                 (replace "refresh")
                 (replace-query "find_replace")))
         (anzu-face (if (and (zerop total)
                             (not (string= isearch-string "")))
                        'anzu-mode-line-no-match 'anzu-mode-line))
         (text-face `(:height ,(ati-height 1.1) :inherit ,anzu-face))
         (icon-face `(:height ,(ati-height 1.1) :family ,(all-the-icons-material-family) :inherit ,anzu-face)))

    (concat " "
     (propertize (all-the-icons-material icon) 'face icon-face)
     (propertize status 'face text-face) " ")))

;; ace-window
(defun powerline-ace-window ()
  (propertize (or (window-parameter (selected-window) 'my-ace-window-path) "") 'face 'error))

(defun api--projectile ()
  "Show projectile project name if available."
  (if (fboundp 'projectile-project-name)
      (let ((help-echo "Switch Project")
            (raise 0.0)
            (height 1.0)
            (local-map (make-mode-line-mouse-map 'mouse-1 'projectile-switch-project)))
            (propertize (projectile-project-name)
                        'face `(:height ,(ati-height height) :weight bold :slant italic :inherit)
                        'display `(raise ,raise)
                        'help-echo help-echo
                        'local-map local-map))))

(defun api--music ()
  (if (boundp 'mingus-mode-line-object)
      mingus-mode-line-object))
;;------------------------------------------------------------------------------
;; `evil-anzu': Enables showing evil search status in modeline.
;;------------------------------------------------------------------------------
(use-package evil-anzu
  :after evil
  :delight
  :init (global-anzu-mode t)
  :config
  (setq ;anzu-cons-mode-line-p nil
        anzu-search-threshold 999)
  (setq-default anzu-mode-line-update-function 'api--anzu-update-func))

;;------------------------------------------------------------------------------
;; `powerline':
;; https://github.com/MaxSt/dotfiles/blob/master/emacs.d/config.org#powerline
;;------------------------------------------------------------------------------
(use-package powerline
;;  :disabled
  :init
  (add-hook 'after-init-hook #'api|set-powerline)
  (add-hook 'desktop-after-read-hook 'powerline-reset)
  ;;:config
  ;;(setq ns-use-srgb-colorspace nil)
  (setq powerline-height 20)
  (setq powerline-default-separator 'utf-8)

  (defun api|set-powerline ()
   (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (special (if active 'highlight 'mode-line-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))

                          (lhs (list
                                (powerline-raw (api--window-number) special 'l)
                                (powerline-raw " " special)
                                (funcall separator-left special mode-line)
                                (powerline-raw (api--projectile) mode-line 'l)
                                (powerline-raw " " mode-line)
                                (funcall separator-left mode-line face1)
                                (powerline-raw (api--buffer-info) face1 'l)
                                (if active
                                    (powerline-raw (api--flycheck-status (api--face-background face1)) face1 'l))
                                (powerline-raw " " face1)
                                (funcall separator-left face1 face2)
                                ))
                          ;; (center (list
                          ;;          " "
                          ;;          (api--vc)
                          ;;          " "))
                          (rhs (list
                                (powerline-raw (api--music) face2 'r)
                                (powerline-raw global-mode-string face2 'r)
                                (if mark-active
                                    (funcall separator-right face2 'region))
                                (powerline-raw (api--selection-info) 'region 'r)
                                (if mark-active
                                    (funcall separator-right 'region face1)
                                  (funcall separator-right face2 face1))
                                (powerline-raw " " face1)
                                (if active (powerline-raw (api--vc) face1 'r))
                                (powerline-raw (api--mu4e) face1 'r)
                                (powerline-raw (api--mode-icon) face1 'r)
                                (funcall separator-right face1 mode-line)
                                (powerline-raw " " mode-line 'r)
                                (powerline-buffer-size mode-line 'r)
                                (when powerline-display-mule-info
                                  (powerline-raw mode-line-mule-info mode-line 'r))
                                (powerline-raw "%l:%c" mode-line 'r)
                                (powerline-raw "%6p" mode-line 'r)
                                ))
                          )
                     (concat
                      (powerline-render lhs)
                      ;;(powerline-fill-center 'default (/ (powerline-width center) 2.0))
                      ;;(powerline-render center)
                      (powerline-fill face2 (powerline-width rhs))
                      (powerline-render rhs)))))))
   )

;;------------------------------------------------------------------------------
;; `shrink-path': Fish-shell like directory/file paths.
;;------------------------------------------------------------------------------
(use-package shrink-path
  :commands (shrink-path-file))

;;------------------------------------------------------------------------------
;; `smart-mode-line':
;;------------------------------------------------------------------------------
(use-package smart-mode-line
  :disabled
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'nil)
  (add-hook 'after-init-hook #'sml/setup))

;;------------------------------------------------------------------------------
;; `spaceline': Spacemacs modeline.
;;------------------------------------------------------------------------------
;;(use-package spaceline)

;;(use-package spaceline-all-the-icons
(use-package spaceline
  ;;:after spaceline
  :disabled
  :init
  (defun api|apply-modeline ()
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
  (add-hook 'after-init-hook #'api|apply-modeline)
  ;;:config
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

  ;;(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))



  ;;(after! helm (spaceline-helm-mode +1))

 )

;;------------------------------------------------------------------------------
;; `telephone-line': Powerline alternative.
;;------------------------------------------------------------------------------
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

(provide 'init-modeline)
;;; init-modeline.el ends here
