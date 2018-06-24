;;; init-mac.el --- Mac specific settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Taken from doom-emacs.

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mac-command-modifier 'super
      ;;mac-right-control-modifier 'hyper
      mac-function-modifier 'hyper  ; make Fn key do Hyper
      mac-option-modifier 'hyper ; make option do Hyper

      ;; clipboard
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      ;; Use a shared clipboard
      select-enable-clipboard t
      select-enable-primary t

      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil
      mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling

      ;; NOTE Meaningless to railwaycat's emacs-mac build
      ns-use-native-fullscreen nil
      ;; Don't open files from the workspace in a new frame
      ns-pop-up-frames nil)

;; https://www.reddit.com/r/emacs/comments/8ph0hq/i_have_converted_from_the_mac_port_to_the_ns_port/
;; Will at least display native Unicode emojis if the multicolor font
;; patch is applied
(set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
(setq frame-title-format "%b"
      ns-use-mwheel-momentum t
      ns-use-mwheel-acceleration t
      ;;pixel-scroll-mode t
      mouse-wheel-flip-direction t
      mouse-wheel-tilt-scroll t
      ;; MacPorts emacs-app port bug
      ;;x-colors (ns-list-colors)
      )

(after! evil
  ;; stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore))

;;------------------------------------------------------------------------------
;; `exec-path-from-shell':
;;------------------------------------------------------------------------------
(use-package exec-path-from-shell
  ;;:disabled
  :if (display-graphic-p)
  :init
  (when (require 'exec-path-from-shell nil t)
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
    (exec-path-from-shell-initialize)))

(provide 'init-mac)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mac.el ends here
