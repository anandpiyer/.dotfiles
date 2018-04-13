;;; init-mac.el --- Mac specific settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Taken from doom-emacs.
;;; Code:

;;------------------------------------------------------------------------------
;; Defaults.
;;------------------------------------------------------------------------------
(setq mac-command-modifier 'super
      mac-right-control-modifier 'hyper

      ;; clipboard
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      ;; Use a shared clipboard
      select-enable-clipboard t
      select-enable-primary t

      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil
      mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
      ;; Curse Lion and its sudden but inevitable fullscreen mode!
      ;; NOTE Meaningless to railwaycat's emacs-mac build
      ns-use-native-fullscreen nil
      ;; Don't open files from the workspace in a new frame
      ns-pop-up-frames nil)

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
  :config
  (cond ((display-graphic-p)
         ;; A known problem with GUI Emacs on MacOS: it runs in an isolated
         ;; environment, so envvars will be wrong. That includes the PATH
         ;; Emacs picks up. `exec-path-from-shell' fixes this. This is slow
         ;; and benefits greatly from compilation.
         (setq exec-path
               (or (eval-when-compile
                     (when (require 'exec-path-from-shell nil t)
                       (setq exec-path-from-shell-check-startup-files nil
                             exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
                       (nconc exec-path-from-shell-variables '("GOPATH" "GOROOT" "PYTHONPATH"))
                       (exec-path-from-shell-initialize)
                       exec-path))
                   exec-path)))
        (t
         (when (require 'osx-clipboard nil t)
           (osx-clipboard-mode +1)))))

(provide 'init-mac)
;;; init-mac.el ends here
