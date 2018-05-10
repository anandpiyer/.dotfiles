;;; init-keybindings.el --- Key bindings -*- lexical-binding: t; -*-
;;; Commentary:
;; Initialize key bindings
;;; Code:

;;------------------------------------------------------------------------------
;; leader, super, hyper
;;------------------------------------------------------------------------------
(defvar my-leader-key "SPC"
  "Anand's leader prefix key.")

;;; Super (cmd) key bindings
(after! winum
  (global-set-key (kbd "s-0") 'winum-select-window-0-or-10)
  (global-set-key (kbd "s-1") 'winum-select-window-1)
  (global-set-key (kbd "s-2") 'winum-select-window-2)
  (global-set-key (kbd "s-3") 'winum-select-window-3)
  (global-set-key (kbd "s-4") 'winum-select-window-4))

;;; Hyper (right ctrl) key bindings
(global-set-key (kbd "H-h") help-map)

;;------------------------------------------------------------------------------
;; `hydra':
;;------------------------------------------------------------------------------
(use-package hydra
  :after evil
  :init
  (setq lv-use-seperator t)

  (defhydra api@text-zoom (:hint t :color red)
    "
      Text zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("0" (text-scale-set 0) "reset"))

  (defhydra api@window-nav (:hint nil)
    "
Move   : _h_: far left         _j_: very bottom      _k_:very top       _l_:far right      _s_: swap with other
Resize : _+_: increase height  _-_: decrease height  _<_:decrease width _>_:increase width _=_: balance
"
    ("h" evil-window-move-far-left)
    ("j" evil-window-move-very-bottom)
    ("k" evil-window-move-very-top)
    ("l" evil-window-move-far-right)
    ("s" ace-swap-window)

    ("+" evil-window-increase-height)
    ("-" evil-window-decrease-height)
    ("<" evil-window-decrease-width)
    (">" evil-window-increase-width)
    ("=" balance-windows)

    ("q" nil)))

;;------------------------------------------------------------------------------
;; `general': Convenient key definitions.
;;------------------------------------------------------------------------------
(use-package general
  :commands (general-evil-setup)
  :after evil
  :init
  (general-evil-setup t)

  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode)

  (general-define-key :states '(insert visual operator replace)
                      :keymaps 'global
                      "C-g" 'evil-escape)

  (general-define-key :states '(normal visual emacs)
                      :keymaps 'global
                      :prefix my-leader-key

                      "SPC" 'counsel-M-x

                      "b" '(:ignore t :which-key "buffer")
                      "bb" 'ivy-switch-buffer
                      "bd" 'kill-this-buffer
                      "bk" 'kill-this-buffer
                      "bn" 'evil-buffer-new
                      "bh" 'previous-buffer
                      "bl" 'next-buffer

                      "e" 'mu4e

                      "f" '(:ignore t :which-key "file")
                      "ff" 'counsel-find-file
                      "fs" 'save-buffer

                      "g" '(:ignore t :which-key "git")
                      "gs" 'magit-status
                      "gd" 'magit-diff
                      "gc" 'magit-commit
                      "gp" 'magit-push

                      "h" '(:ignore t :which-key "hydras")
                      "hj" '(api@dumb-jump/body :which-key "Dumb Jump")
                      "ht" '(api@text-zoom/body :which-key "Text zoom")
                      "hw" '(api@window-nav/body :which-key "Window navigation")
                      "hc" '(api@multiple-cursors/body :which-key "Multiple cursors")
                      "hg" '(api@git-gutter/body :which-key "Git gutter")

                      "l" '(:ignore t :which-key "window layout")
                      "lc" '(eyebrowse-create-window-config :which-key "create new layout")
                      "lp" '(eyebrowse-prev-window-config :which-key "previous layout")
                      "ln" '(eyebrowse-next-window-config :which-key "next layout")
                      "ls" '(eyebrowse-switch-to-window-config :which-key "switch layout")
                      "ld" '(eyebrowse-close-window-config :which-key "delete this layout")
                      "lr" '(eyebrowse-rename-window-config :which-key "rename layout")
                      "ll" '(eyebrowse-last-window-config :which-key "last layout")
                      "l0" '(eyebrowse-switch-to-window-config-0 :which-key "layout 0")
                      "l1" '(eyebrowse-switch-to-window-config-1 :which-key "layout 1")
                      "l2" '(eyebrowse-switch-to-window-config-2 :which-key "layout 2")
                      "l3" '(eyebrowse-switch-to-window-config-3 :which-key "layout 3")
                      "l4" '(eyebrowse-switch-to-window-config-4 :which-key "layout 4")
                      "l5" '(eyebrowse-switch-to-window-config-5 :which-key "layout 5")
                      "l6" '(eyebrowse-switch-to-window-config-6 :which-key "layout 6")
                      "l7" '(eyebrowse-switch-to-window-config-7 :which-key "layout 7")
                      "l8" '(eyebrowse-switch-to-window-config-8 :which-key "layout 8")
                      "l9" '(eyebrowse-switch-to-window-config-9 :which-key "layout 9")

                      "o" '(:ignore t :which-key "org-mode")
                      "oa" 'api/show-org-agenda-frame
                      "oc" 'api/open-org-capture-frame
                      "od" 'org-deadline
                      "or" 'org-refile
                      "os" 'org-schedule

                      "p" '(:ignore t :which-key "project")
                      "pb" 'counsel-projectile-switch-to-buffer
                      "pp" 'counsel-projectile
                      "pf" 'counsel-projectile-find-file
                      "ps" 'projectile-switch-project
                      "pr" 'counsel-projectile-recentf
                      "pv" 'counsel-projectile-vc
                      "px" 'projectile-invalidate-cache

                      "q" 'evil-quit

                      "/" '(:ignore t :which-key "search")
                      "//" 'swiper
                      "/B" 'swiper-all
                      "/f" 'helm-do-grep-ag

                      "s" '(:ignore t :which-key "sessions")
                      "sa" '(ivy-push-view :which-key "save session")
                      "sd" '(ivy-pop-view :which-key "delete session")
                      "sl" '(api/load-ivy-views :which-key "load saved sessions")
                      "ss" '(ivy-switch-view :which-key "switch session")
                      "sw" '(api/save-ivy-views :which-key "write sessions to file")

                      "t" '(:ignore t :which-key "toggle")
                      "tc" '(evilnc-comment-or-uncomment-lines :which-key "comments")
                      "ti" 'highlight-indent-guides-mode
                      "tt" 'flyspell-mode
                      "ts" 'flycheck-mode
                      "th" 'highlight-symbol
                      "tg" '((lambda ()
                             (interactive)
                             (if (bound-and-true-p golden-ratio-mode)
                                 (progn
                                   (golden-ratio-mode -1)
                                   (balance-windows))
                               (progn
                                 (golden-ratio-mode +1)
                                 (golden-ratio))))
                             :which-key "golden ratio mode")
                      "tw" 'writegood-mode

                      "v" #'er/expand-region
                      "V" #'er/contract-region

                      "w" '(evil-window-map :which-key "window")
                      ;;"w" '(:ignore t :which-key "window")

                      "y" '(:ignore t :which-key "snippets")
                      "yn" 'yas-new-snippet
                      "yi" 'yas-insert-snippet
                      "yv" 'yas-visit-snippet-file

                      "TAB" 'ace-window))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
