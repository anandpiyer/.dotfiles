;;; setup-hydra.el --- Hydra -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Hydra related settings
;;
;;; Code:

(defhydra api/themes-hydra (:hint nil :color pink :columns 3)
  "
Themes

^Gruvbox^   ^Material^   ^Other^        ^Solarized^
----------------------------------------------------
_g_: Dark   _m_: Dark    _z_: Zenburn    _s_: Dark
_G_: Light  _M_: Light   _s_: Seoul256   _S_: Light
                           _n_: Nord
"
  ("g" (load-theme 'gruvbox-dark-soft  t))
  ("G" (load-theme 'gruvbox-light t))
  ("m" (load-theme 'material        t))
  ("M" (load-theme 'material-light  t))
  ("s" (load-theme 'solarized-dark  t))
  ("S" (load-theme 'solarized-light  t))
  ("z" (load-theme 'zenburn         t))
  ("s" (load-theme 'seoul256 t))
  ("n" (load-theme 'nord t))
  ("RET" nil "done" :color blue))

(use-package multiple-cursors
  :defer t)

(defhydra api/multiple-cursors-hydra (:hint nil)
  "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))

(provide 'setup-hydra)
;;; setup-hydra.el ends here
