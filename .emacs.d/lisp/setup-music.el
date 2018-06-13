;;; setup-music.el --- Music control with Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq mpc-host "localhost:6600")

;;------------------------------------------------------------------------------
;; `emms': Emacs Multimedia System
;;------------------------------------------------------------------------------
(use-package emms
  :commands (emms
             emms-add-directory-tree
             emms-player-mpd-connect
             emms-cache-set-from-mpd-all
             emms-smart-browse
             emms-pause
             emms-next)
  :init
  (defun api/connect-to-music-daemon ()
    "Connect to MPD."
    (interactive)
    (emms-player-mpd-connect)
    (emms-cache-set-from-mpd-all)
    (message "EMMS connected to MPD!"))
  (add-hook 'emacs-startup-hook #'api/connect-to-music-daemon)
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-source-file-default-directory "~/Music/beets/"
        emms-info-asynchronously t
        emms-show-format "♪ %s"
        emms-seek-seconds 5
        emms-player-list '(emms-player-mpd)
        emms-info-functions '(emms-info-mpd)
        emms-player-mpd-music-directory "~/Music/beets/"
        emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6600")

  (require 'emms-mode-line)
  (emms-mode-line 1)
  (require 'emms-playing-time)
  (emms-playing-time 1)

  (add-hook 'emms-browser-mode-hook 'evil-emacs-state)
  (add-hook 'emms-playlist-mode-hook 'evil-emacs-state))

;;------------------------------------------------------------------------------
;; `mingus': Frontend for GNU Emacs to the Music Player daemon.
;;------------------------------------------------------------------------------
(use-package mingus
  :disabled ;; doesn't play well with mu4e.
  :commands (mingus
             mingus-browse)
  :init
  (setq mingus-mode-always-modeline t
        mingus-mode-line-show-status t
        mingus-mode-line-show-elapsed-time t
        mingus-mode-line-show-volume nil
        mingus-mode-line-separator
        (if window-system
            " ● "
          " + "))
  (add-hook 'mingus-browse-hook 'evil-emacs-state t)
  (add-hook 'mingus-playlist-hooks 'evil-emacs-state t))

;;------------------------------------------------------------------------------
;; `simple-mpc': Very simple frontend for MPD.
;;------------------------------------------------------------------------------
(use-package simple-mpc
  :commands (simple-mpc)
  :config
  (evil-set-initial-state 'simple-mpc-mode 'emacs))

(provide 'setup-music)
;;; setup-music.el ends here
