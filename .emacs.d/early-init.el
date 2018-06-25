(unless noninteractive
  (defvar api--file-name-handler-alist file-name-handler-alist)
  (unless after-init-time
    (setq gc-cons-threshold (* 512 1024 1024)
          gc-cons-percentage 0.6
          file-name-handler-alist nil
          auto-window-vscroll nil)
    (defun api|reset-gc ()
      (setq gc-cons-threshold (* 16 1024 1024)
            gc-cons-percentage 0.1
            file-name-handler-alist api--file-name-handler-alist))
    (add-hook 'emacs-startup-hook #'api|reset-gc)))

(setq user-emacs-directory (file-name-directory load-file-name)
      package-enable-at-startup nil
      ;;package-quickstart t
      load-prefer-newer noninteractive)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))
