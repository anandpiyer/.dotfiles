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

(setq ;;package-enable-at-startup nil
      package-quickstart t)
