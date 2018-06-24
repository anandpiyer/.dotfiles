;; init-fonts.el --- Fonts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar api-emacs-font-size 14)
(defvar api-emacs-font (font-spec :family "PragmataPro" :size api-emacs-font-size))
(defvar api-emacs-variable-pitch-font (font-spec :family "PragmataPro"))
(defvar api-emacs-fixed-pitch-font (font-spec :family "PragmataPro"))
(defvar api-emacs-unicode-font (font-spec :family "PragmataPro"))
(defvar api-emacs-fallback-font (font-spec :family "PragmataPro"))

(set-face-attribute 'default t :font api-emacs-font)
(set-frame-font api-emacs-font nil t)
(set-fontset-font t 'unicode api-emacs-unicode-font)
(set-face-attribute 'fixed-pitch nil
                    :family api-emacs-fixed-pitch-font
                    :height (* (+ api-emacs-font-size 2) 10))

(set-fontset-font "fontset-default"
                  '(#x2776 . #x2793) api-emacs-fallback-font nil 'prepend)
(set-fontset-font "fontset-default"
                  '(#x24b6 . #x24fe) api-emacs-fallback-font nil 'prepend)
(set-fontset-font "fontset-default"
                  '(#x2295 . #x22a1) api-emacs-fallback-font nil 'prepend)
(set-fontset-font "fontset-default"
                  '(#x2190 . #x2200) api-emacs-fallback-font nil 'prepend)

(provide 'init-fonts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fonts.el ends here
