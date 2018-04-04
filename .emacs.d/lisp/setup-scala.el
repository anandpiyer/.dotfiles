;;; setup-scala.el --- scala development -*- lexical-binding: t; -*-
;;; Commentary:
;; Set up scala development related environment.
;;; Code:
(use-package ensime
  :defer t
  :pin melpa-stable
  :init
  (add-hook 'scala-mode-hook
            (lambda ()
              (ensime-mode +1)
              (setq prettify-symbols-alist scala-prettify-symbols-alist)
              (prettify-symbols-mode))))
(provide 'setup-scala)
;;; setup-scala.el ends here
