;;; setup-scala.el --- scala development -*- lexical-binding: t; -*-
;;; Commentary:
;; Set up scala development related environment.
;;; Code:
(use-package ensime
  :pin melpa-stable
  :commands (ensime
             ensime-scala-mode-hook)
  :hook (scala-mode . ensime-mode))

(provide 'setup-scala)
;;; setup-scala.el ends here
