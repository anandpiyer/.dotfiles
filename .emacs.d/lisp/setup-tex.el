;;; setup-tex.el --- latex configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;------------------------------------------------------------------------------
;; Defaults & Helpers
;;------------------------------------------------------------------------------
(put 'TeX-command-extra-options 'safe-local-variable #'string)

(defun api/current-bib ()
  "Set the bibliography to the current directory."
  (interactive)
  (let ((local-bib-file (concat (file-name-directory buffer-file-name) "references.bib")))
    (if (file-exists-p local-bib-file)
        (progn
          (setq bibtex-completion-bibliography local-bib-file)
          (message (concat "Local bib file is " local-bib-file)))
      (message "Local bib file references.bib not found!"))))

;; (setenv "PATH"
;;         (concat
;;          (getenv "PATH")
;;          ":/Library/TeX/texbin"
;;          ":/usr/local/bin"))

;; (setq exec-path
;;       (append exec-path
;;               '("/Library/TeX/texbin"
;;                 "/usr/local/bin")))

;;------------------------------------------------------------------------------
;; `auctex-latexmk': Latex Mk support for AUCTeX
;;------------------------------------------------------------------------------
(use-package auctex-latexmk
  ;;:disabled
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (add-hook 'emacs-startup-hook #'auctex-latexmk-setup))

;;------------------------------------------------------------------------------
;; `bibtex':
;;------------------------------------------------------------------------------
(use-package bibtex
  :ensure nil ; in-built
  :mode ("\\.bib" . bibtex-mode)
  :config
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an")
        bibtex-autokey-titleword-length 0
        bibtex-autokey-titlewords 0
        bibtex-autokey-year-length 4))

;;------------------------------------------------------------------------------
;; `company-auctex':
;;------------------------------------------------------------------------------
(use-package company-auctex
  :after company
  :init
  (defun api|add-company-auctex-backend ()
    "Add company-auctex to company backends."
    (make-variable-buffer-local 'company-backends)
    (company-auctex-init))
  (add-hook 'latex-mode-hook #'api|add-company-auctex-backend))

;;------------------------------------------------------------------------------
;; `helm-bibtex':
;;------------------------------------------------------------------------------
;; (use-package helm-bibtex
;;   :disabled
;;   :requires helm
;;   :commands helm-bibtex
;;   :init
;;   (setq bibtex-completion-bibliography `(,(concat org-root-directory "papers/references.bib"))
;;         bibtex-completion-library-path (concat org-root-directory "papers/pdfs")
;;         bibtex-completion-notes-path (concat org-root-directory "papers/notes.org")))

;;------------------------------------------------------------------------------
;; `ivy-bibtex':
;;------------------------------------------------------------------------------
(use-package ivy-bibtex
  :after ivy
  :commands (ivy-bibtex
             ivy-bibtex-with-local-bibliography)
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key))

;;------------------------------------------------------------------------------
;; `reftex':
;;------------------------------------------------------------------------------
(use-package reftex
  :ensure nil ; in-built
  :commands (turn-on-reftex)
  :init
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex))

;;------------------------------------------------------------------------------
;; `tex':
;;------------------------------------------------------------------------------
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t
        ;;TeX-show-compilation t
        TeX-view-program-selection '((output-pdf "PDF Viewer"))
        TeX-source-correlate-start-server t
        TeX-syntactic-comment t
        LaTeX-fill-break-at-separators nil
        LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label))

  (defadvice TeX-LaTeX-sentinel
      (around mg-TeX-LaTeX-sentinel-open-output activate)
    "Open output when there are errors."
    ;; Run `TeX-LaTeX-sentinel' as usual.
    ad-do-it
    ;; Check for the presence of errors.
    (when
        (with-current-buffer TeX-command-buffer
          (plist-get TeX-error-report-switches (intern (TeX-master-file))))
      ;; If there are errors, open the output buffer.
      (TeX-recenter-output-buffer nil)))

  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)
  :config
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (unless (assoc "PDF Tools" TeX-view-program-list-builtin)
    (push '("PDF Tools" TeX-pdf-tools-sync-view) TeX-view-program-list))
  ;;(add-hook 'LaTeX-mode-hook (lambda () (setq compile-command "latexmk -pdf")))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(provide 'setup-tex)
;;; setup-tex.el ends here
