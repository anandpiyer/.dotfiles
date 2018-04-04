;;; setup-org.el --- org setup -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Some settings related to org mode.  Majorly based on:
;; https://m.reddit.com/r/emacs/comments/4gudyw/help_me_with_my_orgmode_workflow_for_notetaking/
;; https://github.com/sriramkswamy/dotemacs/
;; 
;;; Code:

;; http://stackoverflow.com/questions/21073859/is-there-a-way-with-org-capture-templates-to-not-insert-a-line-if-initial-conten
(defun v-i-or-nothing ()
  "Use initial content only if available."
  (let ((v-i (plist-get org-store-link-plist :initial)))
    (if (equal v-i "")
        ""
      (concat v-i "\n"))))

(use-package org-plus-contrib
  :defer t
  :bind ("C-c c" . org-capture)
  :init
  (setq org-directory org-root-directory
        org-default-notes-file (concat org-directory "organizer.org")
        org-startup-with-inline-images t
        org-src-fontify-natively t
        org-imenu-depth 8
        org-log-done t
        org-log-into-drawer t
        org-agenda-window-setup 'current-window
        org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

  (setq org-agenda-files
        (list (concat org-directory "organizer.org")
              (concat org-directory "beorg.org")
              (concat org-directory "notes.org")
              (concat org-directory "papers/notes.org"))
        org-deadline-warning-days 7
        org-agenda-start-on-weekday nil
        org-agenda-show-all-dates t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-span 7
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-reverse-note-order t)

  (setq org-capture-templates
        (quote (("t" "Todo" entry
                 (file+headline org-default-notes-file "Inbox")
                 "* TODO %^{Todo}\n:PROPERTIES:\n:CREATED: %T\n:END:\n%(v-i-or-nothing)"
                 :empty-lines 1
                 :prepend t
                 :immediate-finish t
                 :kill-buffer t)
                ("n" "Note" entry (file+headline org-default-notes-file "Notes")
                 "* %U %?"
                 :prepend t
                 :empty-lines 1
                 :kill-buffer t)
                ("p" "Paper" entry
                 (file+headline org-default-notes-file "Papers")
                 "* %^{Title} %(org-set-tags)\n:PROPERTIES:\n:CREATED: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?"
                 :prepend t
                 :empty-lines 1
                 :created t))))

  ;; Add creation date as a property in all captures.
  ;; (require 'org-expiry)
  ;; (add-hook 'org-capture-before-finalize-hook
  ;;        #'(lambda()
  ;;              (save-excursion
  ;;                   (org-back-to-heading)
  ;;                   (org-expiry-insert-created))))

  ;; Once inside the capture, change to insert state.
  (add-hook 'org-capture-mode-hook #'evil-insert-state)

  ;; No need to show line numbers in org mode.
  (add-hook 'org-mode-hook #'api|disable-line-numbers))

(use-package org-bullets
  :defer t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package evil-org
  :diminish (evil-org-mode . "ⓔ")
  :commands (evil-org-mode evil-org-recompute-clocks)
  :init (add-hook 'org-mode-hook 'evil-org-mode))

;; use brew to install pdf-tools so that epdfinfo gets installed properly:
;;     brew install pdf-tools
;; and set the path to epdfinfo from brew installation. To upgrade, do
;; 'brew upgrade pdf-tools' PRIOR to upgrading pdf-tools emacs package. In case
;; things are messed up, uninstall brew pdf-tools, remove elpa pdf-tools and
;; start over.
 (use-package pdf-tools
   :defer t
   :mode (("\\.pdf\\'" . pdf-view-mode))
   :config
   (progn
     (custom-set-variables '(pdf-tools-handle-upgrades nil))
     (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
     (pdf-tools-install)))

(use-package org-ref
  :defer t
  :init
  (progn (setq org-ref-completion-library 'org-ref-ivy-cite
               org-ref-notes-directory (concat org-directory "papers/notes")
               org-ref-bibliography-notes (concat org-directory "papers/notes.org")
               org-ref-default-bibliography `(,(concat org-directory "papers/references.bib"))
               org-ref-pdf-directory (concat org-directory "papers/pdfs/"))
         (add-hook 'org-mode-hook (lambda ()
                                    (require 'org-ref)
                                    (require 'org-ref-latex)
                                    (require 'org-ref-pdf)
                                    (require 'org-ref-url-utils)))))

;; interleave PDFs with notes. This needs to be after pdf-tools. Also, interleave
;; needs to be removed and reinstalled everytime pdf-tools is updated.
;; See: https://github.com/rudolfochrist/interleave/issues/31#issuecomment-252351991
(use-package interleave
  :defer t
  :init
  (progn
    (setq interleave-org-notes-dir-list `(,(concat org-directory "papers")))
    (with-eval-after-load 'doc-view
      (bind-key "i" #'interleave--open-notes-file-for-pdf doc-view-mode-map))
    (with-eval-after-load 'pdf-view
      (bind-key "i" #'interleave--open-notes-file-for-pdf pdf-view-mode-map))))

 ;; notational velocity and nvALT replacement.
(use-package deft
  :commands (deft)
  :init
  (progn
    (setq deft-directory "~/Google Drive/Notes"
          deft-extensions '("org" "md" "txt" "markdown")
          deft-text-mode 'org-mode
          deft-use-filename-as-title t
          deft-use-filter-string-for-filename t)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'setup-org)
;;; setup-org.el ends here
