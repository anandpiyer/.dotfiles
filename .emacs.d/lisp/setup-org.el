;;; setup-org.el --- Org setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Some settings related to org mode.  Majorly based on:
;; https://m.reddit.com/r/emacs/comments/4gudyw/help_me_with_my_orgmode_workflow_for_notetaking/
;; https://github.com/sriramkswamy/dotemacs/

;;; Code:

;;------------------------------------------------------------------------------
;; Defaults & helpers:
;;------------------------------------------------------------------------------
;; http://stackoverflow.com/questions/21073859/is-there-a-way-with-org-capture-templates-to-not-insert-a-line-if-initial-conten
(defun v-i-or-nothing ()
  "Use initial content only if available."
  (let ((v-i (plist-get org-store-link-plist :initial)))
    (if (equal v-i "")
        ""
      (concat v-i "\n"))))

;;------------------------------------------------------------------------------
;; `evil-org':
;;------------------------------------------------------------------------------
(use-package evil-org
  :commands (evil-org-mode evil-org-recompute-clocks)
  :hook (org-mode . evil-org-mode)
  :config
  (setf evil-org-key-theme '(navigation insert textobjects additional))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;------------------------------------------------------------------------------
;; `org':
;;------------------------------------------------------------------------------
(use-package org-plus-contrib
  :mode (("\\.org\\'" . org-mode))
  ;;:bind ("C-c c" . org-capture)
  :init
  (setq org-directory org-root-directory
        org-default-notes-file (concat org-directory "organizer.org")
        org-startup-with-inline-images t
        org-src-fontify-natively t
        org-imenu-depth 8
        org-log-done t
        org-log-into-drawer t
        org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

  (setq org-agenda-files
        (list (concat org-directory "organizer.org")
              (concat org-directory "beorg.org")
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
                ("e" "Email-process-soon" entry (file+headline org-default-notes-file "Tasks")
                 "* TODO [Email] %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))"
                 :empty-lines 1)
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

  ;;;###autoload
  (defun api/show-org-agenda-frame ()
    "Show org-agenda in new frame or select the frame if already open."
    (interactive)
    (let* ((name "Org Agenda")
           (curr-frame (selected-frame))
           (frame-alist (nameframe-frame-alist))
           (frame (nameframe-get-frame name frame-alist)))
      (cond
       ;; org-agenda frame already exists
       ((and frame (not (equal frame curr-frame)))
        (select-frame-set-input-focus frame))
       ((not frame)
        (progn (nameframe-make-frame name)
               (funcall #'org-agenda-list)
               (delete-other-windows))))))

  ;; -- Make org-capture popup in its own frame.
  ;;;###autoload
  (defun api/open-org-capture-frame ()
    "Create a new frame and run `org-capture'."
    (interactive)
    (select-frame (make-frame '((api|org-capture . t))))
    (delete-other-windows)
    (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
      (condition-case err
          (org-capture)
        ;; `org-capture' signals (error "Abort") when "q" is typed, so
        ;; delete the newly-created frame in this scenario.
        (error
         (message "org-capture: %s" (error-message-string err))
         (delete-frame)))))

   (defadvice org-capture-finalize (after delete-capture-frame activate)
    "Delete the frame after `capture-finalize'."
    (when (frame-parameter nil 'api|org-capture)
      (delete-frame)))

  (defadvice org-capture-destroy (after delete-capture-frame activate)
    "Delete the frame after `capture-destroy'."
    (when (frame-parameter nil 'api|org-capture)
      (delete-frame)))

  ;;Add creation date as a property in all captures.
  (require 'org-expiry)
  (add-hook 'org-capture-before-finalize-hook
         #'(lambda()
               (save-excursion
                    (org-back-to-heading)
                    (org-expiry-insert-created))))

  ;; Once inside the capture, change to insert state.
  (add-hook 'org-capture-mode-hook #'evil-insert-state)

  ;; No need to show line numbers in org mode.
  (add-hook 'org-mode-hook #'api/disable-line-numbers)

  (setq org-publish-project-alist
        '(

          ("org-api"
           ;; Path to your org files.
           :base-directory "~/Code/anand-iyer.com/_org/"
           :base-extension "org"

           ;; Path to your Jekyll project.
           :publishing-directory "~/Code/anand-iyer.com/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :body-only t ;; Only export section between <body> </body>

           :section-numbers nil
           :with-toc nil
           :auto-index nil
           :auto-preamble nil
           :auto-postamble nil
           ))))

;;------------------------------------------------------------------------------
;; `org-babel':
;;------------------------------------------------------------------------------
(use-package org-babel
  :ensure nil ; org
  :after org
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (C . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (js . t)
     (perl . t)
     (python . t)
     (R . t)
     (sql . t))))

;;------------------------------------------------------------------------------
;; `org-bullets':
;;------------------------------------------------------------------------------
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;;------------------------------------------------------------------------------
;; `org-mime':
;;------------------------------------------------------------------------------
(use-package org-mime
  :disabled
  :after (org mu4e)
  :config
  (setq
   org-mime-library 'mml
   org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil)))

;;------------------------------------------------------------------------------
;; `org-ref':
;;------------------------------------------------------------------------------
(use-package org-ref
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-notes-directory (concat org-directory "papers/notes")
        org-ref-bibliography-notes (concat org-directory "papers/notes.org")
        org-ref-default-bibliography `(,(concat org-directory "papers/references.bib"))
        org-ref-pdf-directory (concat org-directory "papers/pdfs/"))
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (require 'org-ref)
                             (require 'org-ref-latex)
                             (require 'org-ref-pdf)
                             (require 'org-ref-url-utils))))

;; interleave PDFs with notes. This needs to be after pdf-tools. Also, interleave
;; needs to be removed and reinstalled everytime pdf-tools is updated.
;; See: https://github.com/rudolfochrist/interleave/issues/31#issuecomment-252351991
(use-package interleave
  :init
  (setq interleave-org-notes-dir-list `(,(concat org-directory "papers")))
  (with-eval-after-load 'doc-view
    (bind-key "i" #'interleave--open-notes-file-for-pdf doc-view-mode-map))
  (with-eval-after-load 'pdf-view
    (bind-key "i" #'interleave--open-notes-file-for-pdf pdf-view-mode-map)))

 ;; notational velocity and nvALT replacement.
(use-package deft
  :commands (deft)
  :init
  (defun api/deft ()
    "Helper for deft"
    (interactive)
    (deft)
    (evil-insert-state nil))
  (setq deft-directory "~/Notes"
        deft-extensions '("org" "md" "txt" "markdown")
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'setup-org)
;;; setup-org.el ends here
