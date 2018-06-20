;;; setup-email.el --- Email -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;------------------------------------------------------------------------------
;; Defaults:
;;------------------------------------------------------------------------------
(setq mail-user-agent 'mu4e-user-agent
      ;; Show CC and BCC in compose.
      message-default-mail-headers "Cc: \nBcc: \n"

      ;; Use msmtp to send mails.
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"

      ;; Setup a queue directory so that we have an option to work offline.
      ;; smtpmail-queue-mail t  ;; start in queuing mode
      smtpmail-queue-dir   "~/Maildir/queue/cur"

      ;; Tell msmtp to choose the SMTP server according to the from field in the outgoing email
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-sendmail-f-is-evil 't

      ;; make html messages a bit easier to read in dark themes.
      shr-color-visible-luminance-min 80

      ;; Don't keep message buffers around
      message-kill-buffer-on-exit t)

(defun shr-html2text ()
  "Replacement for standard html2text using shr."
  (interactive)
  (let ((dom (libxml-parse-html-region (point-min) (point-max)))
        (shr-width fill-column)
        (shr-inhibit-images t)
        (shr-bullet " "))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

;; https://ericabrahamsen.net/tech/2014/oct/gnus-dovecot-lucene.html
(use-package gnus
  :ensure nil ; in-built
  :config
  ;;(setq gnus-select-method '(nntp "news.gmane.org"))
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)
  (setq gnus-secondary-select-methods
        '((nnimap "anandebiz"
                  (nnimap-stream network)
                  (nnimap-address "localhost")
                  (nnimap-authenticator login)
                  (nnimap-user "anand.ebiz@gmail.com"))
          (nnimap "anand.padmanabha.iyer"
                  (nnimap-stream network)
                  (nnimap-address "localhost")
                  (nnimap-authenticator login)
                  (nnimap-user "anand.padmanabha.iyer@gmail.com")))))

;;------------------------------------------------------------------------------
;; `mu4e':
;;
;; * install `mu' using `EMACS=$(which emacs) brew install mu --with-emacs --HEAD'
;; * install `msmtp': `brew install msmtp'
;; * install `mbsync': `brew install isync'
;;
;; Most of the settings are taken from the `mu4e' manual:
;; https://www.djcbsoftware.nl/code/mu/mu4e/
;;------------------------------------------------------------------------------
(use-package mu4e
  :ensure nil ; installs with `brew install mu --with-emacs`
  :commands  (mu4e
              mu4e-compose-new
              mu4e-update-index
              mu4e-shr2text)
  :init
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp"); /mu/mu4e")
  :config
  (require 'mu4e)
  (require 'mu4e-contrib)

  (setq mail-user-agent 'mu4e-user-agent)

  (defvaralias 'mu4e-compose-signature 'message-signature)

  (setq mu4e-maildir "~/Maildir"

        mu4e-compose-dont-reply-to-self t

        ;; allow for updating mail using 'U' in the main view:
        mu4e-get-mail-command "mbsync -a"

        ;; sync every 5 minutes.
        mu4e-update-interval 300

        mu4e-completing-read-function 'ivy-completing-read

        ;; no need to ask before quitting.
        mu4e-confirm-quit 'nil

        ;; use 'fancy' non-ascii characters in various places in mu4e
        ;;mu4e-use-fancy-chars t ; too slow!

        ;; attempt to show images when viewing messages
        mu4e-view-show-images t

        ;; restrict image width
        mu4e-view-image-max-width 800

        ;; tell mu4e to use w3m for html rendering
        ;;mu4e-html2text-command "w3m -dump -T text/html"
        ;;mu4e-html2text-command 'mu4e-shr2text
        ;; mu4e-html2text-command "w3m -dump -s -T text/html -o display_link_number=true"
        mu4e-view-prefer-html t
        ;;mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain"

        mu4e-view-show-addresses t

        ;; composer in new frame.
        mu4e-compose-in-new-frame t

        mu4e-compose-format-flowed t

        ;;mu4e-index-cleanup nil
        ;;mu4e-index-lazy-check t
        mu4e-hide-index-messages t

        ;; This enabled the thread like viewing of email similar to gmail's UI.
        mu4e-headers-include-related 'nil
        mu4e-headers-show-threads 'nil

        ;; Skip duplicates during search.
        mu4e-headers-skip-duplicates t

        ;; rename files when moving (NEEDED FOR MBSYNC)
        mu4e-change-filenames-when-moving t

        ;; number of lines visible in split view.
        mu4e-headers-visible-lines 15

        ;; Remove "lists" from columns.
        mu4e-headers-fields '((:human-date . 16)
                              (:flags . 6)
                              (:size . 6)
                              (:from . 25)
                              (:subject)))

  ;; Add a column to display what email account the email belongs to.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

   ;; Refresh the current view after marks are executed
  ;;(defun api*refresh-mu4e-view (&rest _) (mu4e-headers-rerun-search))
  ;;(advice-add #'mu4e-mark-execute-all :after #'api*refresh-mu4e-view)

  ;; add option to view a message in the browser.
  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; Wrap text in messages
  (add-hook 'mu4e-view-mode-hook
            (lambda () (setq-local truncate-lines nil)))

  ;; Turn on spell check in compose mode.
  (after! flyspell
    (add-hook 'mu4e-compose-mode-hook #'turn-on-flyspell))

  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "padmanabha.iyer"
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/anand.padmanabha.iyer@gmail.com"
                                             (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address      . "anand.padmanabha.iyer@gmail.com"  )
                      ( user-full-name         . "Anand" )
                      ( mu4e-compose-signature  . "Anand\n")
                      ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                      ( mu4e-sent-messages-behavior . delete )
                      ( mu4e-sent-folder      . "/anand.padmanabha.iyer@gmail.com/sent" )
                      ( mu4e-drafts-folder    . "/anand.padmanabha.iyer@gmail.com/drafts" )
                      ( mu4e-refile-folder    . "/anand.padmanabha.iyer@gmail.com/all" )
                      ( mu4e-trash-folder     . "/anand.padmanabha.iyer@gmail.com/trash" )))

           ,(make-mu4e-context
             :name "cs-bmail"
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "api@cs.berkeley.edu")))
             :vars '( ( user-mail-address      . "api@cs.berkeley.edu"  )
                      ( user-full-name         . "Anand Iyer" )
                      ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                      ( mu4e-sent-messages-behavior . delete )
                      ( mu4e-sent-folder      . "/bmail/sent" )
                      ( mu4e-drafts-folder    . "/bmail/drafts" )
                      ( mu4e-refile-folder    . "/bmail/all" )
                      ( mu4e-trash-folder     . "/bmail/trash" )
                      ( mu4e-compose-signature  . (concat "\n" "Anand\n"))))

           ,(make-mu4e-context
             :name "ebiz"
             ;; we match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/anand.ebiz@gmail.com" (mu4e-message-field msg :maildir))))
             ;; :match-func (lambda (msg)
             ;;               (when msg
             ;;                 (mu4e-message-contact-field-matches msg
             ;;                                                     :to "anand.ebiz@gmail.com")))
             :vars '( ( user-mail-address      . "anand.ebiz@gmail.com"  )
                      ( user-full-name         . "Andy" )
                      ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                      ( mu4e-sent-messages-behavior . delete )
                      ( mu4e-sent-folder      . "/anand.ebiz@gmail.com/sent" )
                      ( mu4e-drafts-folder    . "/anand.ebiz@gmail.com/drafts" )
                      ( mu4e-refile-folder    . "/anand.ebiz@gmail.com/all" )
                      ( mu4e-trash-folder     . "/anand.ebiz@gmail.com/trash" )))

           ,(make-mu4e-context
             :name "bmail"
             :match-func (lambda (msg)
                           (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "berkeley.edu$")))
                             ;;(string-match-p "berkeley.edu$" (mu4e-message-field :to))))
             :vars '( ( user-mail-address      . "anand.iyer@berkeley.edu"  )
                      ( user-full-name         . "Anand Iyer" )
                      ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                      ( mu4e-sent-messages-behavior . delete )
                      ( mu4e-sent-folder      . "/bmail/sent" )
                      ( mu4e-drafts-folder    . "/bmail/drafts" )
                      ( mu4e-refile-folder    . "/bmail/all" )
                      ( mu4e-trash-folder     . "/bmail/trash" )
                      ( mu4e-compose-signature  . "Anand\n")))

           ,(make-mu4e-context
             :name "iyer.p"
             ;;:enter-func (lambda () (mu4e-message "Switch to the anand.iyer.p context"))
             ;; no leave-func
             ;; we match based on the maildir of the message
             ;; this matches maildir /Arkham and its sub-directories
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/anand.iyer.p@gmail.com" (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address       . "anand.iyer.p@gmail.com" )
                      ( user-full-name          . "Anand Iyer" )
                      ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                      ( mu4e-sent-messages-behavior . delete )
                      ( mu4e-sent-folder      . "/anand.iyer.p@gmail.com/sent" )
                      ( mu4e-drafts-folder    . "/anand.iyer.p@gmail.com/drafts" )
                      ( mu4e-refile-folder    . "/anand.iyer.p@gmail.com/all" )
                      ( mu4e-trash-folder     . "/anand.iyer.p@gmail.com/trash")
                      ( mu4e-compose-signature  . "Anand\n"))))

        ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
        ;; guess or ask the correct context, e.g.

        ;; start with the first (default) context;
        ;; default is to ask-if-none (ask when there's no context yet, and none match)
        mu4e-context-policy 'pick-first

        ;; compose with the current context is no context matches;
        ;; default is to ask
        mu4e-compose-context-policy 'ask-if-none)

  ;; This sets `mu4e-user-mail-address-list' to the concatenation of all
  ;; `user-mail-address' values for all contexts. If you have other mail
  ;; addresses as well, you'll need to add those manually.
  (setq mu4e-user-mail-address-list
    (delq nil
      (mapcar (lambda (context)
                (when (mu4e-context-vars context)
                  (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
              mu4e-contexts)))

  ;; Mail directory shortcuts.
  (setq mu4e-maildir-shortcuts
        '(("/bmail/INBOX" . ?b)
          ("/bmail/all" . ?B)
          ("/anand.ebiz@gmail.com/INBOX" . ?e)
          ("/anand.ebiz@gmail.com/all" . ?E)
          ("/anand.iyer.p@gmail.com/INBOX" . ?p)
          ("/anand.iyer.p@gmail.com/all" . ?P)
          ("/anand.padmanabha.iyer@gmail.com/INBOX" . ?a)
          ("/anand.padmanabha.iyer@gmail.com/all" . ?A)))

  ;; Bookmarks
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now AND NOT flag:trashed" "Today's messages" ?t)
          ("date:7d..now AND NOT flag:trashed" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar (lambda (context)
                                 (when (mu4e-context-vars context)
                                   (concat "maildir:" (cdr (assq 'mu4e-refile-folder (mu4e-context-vars context))))))
                               mu4e-contexts) " OR ")
           "All mail" ?a)
         (,(mapconcat 'identity
                       (mapcar (lambda (context)
                                 (when (mu4e-context-vars context)
                                   (concat "maildir:" (cdr (assq 'mu4e-sent-folder (mu4e-context-vars context))))))
                               mu4e-contexts) " OR ")
          "All sent" ?s)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (if (string-suffix-p "INBOX" (car maildir))
                            (concat "maildir:" (car maildir))))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i)))

  ;; Take care of gmail.
  (add-hook 'mu4e-mark-execute-pre-hook
          (lambda (mark msg)
            (cond ((equal mark 'refile) (mu4e-action-retag-message msg "-\\Inbox"))
                  ((equal mark 'trash) (mu4e-action-retag-message msg "-\\Inbox"))
                  ((equal mark 'flag) (mu4e-action-retag-message msg "-\\Inbox,\\Starred"))
                  ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))
)

;;------------------------------------------------------------------------------
;; `mu4e-alert':
;;------------------------------------------------------------------------------
(use-package mu4e-alert
  :disabled
  :after mu4e
  :init
  (mu4e-alert-enable-mode-line-display))

;;------------------------------------------------------------------------------
;; `mu4e-conversation': Show messages as conversations.
;;------------------------------------------------------------------------------
(use-package mu4e-conversation
  :after mu4e
  :config
  (require 'mu4e-conversation)
  (setq mu4e-view-func 'mu4e-conversation))

;;------------------------------------------------------------------------------
;; `mu4e-maildirs-extension': Show maildirs in `mu4e' welcome page.
;;------------------------------------------------------------------------------
(use-package mu4e-maildirs-extension
  :disabled
  :after mu4e
  :init
  (mu4e-maildirs-extension))

;;------------------------------------------------------------------------------
;; `notmuch':
;;------------------------------------------------------------------------------
(use-package notmuch-hello
  ;;  :disabled
  :ensure nil
  :commands (notmuch notmuch-hello-search notmuch-hello-delete-search-from-history
                     notmuch-hello-insert-search notmuch-hello-nice-number
                     notmuch-hello-widget-search)
  :init
  ;;(autoload 'notmuch "notmuch" "notmuch mail" t)

  (setq notmuch-show-logo nil
        ;; Newer messages on top.
        notmuch-search-oldest-first 'nil
        ;; only show one message
        notmuch-show-only-matching-messages t
        )

  ;;(setq mm-text-html-renderer "w3m")

  :config

  (require 'notmuch)

  (eval-after-load 'notmuch-show
    '(define-key notmuch-show-mode-map "`" 'notmuch-show-apply-tag-macro))

  (setq notmuch-show-tag-macro-alist
        (list
         '("d" "+notmuch::trash" "-notmuch::new" "-notmuch::inbox")))

  (defun notmuch-show-apply-tag-macro (key)
    (interactive "k")
    (let ((macro (assoc key notmuch-show-tag-macro-alist)))
      (apply 'notmuch-show-tag-message (cdr macro))))

 (define-key notmuch-search-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "trash" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-trash"))
      (notmuch-search-tag (list "+trash" "-inbox" "-new" "-unread")))))

 (define-key notmuch-show-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "trash" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-trash"))
      (notmuch-show-tag (list "+trash" "-new" "-unread" "-inbox")))))

  (define-key notmuch-tree-mode-map "d"
    (lambda ()
      "mark message as deleted"
      (interactive)
      (notmuch-tree-tag (list "+trash" "-new" "-unread" "-important" "-inbox")))))

;;------------------------------------------------------------------------------
;; `org-mu4e':
;; http://pragmaticemacs.com/emacs/master-your-inbox-with-mu4e-and-org-mode/
;;------------------------------------------------------------------------------
(use-package org-mu4e
  :ensure nil ; bundled with mu4e
  :commands (org-mu4e-store-and-capture)
  ;;:hook (mu4e-compose-mode . org-mu4e-compose-org-mode)
  :bind
  (:map mu4e-headers-mode-map
        ("C-c c" . org-mu4e-store-and-capture)
        :map mu4e-view-mode-map
        ("C-c c" . org-mu4e-store-and-capture))
  :config

  ;;store link to message if in header view, not to header query
  (setq org-mu4e-link-query-in-headers-mode nil)

  ;; Only render to html once. If the first send fails for whatever reason,
  ;; org-mu4e would do so each time you try again.
  ;;(add-hook 'message-send-hook (lambda ()
  ;;(setq-local org-mu4e-convert-to-html nil))))
  )

;;------------------------------------------------------------------------------
;; `prodigy': Manage imapnotify for email notifications.
;;
;; * Install imapnotify: `npm install -g imapnotify'
;;------------------------------------------------------------------------------
(use-package prodigy
  ;;:disabled
  :init
  (prodigy-define-tag
          :name 'email
          :ready-message "Checking Email using IMAP IDLE. Ctrl-C to shutdown.")

  (prodigy-define-service
    :name "imapnotify-bmail"
    :command "imapnotify"
    :args '("-c" "~/.config/imapnotify/bmail.js")
    :tags '(email)
    :auto-start t
    :kill-process-buffer-on-stop t
    :kill-signal 'sigkill)

  (prodigy-define-service
    :name "imapnotify-anand.ebiz"
    :command "imapnotify"
    :args '("-c" "~/.config/imapnotify/anand.ebiz@gmail.com.js")
    :tags '(email)
    :auto-start t
    :kill-process-buffer-on-stop t
    :kill-signal 'sigkill)

  (prodigy-define-service
    :name "imapnotify-anand.iyer.p"
    :command "imapnotify"
    :args '("-c" "~/.config/imapnotify/anand.iyer.p@gmail.com.js")
    :tags '(email)
    :auto-start t
    :kill-process-buffer-on-stop t
    :kill-signal 'sigkill)

  (prodigy-define-service
    :name "imapnotify-anand.padmanabha.iyer"
    :command "imapnotify"
    :args '("-c" "~/.config/imapnotify/anand.padmanabha.iyer@gmail.com.js")
    :tags '(email)
    :auto-start t
    :kill-process-buffer-on-stop t
    :kill-signal 'sigkill)

  (defun api|start-prodigy ()
    (prodigy-start-service (prodigy-find-service "imapnotify-bmail"))
    (prodigy-start-service (prodigy-find-service "imapnotify-anand.iyer.p"))
    (prodigy-start-service (prodigy-find-service "imapnotify-anand.padmanabha.iyer"))
    (prodigy-start-service (prodigy-find-service "imapnotify-anand.ebiz")))
  (add-hook 'emacs-startup-hook #'api|start-prodigy))

(provide 'setup-email)
;;; setup-email.el ends here
