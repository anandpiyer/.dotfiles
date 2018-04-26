;;; setup-email.el --- Email -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;------------------------------------------------------------------------------
;; `mu4e':
;;
;; * install `mu' using `EMACS=$(which emacs) brew install mu --with-emacs --HEAD'
;; * install `msmtp': `brew install msmtp'
;; * install `mbsync': `brew install isync'
;;
;; Most of the settings are taken from `mu4e' manual:
;; https://www.djcbsoftware.nl/code/mu/mu4e/
;;------------------------------------------------------------------------------
(use-package mu4e
  :ensure nil ; installs with `brew install mu --with-emacs`
  :commands  (mu4e
              mu4e-compose-new
              mu4e-shr2text)
  :config
  (require 'mu4e)

  (setq mail-user-agent 'mu4e-user-agent

        ;; Use msmtp to send mails.
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"

        ;; Setup a queue directory so that we have an option to work offline.
        ;; smtpmail-queue-mail t  ;; start in queuing mode
        smtpmail-queue-dir   "~/Maildir/queue/cur"

        ;; Tell msmtp to choose the SMTP server according to the from field in the outgoing email
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-sendmail-f-is-evil 't

        ;; Don't keep message buffers around
        message-kill-buffer-on-exit t)

  (setq mu4e-maildir "~/Maildir"

        ;; allow for updating mail using 'U' in the main view:
        mu4e-get-mail-command "mbsync -a"

        mu4e-completing-read-function 'completing-read

        ;; no need to ask before quitting.
        mu4e-confirm-quit 'nil

        ;; use 'fancy' non-ascii characters in various places in mu4e
        ;; mu4e-use-fancy-chars t ; too slow!

        ;; attempt to show images when viewing messages
        mu4e-view-show-images t

        ;; tell mu4e to use w3m for html rendering
        mu4e-html2text-command "w3m -dump -T text/html"

        mu4e-view-show-addresses t

        ;; composer in new frame.
        mu4e-compose-in-new-frame t

        mu4e-compose-format-flowed t

        ;; This enabled the thread like viewing of email similar to gmail's UI.
        mu4e-headers-include-related t

        ;; Skip duplicates during search.
        mu4e-headers-skip-duplicates t

        ;; rename files when moving (NEEDED FOR MBSYNC)
        mu4e-change-filenames-when-moving t)

  ;; add option to view a message in the browser.
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "ebiz"
             :enter-func (lambda () (mu4e-message "Entering anand.ebiz context"))
             :leave-func (lambda () (mu4e-message "Leaving anand.ebiz context"))
             ;; we match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "anand.ebiz@gmail.com")))
             :vars '( ( user-mail-address      . "anand.ebiz@gmail.com"  )
                      ( user-full-name         . "Andy" )
                      ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                      ( mu4e-sent-messages-behavior . delete )
                      ( mu4e-sent-folder      . "/anand.ebiz@gmail.com/sent" )
		              ( mu4e-drafts-folder    . "/anand.ebiz@gmail.com/drafts" )
		              ( mu4e-refile-folder    . "/anand.ebiz@gmail.com/all" )
                      ( mu4e-trash-folder     . "/anand.ebiz@gmail.com/trash" )))

           ,(make-mu4e-context
             :name "iyer.p"
             :enter-func (lambda () (mu4e-message "Switch to the anand.iyer.p context"))
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
                      ( mu4e-compose-signature  .
                                                (concat
                                                 "\n"
                                                 "-Anand\n")))))

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
              mu4e-contexts))))

;;------------------------------------------------------------------------------
;; `mu4e-maildirs-extension': Show maildirs in `mu4e' welcome page.
;;------------------------------------------------------------------------------
(use-package mu4e-maildirs-extension
  :after mu4e
  :init
  (mu4e-maildirs-extension))

;;------------------------------------------------------------------------------
;; `notmuch':
;;------------------------------------------------------------------------------
(use-package notmuch-hello
  :ensure nil
  :commands (notmuch)
  :init
  (autoload 'notmuch "notmuch" "notmuch mail" t)
  :config
  ;; (setq notmuch-saved-searches
  ;;       '((:key "i" :name "inbox" :query "folder:inbox")
  ;;         (:key "D" :name "Deleted" :query "tag:deleted")
  ;;         (:key "F" :name "Flagged" :query "tag:flagged")
  ;;         (:key "S" :name "Sent" :query "folder:sent")
  ;;         (:key "u" :name "unread" :query "tag:unread")
  ;;         ))

  ;; We add items later in reverse order with (add-to-list ...):
  ;;(setq notmuch-hello-sections '())

  ;; Add a thousand separator
  (setq notmuch-hello-thousands-separator ",")

  (defun my-notmuch-hello-insert-searches ()
    "Insert the saved-searches section."
    (widget-insert (propertize "New     Total      Key  List\n"))
    (mapc (lambda (elem)
            (when elem
              (let* ((q_tot (plist-get elem :query))
                     (q_new (concat q_tot " AND tag:unread"))
                     (n_tot (notmuch-hello-query-counts q_tot))
                     (n_new (notmuch-hello-query-counts q_new)))
                (notmuch-hello-query-insert n_new q_new elem)
                (notmuch-hello-query-insert n_tot q_tot elem)
                (widget-insert "   ")
                (widget-insert (plist-get elem :key))
                (widget-insert "    ")
                (widget-insert (plist-get elem :name))
                (widget-insert "\n")
                ))
            )
          notmuch-saved-searches))
  ;;(my-notmuch-hello-insert-searches)
  )

(provide 'setup-email)
;;; setup-email.el ends here
