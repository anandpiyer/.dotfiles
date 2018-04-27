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
              mu4e-update-index
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

        mu4e-completing-read-function 'ivy-completing-read

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
             :name "padmanabha.iyer"
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/anand.padmanabha.iyer@gmail.com"
                                             (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address      . "anand.padmanabha.iyer@gmail.com"  )
                      ( user-full-name         . "Anand" )
                      ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                      ( mu4e-sent-messages-behavior . delete )
                      ( mu4e-sent-folder      . "/anand.padmanabha.iyer@gmail.com/sent" )
                      ( mu4e-drafts-folder    . "/anand.padmanabha.iyer@gmail.com/drafts" )
                      ( mu4e-refile-folder    . "/anand.padmanabha.iyer@gmail.com/all" )
                      ( mu4e-trash-folder     . "/anand.padmanabha.iyer@gmail.com/trash" )))

           ,(make-mu4e-context
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

(use-package mu4e-alert
  :after mu4e
  :commands (mu4e-alert-enable-notifications
             mu4e-alert-enable-mode-line-display)
  :init
  (mu4e-alert-set-default-style 'notifier)
  ;;(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  ;;(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))
  (mu4e-alert-enable-notifications))

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
  :disabled
  :ensure nil
  :commands (notmuch)
  :init
  (autoload 'notmuch "notmuch" "notmuch mail" t))

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
    (prodigy-start-service (prodigy-find-service "imapnotify-anand.iyer.p"))
    (prodigy-start-service (prodigy-find-service "imapnotify-anand.padmanabha.iyer"))
    (prodigy-start-service (prodigy-find-service "imapnotify-anand.ebiz")))
  (add-hook 'emacs-startup-hook #'api|start-prodigy))

(provide 'setup-email)
;;; setup-email.el ends here
