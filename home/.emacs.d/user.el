;; -*- mode: emacs-lisp -*-
;; This file is loaded by ohai-emacs at startup.

(setq default-frame-alist '((cursor-color . "gold")))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq create-lockfiles nil)

;;;
;;; nix mode
;;;

(require 'nix-mode)

;;;
;;; Org capture customisation
;;; from http://cestdiego.github.io/blog/2015/08/19/org-protocol/
;;;

(require 'org-capture)
(require 'org-protocol)

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/perso/organizer.org")))


;;;; Thank you random guy from StackOverflow
;;;; http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection

(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))

;;; Capture Templates
;;;; Add idea, mind-onanism, contacts, movies to download das
(setq org-capture-templates
      '(("l" "Temp Links from the interwebs" item
         (file+headline "~/perso/organizer.org" "Temporary Links")
         "%?\nEntered on %U\n \%i\n %a")

        ("j" "Journal entry" plain
         (file+datetree "~/perso/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)
        ("J" "Journal entry with date" plain
         (file+datetree+prompt "~/perso/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)

        ("n" "Daily note" table-line (file+olp "~/perso/organizer.org" "Daily notes")
          "| %u | %^{Note} |"
          :immediate-finish t)
        ("r" "Notes" entry
          (file+datetree "~/perso/organizer.org")
          "* %?\n\n%i\n")

        ))

;; customize flycheck for otp standard directory structure
(require 'flycheck)
(flycheck-define-checker erlang-otp
  "An Erlang syntax checker using the Erlang interpreter."
  :command ("erlc" "-o" temporary-directory "-Wall"
            "-I" "../include" "-I" "../../include"
            "-I" "../../../include"
            "-I" "../../lib" ;; for nitrogen
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
   (error line-start (file-name) ":" line ": " (message) line-end)))

(add-hook 'erlang-mode-hook
          (lambda ()
            (flycheck-select-checker 'erlang-otp)
            (flycheck-mode)))


;; nitrogen mode
;;(add-to-list 'load-path "/home/bernard/nitrogen/support/nitrogen-mode")
;;(require 'nitrogen-mode)


;;
;; Flycheck for Elixir
;;

;; A Flycheck checker that uses Mix, so it finds project deps.

(flycheck-define-checker elixir-mix-bernard
  "An Elixir syntax checker using the Elixir interpreter.
     See URL `http://elixir-lang.org/'."
  :command ("mix"
            "compile"
            source)
  :predicate is-mix-project-p
  :error-patterns
  ((error line-start "** (" (zero-or-more not-newline) ") "
          (file-name) ":" line ": " (message) line-end)
   (warning line-start
            (file-name) ":"
            line ": "
            (message)
            line-end))
  :modes elixir-mode)

(add-to-list 'flycheck-checkers 'elixir-mix-bernard)

(defun is-mix-project-p ()
  (let ((mix-project-root (locate-dominating-file (buffer-file-name) "mix.exs")))
    (if mix-project-root (cd mix-project-root) nil)))
