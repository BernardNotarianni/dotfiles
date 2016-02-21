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
            "-pa" "../_build/default/lib/cowboy/ebin" ;; cowboy behaviours
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

;;====================================================================

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

;;
;; Open new frame with Phoenix templates/view/controller/model
;;

(defun open-mvc ()
  "Open Phoenix MVC files in current frame"
  (interactive)
  (let ((concept (choose-concept))
        (color-scheme (choose-color-scheme)))
    (open-mvc-from-root (mix-project-root) concept color-scheme)))

(defun choose-concept ()
  (let* ((root (mix-project-root))
         (files (phoenix-controllers root)))
    (completing-read "Open MVC concept: " files)))

(defun choose-color-scheme ()
  (let ((colors-string (mapcar 'number-to-string (number-sequence 0 (- (length mvc-colors-scheme) 1)))))
    (nth (string-to-number (completing-read "Choose color scheme:" colors-string))
         mvc-colors-scheme)))


(defun open-mvc-from-root (root concept colors)
  (let ((files (zip-color-scheme (mvc-files root concept) colors)))
    (delete-other-windows)
    (find-file-with-background (first files))
    (mapcar (lambda (f)
              (select-window (split-window-below))
              (find-file-with-background f))
            (cdr files))
    (balance-windows)))

(defun find-file-with-background (file-dot-color)
  (let ((file-name (car file-dot-color))
        (background-color (cdr file-dot-color)))
    (find-file file-name)
    (buffer-face-set (list :background background-color))))

(defun zip-color-scheme (list-of-mvc-files colors)
  (mapcar* #'cons list-of-mvc-files colors))


(defun mix-project-root ()
  (let ((current-path (if (string= "dired-mode" major-mode)
                          default-directory
                        (buffer-file-name))))
    (locate-dominating-file current-path "mix.exs")))

(defun mvc-files (root concept)
  (mapcar (lambda (s) (format s root concept))
          '("%s/web/templates/%s"
            "%s/web/views/%s_view.ex"
            "%s/web/controllers/%s_controller.ex"
            "%s/web/models/%s.ex")))

; shemas computed with http://color.adobe.com
(setq mvc-colors-scheme
     '(("black" "black" "black" "black")
       ("black" "#592E02" "#422F05" "#594B02" "#4F4F26")
       ("black" "#36103B" "#34044A" "#280054")
       ("black" "#042B0B" "#043B02" "#194506")
       ))

(defun phoenix-controllers (root-dir)
  (mapcar 'controller-name-to-concept
          (directory-files (concat root-dir "/web/controllers") nil "_controller.ex")))

(defun controller-name-to-concept (file-name)
  (string-match "\\(.*\\)_controller.ex" file-name)
  (match-string 1 file-name))
