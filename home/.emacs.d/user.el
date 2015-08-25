;; -*- mode: emacs-lisp -*-
;; This file is loaded by ohai-emacs at startup.

(set-cursor-color "gold")

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;;
;;; Org capture customisation
;;; from http://cestdiego.github.io/blog/2015/08/19/org-protocol/
;;;

(require 'org-capture)
(require 'org-protocol)

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
         (file+headline "links.org" "Temporary Links")
         "%?\nEntered on %U\n \%i\n %a")))
