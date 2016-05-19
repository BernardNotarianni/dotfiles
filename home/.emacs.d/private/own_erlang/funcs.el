(defun erlang-export (fun-arity)
  "Create an export clause for the function under the cursor."
  (interactive
   (list (read-no-blanks-input "function/arity: " (erlang-current-function))))
  (save-excursion
    (erlang-move-to-export-insertion)
    (newline)
    (insert (format "-export ([%s])." fun-arity))))

(defun erlang-current-function ()
  "Find the name of the function under the cursor."
  (save-excursion
    (if (not (equal (point) (point-max))) (forward-char))
    (erlang-beginning-of-function)
    (let ((beg (point))
          (fun-name)
          (fun-arity)
          (result '()))
      (search-forward "(")
      (backward-char)
      (setq fun-name (stripwhite (buffer-substring beg (point))))
      (if (char-equal (char-after) ?\))
          (setq fun-arity 0)
        (forward-char)
        (setq fun-arity 0)
        (while (not (char-equal (char-after) ?\)))
          (erlang-forward-arg)
          (setq fun-arity (+ fun-arity 1))))
      (format "%s/%d" fun-name fun-arity))))

(defun erlang-move-to-export-insertion ()
  (interactive)
  (goto-char (point-max))
  (if (search-backward-regexp "^-export" 0 t)
      (end-of-line)
    (search-backward-regexp "^-" 0 t)
    (end-of-line)))

(defun erlang-forward-arg ()
  (forward-sexp))

(defun stripwhite (str)
  "Remove any whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "[ \t\n]*" "" s)))

