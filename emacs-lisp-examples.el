;; To see a function's documentation, call describe-function [F1 f].
;; To see a  variable's documentation, call describe-variable [F1 v].

;; Insert Text
; This code shows how to insert a string, and also position cursor after the insertion.
(defun insert-p-tag ()
  "Insert <p></p> at cursor point."
  (interactive)
  (insert "<p></p>")
  (backward-char 4))
;======================================================================


;; Insert Around Region
; This code shows how to place a string at the beginning and end of a region.
(defun wrap-markup-region (begin end)
  "Insert a markup <b></b> around a region."
  (interactive "r")
  (goto-char end) (insert "</b>")
  (goto-char start) (insert "<b>"))
;======================================================================


;; Select Current Word, Select Current Line
; This code shows you how to set a mark (select text) programmatically.
(transient-mark-mode 1)
(defun select-current-word ()
  (interactive)
  (let (pt)
    (skip-chars-backward "_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "_A-Za-z0-9")
    (set-mark pt)))
;======================================================================


;; Find/Replace Text Region
; This code illustrates how to do text replacements on a region. Very useful.
(defun replace-greek-region (start end)
  "Replace ¡°alpha¡± to ¡°¦Á¡± and other greek letters in current region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)

    (goto-char (point-min))
    (while (search-forward "alpha" nil t) (replace-match "¦Á" nil t))

    (goto-char (point-min))
    (while (search-forward "beta" nil t) (replace-match "¦Â" nil t))

    (goto-char (point-min))
    (while (search-forward "gamma" nil t) (replace-match "¦Ã" nil t))))
;======================================================================


;; Delete Enclosed Text
; This code illustrates how to delete a text enclosed by any pairs of delimiters.
(defun delete-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^([<>") (setq p1 (point))
      (skip-chars-forward "^)]<>") (setq p2 (point))
      (delete-region p1 p2))))
;======================================================================


;; Delete Linebreaks
; This example shows how to temporarily change a pre-defined variable's value, then call a function whose behavior depends on the var.
(defun remove-line-breaks ()
  "Remove line endings in current paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;======================================================================


;; Inserting a Random Number
(defun insert-a-random-number ()
  "Insert a random number between 0 to 999999."
  (interactive)
  (insert (number-to-string (random 999999))))
;======================================================================
