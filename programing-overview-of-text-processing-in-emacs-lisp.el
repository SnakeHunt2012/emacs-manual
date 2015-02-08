;; Cursor Position
; Current cursor position is called "point".
; The first char in buffer is 1
(point)            ; returns the current cursor position

(region-beginning) ; returns the position of the beginning of region
(region-end)       ; returns the position of the end of region

(line-beginning-position) ; returns the position of beginning of current line
(line-end-position)       ; returns the position of end of current line

(point-min) ; returns the position for the beginning of buffer, taking account of narrow-to-region
(point-max) ; returns the position for the end of buffer, taking account of narrow-to-region
;======================================================================


;; Moving Cursor and Searching
(goto-char 392) ; move cursor to position 392

(forward-char n)  ; move cursor forward by n chars
(backward-char n) ; move cursor backward by n chars

; move cursor to the location of my-str
; returns the new position
(search-forward my-str) ; end of my-str
(search-backward my-str) ; beginning of my-str

;; move cursor to the location matched by a regex
;; returns the new position
(re-search-forward myRegex)
(re-search-backward myRegex)

;; move cursor to the first char that's not "a to z"
;; returns the distance traveled.
(skip-chars-forward "a-z")
(skip-chars-backward "a-z")
;======================================================================


;; Deleting/Inserting/Changing Text
; delete 9 chars starting at current cursor pos
(delete-char 9)

; deleting text
(delete-region my-start-pos my-end-pos)

; insert string at current cursor position
(insert "hi i love u.")

; get the string from buffer
(setq my-str (buffer-substring my-start-pos my-end-pos))

; change case
(capitalize-region my-start-pos my-end-pos)
;======================================================================


;; Strings
; length
(length "abc") ; returns 3

; gets a substring
(substring my-str start-index end-index)

; change a given string using regex
(replace-regexp-in-string my-regex my-replacement my-str)
;======================================================================


;; Buffers
; return the name of current buffer
(buffer-name)

; return the full path of current file
(buffer-file-name)

; switch to the buffer named myBufferName
(set-buffer my-buffer-name)

; save current buffer
(save-buffer)

; close a buffer
(kill-buffer my-buff-name)

; close the current buffer
(kill-this-buffer)

; temporarily sets a buffer as current to work with
(with-current-buffer my-buffer
  ; do something here ...
)
;======================================================================


;; Files
; open a file (in a buffer)
(find-file my-path)

; close current buffer and open the new saved, same as "Save As".
(write-file my-path)

; insert file into current position
(insert-file-contents my-path)

; append a text block to file
(append-to-file my-start-pos my-end-pos my-path)

; renaming file
(rename-file file-name new-name)

; copying file
(copy-file old-name new-name)

; deleting file
(delete-file file-name)

; get dir path
(file-name-directory my-full-path)

; get filename part
(file-name-nondirectory my-full-path)

; get filename's suffix
(file-name-extension my-file-name)

; get filename sans suffix
(file-name-sans-extension my-file-name)
;======================================================================


;; A Simple Example
; This code illustrates how to insert a string, then position cursor somewhere inside.
(defun insert-p-tag ()
  "Insert <p></p> at cursor point."
  (interactive)
  (insert "<p></p>")
  (backward-char 4))
; Type the above, then select the whole code, call eval-region [Alt+x].
; To execute the command, call "insert-p-tag".
;======================================================================
