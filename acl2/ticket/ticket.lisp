; ticket.lisp - ACL2 implementation of ticket create command
; This file is designed to be certified by ACL2
;
; To certify: (certify-book "ticket")
; To use: (include-book "ticket")

(in-package "ACL2")

;;; ============================================================
;;; Pure functions (admitted by ACL2 in :logic mode)
;;; ============================================================

; Convert character to lowercase
(defun char-downcase-simple (c)
  (if (and (characterp c)
           (char>= c #\A) 
           (char<= c #\Z))
      (code-char (+ (char-code c) 32))
      c))

; Map char-downcase over list of chars
(defun downcase-chars (chars)
  (if (endp chars)
      nil
      (cons (char-downcase-simple (car chars))
            (downcase-chars (cdr chars)))))

; Downcase a string (no guard verification)
(defun string-downcase-simple (s)
  (declare (xargs :verify-guards nil))
  (coerce (downcase-chars (coerce s 'list)) 'string))

; Split string by delimiter into list of strings
(defun split-by-char (chars delim acc result)
  (declare (xargs :verify-guards nil))
  (cond ((endp chars)
         (if (endp acc)
             (reverse result)
             (reverse (cons (coerce (reverse acc) 'string) result))))
        ((eql (car chars) delim)
         (if (endp acc)
             (split-by-char (cdr chars) delim nil result)
             (split-by-char (cdr chars) delim nil
                            (cons (coerce (reverse acc) 'string) result))))
        (t (split-by-char (cdr chars) delim (cons (car chars) acc) result))))

(defun split-string (s delim)
  (declare (xargs :verify-guards nil))
  (split-by-char (coerce s 'list) delim nil nil))

; Get first character of each non-empty string in list
(defun first-chars (strings)
  (declare (xargs :verify-guards nil))
  (if (endp strings)
      nil
      (let ((s (car strings)))
        (if (and (stringp s) (> (length s) 0))
            (cons (char s 0) (first-chars (cdr strings)))
            (first-chars (cdr strings))))))

; Extract prefix from directory name
(defun extract-prefix (dir-name)
  (declare (xargs :verify-guards nil))
  (let* ((s1 (substitute #\Space #\- dir-name))
         (s2 (substitute #\Space #\_ s1))
         (segments (split-string s2 #\Space))
         (chars (first-chars segments)))
    (if (> (len chars) 1)
        (string-downcase-simple (coerce chars 'string))
        (if (>= (length dir-name) 3)
            (string-downcase-simple (subseq dir-name 0 3))
            (string-downcase-simple dir-name)))))

; Build YAML frontmatter as list of strings
(defun build-frontmatter (id title ticket-type priority timestamp)
  (declare (xargs :verify-guards nil))
  (list "---"
        (concatenate 'string "id: " id)
        "status: open"
        "deps: []"
        "links: []"
        (concatenate 'string "created: " timestamp)
        (concatenate 'string "type: " ticket-type)
        (concatenate 'string "priority: " 
                     (coerce (list (code-char (+ 48 (mod priority 10)))) 'string))
        "---"
        (concatenate 'string "# " title)
        ""))

; Join strings with newlines
(defun join-with-newlines (strings)
  (declare (xargs :verify-guards nil))
  (if (endp strings)
      ""
      (if (endp (cdr strings))
          (car strings)
          (concatenate 'string (car strings) 
                       (coerce '(#\Newline) 'string)
                       (join-with-newlines (cdr strings))))))

; Build full ticket content
(defun build-ticket-content (id title ticket-type priority timestamp)
  (declare (xargs :verify-guards nil))
  (join-with-newlines (build-frontmatter id title ticket-type priority timestamp)))


;;; ============================================================
;;; I/O functions (program mode - not proved, but ACL2 admits them)
;;; ============================================================

(program)

; Write string to file
(defun write-ticket-file (filename content state)
  (declare (xargs :stobjs state))
  (mv-let (channel state)
          (open-output-channel filename :character state)
          (if channel
              (let ((state (princ$ content channel state)))
                (let ((state (close-output-channel channel state)))
                  (mv nil state)))
              (mv "Failed to open file" state))))

; Get current timestamp (simplified - uses state)
(defun get-timestamp (state)
  (declare (xargs :stobjs state))
  (mv-let (d state)
          (read-run-time state)
          (declare (ignore d))
          ; For simplicity, return a fixed format - real impl would compute from time
          (mv "2026-01-14T00:00:00Z" state)))

; Main create function
(defun ticket-create (dir-name title ticket-type priority state)
  (declare (xargs :stobjs state))
  (let ((prefix (extract-prefix dir-name)))
    (mv-let (timestamp state)
            (get-timestamp state)
            ; Generate a simple ID (in real impl, would hash PID+time)
            (let* ((id (concatenate 'string prefix "-0001"))
                   (content (build-ticket-content id title ticket-type priority timestamp))
                   (filename (concatenate 'string ".tickets/" id ".md")))
              (mv-let (err state)
                      (write-ticket-file filename content state)
                      (if err
                          (mv err state)
                          (mv id state)))))))
