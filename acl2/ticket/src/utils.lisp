;;; utils.lisp - Utility functions for ticket-cli
;;;
;;; Contains helper functions for ID generation, timestamps, file operations.
;;; Written in Common Lisp with ACL2-compatible style where possible.

(in-package :ticket)

;;; Constants
(defconstant +tickets-dir+ ".tickets")

;;; String utilities

(defun split-string (string delimiter)
  "Split STRING by DELIMITER character, returning list of substrings."
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) delimiter)
          do (when (> i start)
               (push (subseq string start i) result))
             (setf start (1+ i)))
    (when (< start (length string))
      (push (subseq string start) result))
    (nreverse result)))

(defun string-starts-with (string prefix)
  "Check if STRING starts with PREFIX."
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

(defun trim-whitespace (string)
  "Remove leading and trailing whitespace from STRING."
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

;;; SHA256 hashing

(defun sha256-via-shell (string)
  "Compute SHA256 hash of STRING using shell command.
   Returns hex digest as lowercase string."
  (let* ((cmd (format nil "printf '%s' '~A' | sha256sum 2>/dev/null || printf '%s' '~A' | shasum -a 256"
                      string string))
         (output (with-output-to-string (s)
                   ;; Use uiop for portable process execution
                   (let ((process (sb-ext:run-program "/bin/sh"
                                                      (list "-c" cmd)
                                                      :output s
                                                      :error nil
                                                      :wait t)))
                     (declare (ignore process)))))
         (hash-line (trim-whitespace output)))
    ;; SHA256 output is "hash  -" or "hash  filename", take first 64 chars
    (subseq hash-line 0 (min 64 (or (position #\Space hash-line) 64)))))

(defun sha256-hex (string)
  "Return SHA256 hex digest of STRING."
  (sha256-via-shell string))

;;; Timestamp utilities

(defun iso-timestamp ()
  "Return current UTC time in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0) ; 0 = UTC
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour minute second)))

;;; Directory utilities

(defun get-current-directory-name ()
  "Get the name of the current working directory."
  (let* ((cwd (truename "."))
         (dir-list (pathname-directory cwd)))
    (if (and dir-list (listp dir-list))
        (let ((last-component (car (last dir-list))))
          (if (stringp last-component)
              last-component
              ;; Handle case where directory component might not be string
              (format nil "~A" last-component)))
        "unknown")))

(defun extract-directory-prefix (dir-name)
  "Extract prefix from directory name by taking first letter of each segment.
   Segments are split on - or _. Falls back to first 3 chars if no segments."
  (let* ((normalized (substitute #\Space #\_ (substitute #\Space #\- dir-name)))
         (segments (split-string normalized #\Space))
         (filtered (remove-if (lambda (s) (zerop (length s))) segments)))
    (cond
      ;; Multiple segments: take first letter of each
      ((> (length filtered) 1)
       (coerce (mapcar (lambda (s) (char s 0)) filtered) 'string))
      ;; Single segment or empty: take first 3 chars
      ((and filtered (first filtered))
       (let ((s (first filtered)))
         (subseq s 0 (min 3 (length s)))))
      ;; Fallback
      (t (subseq dir-name 0 (min 3 (length dir-name)))))))

;;; ID generation

(defun generate-id ()
  "Generate ticket ID from directory prefix + timestamp hash.
   Format: {prefix}-{4-char-hash}"
  (let* ((dir-name (get-current-directory-name))
         (prefix (string-downcase (extract-directory-prefix dir-name)))
         ;; Generate entropy from PID and timestamp
         (pid #+sbcl (sb-posix:getpid)
              #-sbcl 0)
         (timestamp (get-universal-time))
         (entropy (format nil "~D~D" pid timestamp))
         (hash (sha256-hex entropy))
         (short-hash (subseq hash 0 4)))
    (format nil "~A-~A" prefix short-hash)))

;;; File system utilities

(defun ensure-tickets-dir ()
  "Ensure the .tickets directory exists."
  (ensure-directories-exist (format nil "~A/" +tickets-dir+)))

(defun ticket-file-path (ticket-id)
  "Return the file path for a ticket with given ID."
  (format nil "~A/~A.md" +tickets-dir+ ticket-id))

(defun file-exists-p* (path)
  "Check if file exists at PATH."
  (probe-file path))

(defun read-file-contents (path)
  "Read entire file contents as string."
  (with-open-file (stream path :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun write-file-contents (path contents)
  "Write CONTENTS string to file at PATH."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

;;; Git utilities

(defun get-git-user-name ()
  "Get git config user.name, or NIL if not available."
  (handler-case
      (let ((output (with-output-to-string (s)
                      (sb-ext:run-program "git"
                                          '("config" "user.name")
                                          :output s
                                          :error nil
                                          :wait t
                                          :search t))))
        (let ((trimmed (trim-whitespace output)))
          (if (plusp (length trimmed))
              trimmed
              nil)))
    (error () nil)))
