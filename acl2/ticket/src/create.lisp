;;; create.lisp - Ticket creation command for ticket-cli
;;;
;;; Implements the `create` command which generates a new ticket file
;;; with YAML frontmatter and markdown body.

(in-package :ticket)

;;; Ticket creation

(defstruct create-options
  "Options for ticket creation."
  (title "Untitled")
  (description nil)
  (design nil)
  (acceptance nil)
  (ticket-type "task")
  (priority 2)
  (assignee nil)
  (external-ref nil)
  (parent nil))

(defun parse-create-args (args)
  "Parse command line arguments for create command.
   Returns a create-options struct."
  (let ((options (make-create-options))
        (i 0))
    ;; Try to get default assignee from git
    (let ((git-user (get-git-user-name)))
      (when git-user
        (setf (create-options-assignee options) git-user)))
    
    (loop while (< i (length args))
          for arg = (nth i args)
          do (cond
               ;; Description
               ((or (string= arg "-d") (string= arg "--description"))
                (incf i)
                (when (< i (length args))
                  (setf (create-options-description options) (nth i args))))
               ;; Design
               ((string= arg "--design")
                (incf i)
                (when (< i (length args))
                  (setf (create-options-design options) (nth i args))))
               ;; Acceptance
               ((string= arg "--acceptance")
                (incf i)
                (when (< i (length args))
                  (setf (create-options-acceptance options) (nth i args))))
               ;; Type
               ((or (string= arg "-t") (string= arg "--type"))
                (incf i)
                (when (< i (length args))
                  (setf (create-options-ticket-type options) (nth i args))))
               ;; Priority
               ((or (string= arg "-p") (string= arg "--priority"))
                (incf i)
                (when (< i (length args))
                  (setf (create-options-priority options)
                        (parse-integer (nth i args) :junk-allowed t))))
               ;; Assignee
               ((or (string= arg "-a") (string= arg "--assignee"))
                (incf i)
                (when (< i (length args))
                  (setf (create-options-assignee options) (nth i args))))
               ;; External ref
               ((string= arg "--external-ref")
                (incf i)
                (when (< i (length args))
                  (setf (create-options-external-ref options) (nth i args))))
               ;; Parent
               ((string= arg "--parent")
                (incf i)
                (when (< i (length args))
                  (setf (create-options-parent options) (nth i args))))
               ;; Unknown flag - skip
               ((string-starts-with arg "-")
                nil)
               ;; Positional argument - title
               (t
                (setf (create-options-title options) arg)))
             (incf i))
    options))

(defun generate-ticket-content (id options)
  "Generate the full ticket file content as a string."
  (with-output-to-string (s)
    ;; YAML frontmatter
    (format s "---~%")
    (format s "id: ~A~%" id)
    (format s "status: open~%")
    (format s "deps: []~%")
    (format s "links: []~%")
    (format s "created: ~A~%" (iso-timestamp))
    (format s "type: ~A~%" (create-options-ticket-type options))
    (format s "priority: ~A~%" (create-options-priority options))
    (when (create-options-assignee options)
      (format s "assignee: ~A~%" (create-options-assignee options)))
    (when (create-options-external-ref options)
      (format s "external-ref: ~A~%" (create-options-external-ref options)))
    (when (create-options-parent options)
      (format s "parent: ~A~%" (create-options-parent options)))
    (format s "---~%")
    
    ;; Title
    (format s "# ~A~%" (create-options-title options))
    (format s "~%")
    
    ;; Description
    (when (create-options-description options)
      (format s "~A~%" (create-options-description options))
      (format s "~%"))
    
    ;; Design section
    (when (create-options-design options)
      (format s "## Design~%")
      (format s "~%")
      (format s "~A~%" (create-options-design options))
      (format s "~%"))
    
    ;; Acceptance criteria section
    (when (create-options-acceptance options)
      (format s "## Acceptance Criteria~%")
      (format s "~%")
      (format s "~A~%" (create-options-acceptance options))
      (format s "~%"))))

(defun cmd-create (args)
  "Execute the create command.
   Creates a new ticket file and prints the generated ID.
   Returns 0 on success, non-zero on failure."
  (handler-case
      (progn
        ;; Ensure tickets directory exists
        (ensure-tickets-dir)
        
        ;; Parse arguments
        (let* ((options (parse-create-args args))
               (id (generate-id))
               (content (generate-ticket-content id options))
               (file-path (ticket-file-path id)))
          
          ;; Write the ticket file
          (write-file-contents file-path content)
          
          ;; Print the ID
          (format t "~A~%" id)
          0))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      1)))
