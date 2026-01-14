;;; ticket.asd - ASDF system definition for ticket-cli
;;;
;;; This defines the Common Lisp system for the ticket-cli ACL2 port.
;;; While aiming for ACL2 compatibility, we use standard CL for I/O.

(defsystem "ticket"
  :description "A minimal ticket system with dependency tracking"
  :version "0.1.0"
  :author "ticket-cli contributors"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:file "src/package")
               (:file "src/utils")
               (:file "src/create")
               (:file "src/cli")))
