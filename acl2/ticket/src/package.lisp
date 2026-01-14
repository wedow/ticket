;;; package.lisp - Package definition for ticket-cli
;;;
;;; Defines the TICKET package with exported symbols.

(defpackage :ticket
  (:use :common-lisp)
  (:export
   ;; Main entry point
   #:main
   
   ;; Commands
   #:cmd-create
   #:cmd-show
   #:cmd-status
   #:cmd-ls
   
   ;; Utilities
   #:generate-id
   #:iso-timestamp
   #:ensure-tickets-dir
   #:ticket-path
   
   ;; Constants
   #:+tickets-dir+))

(in-package :ticket)
