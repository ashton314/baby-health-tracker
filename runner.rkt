#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline racket/port racket/match)
(require "recorder.rkt")

(init-db!)

(define the-type (make-parameter "diaper"))
(define the-subtype (make-parameter "wet"))
(define ask-notes? (make-parameter #t))
(define the-notes (make-parameter ""))

(struct command (action type subtype notes) #:transparent)

(define args
  (command-line
   #:once-each
   [("-n" "--no-notes") "Do not record notes" (ask-notes? #f)]
   [("-t" "--record-type") tp
                           "Record type: must be either <diaper>, <breastfeed>, or <bottlefeed>"
                           (the-type tp)]
   [("-s" "--sub-type") st
                        "Subtype: for type <diaper>, this should be <wet> or <dirty>; for feedings, should be <start> or <stop>"
                        (the-subtype st)]
   #:args ()
   (command 'record (the-type) (the-subtype) "")))


(when (ask-notes?)
  (displayln "Notes:" (current-error-port))
  (the-notes (port->string (current-input-port))))

(match args
  [(command 'record type subtype notes)
   (match type
     ["diaper" (match subtype ["wet" (record-wet (the-notes))] ["dirty" (record-dirty (the-notes))])]
     ["breastfeed" (match subtype ["start" (record-breastfeed-start (the-notes))] ["stop" (record-breastfeed-stop (the-notes))])]
     ["bottlefeed" (match subtype ["start" (record-bottlefeed-start (the-notes))] ["stop" (record-bottlefeed-stop (the-notes))])])])

