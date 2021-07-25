#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline racket/port racket/match)
(require "recorder.rkt")

(init-db!)

(define the-type (make-parameter "diaper"))
(define the-subtype (make-parameter "wet"))
(define the-subtype-datum (make-parameter ""))
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
                        "Subtype: for type <diaper>, this should be <wet> or <dirty>; for feedings, should be <start>, <pause>, <resume>, or <end>"
                        (the-subtype st)]
   [("-d" "--datum") td
                     "Subtype Datum: for breastfeeding, this is the side (<left> or <right>); for bottle feeding, this is the quantity"
                     (the-subtype-datum td)]
   #:args ()
   (command 'record (the-type) (the-subtype) "")))


(when (ask-notes?)
  (displayln "Notes:" (current-error-port))
  (the-notes (port->string (current-input-port))))

(match args
  [(command 'record type subtype notes)
   (match type
     ["diaper" (match subtype ["wet" (record-wet (the-notes))] ["dirty" (record-dirty (the-notes))])]
     ["breastfeed" (match subtype
                     ["start" (record-breast-feed-start (the-subtype-datum) (the-notes))]
                     ["pause" (record-breast-feed-pause (the-notes))]
                     ["resume" (record-breast-feed-resume (the-subtype-datum) (the-notes))]
                     ["end" (record-breast-feed-end (the-notes))])]
     ["bottlefeed" (match subtype
                     ["start" (record-bottle-feed-start (the-notes))]
                     ["pause" (record-bottle-feed-pause (the-subtype-datum) (the-notes))]
                     ["resume" (record-bottle-feed-resume (the-notes))]
                     ["end" (record-bottle-feed-end (the-subtype-datum) (the-notes))])])])

