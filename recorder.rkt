#lang racket/base

(require (for-syntax racket/syntax racket/base))
(require racket/match racket/date db)
(provide (all-defined-out))

(date-display-format 'iso-8601)
(displayln (date->string (seconds->date (current-seconds)) #t) (current-error-port))

(struct record (notes [id #:auto] [timestamp #:auto]) #:transparent #:auto-value null)

(struct diaper record (type) #:transparent)
(struct breast-feed record (event side) #:transparent)
(struct bottle-feed record (event quantity) #:transparent)

(define (record-persist! rec)
  (match rec
    [(diaper notes _ _ type) (query-exec (fetch-db-instance) "insert into diaper_log (type, notes) values ($1, $2)" type notes)]
    [(breast-feed notes _ _ event side) (query-exec (fetch-db-instance) "insert into breastfeed_log (event, side, notes) values ($1, $2, $3)" event side notes)]
    [(bottle-feed notes _ _ event quantity) (query-exec (fetch-db-instance) "insert into bottlefeed_log (event, quantity, notes) values ($1, $2, $3)" event quantity notes)])
  (displayln "recorded"))

(define (init-db!)
  (let ([dbh (fetch-db-instance)])
    (query-exec dbh
                "create table if not exists diaper_log (id integer primary key asc autoincrement, timestamp default CURRENT_TIMESTAMP, type TEXT, notes TEXT)")
    (query-exec dbh
                "create table if not exists breastfeed_log (id integer primary key asc autoincrement, timestamp default CURRENT_TIMESTAMP, event TEXT, side TEXT, notes TEXT)")
    (query-exec dbh
                "create table if not exists bottlefeed_log (id integer primary key asc autoincrement, timestamp default CURRENT_TIMESTAMP, event TEXT, quantity NUMERIC, notes TEXT)")))

(define *db-instance* null)
(define (fetch-db-instance)
  (when (null? *db-instance*)
    (set! *db-instance* (new-db-conn!)))
  *db-instance*)

(define (new-db-conn!)
  (displayln "Creating a new db connection" (current-error-port))
  (sqlite3-connect #:database "./baby.db" #:mode 'create))

(define-syntax (feed-event stx)
  (syntax-case stx ()
    [(_ feed-type event-name)
     (with-syntax ([func-name (format-id #'feed-type "record-~a-~a" #'feed-type #'event-name)]
                   [event-arg (symbol->string (syntax->datum #'event-name))])
       #'(define (func-name [arg ""] [notes ""])
           (record-persist! (feed-type notes event-arg arg))))]))

;;; Convenience functions
(define (record-dirty [notes ""]) (record-persist! (diaper notes "dirty")))
(define (record-wet [notes ""]) (record-persist! (diaper notes "wet")))

(feed-event breast-feed start)
(feed-event breast-feed pause)
(feed-event breast-feed resume)
(feed-event breast-feed end)

(feed-event bottle-feed start)
(feed-event bottle-feed pause)
(feed-event bottle-feed resume)
(feed-event bottle-feed end)
