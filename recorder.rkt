#lang racket/base

(require racket/pretty racket/match db)

(struct record ([timestamp #:auto]) #:transparent #:auto-value "now")

(struct diaper record (type) #:transparent)
(struct breast-feed record (event side) #:transparent)
(struct bottle-feed record (event quantity) #:transparent)

(define (record-persist! rec)
  (match rec
    [(diaper _ type) (query-exec (fetch-db-instance) "insert into diaper_log (type) values ($1)" type)]
    [(breast-feed _ event side) (query-exec (fetch-db-instance) "insert into breastfeed_log (event, side) values ($1, $2)" event side)]
    [(bottle-feed _ event quantity) (query-exec (fetch-db-instance) "insert into bottlefeed_log (event, quantity) values ($1, $2)" event quantity)]))

(define (init-db!)
  (let ([dbh (fetch-db-instance)])
    (query-exec dbh
                "create table diaper_log (id integer primary key asc autoincrement, timestamp default CURRENT_TIMESTAMP, type TEXT)")
    (query-exec dbh
                "create table breastfeed_log (id integer primary key asc autoincrement, timestamp default CURRENT_TIMESTAMP, event TEXT, side TEXT)")
    (query-exec dbh
                "create table bottlefeed_log (id integer primary key asc autoincrement, timestamp default CURRENT_TIMESTAMP, event TEXT, quantity NUMERIC)")))

(define *db-instance* null)
(define (fetch-db-instance)
  (when (null? *db-instance*)
    (set! *db-instance* (new-db-conn!)))
  *db-instance*)

(define (new-db-conn!)
  (displayln "Creating a new db connection" (current-error-port))
  (sqlite3-connect #:database "./baby.db" #:mode 'create))
