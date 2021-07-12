#lang racket/base

(require racket/pretty racket/match db)

(struct record (notes [id #:auto] [timestamp #:auto]) #:transparent #:auto-value null)

(struct diaper record (type) #:transparent)
(struct breast-feed record (event side) #:transparent)
(struct bottle-feed record (event quantity) #:transparent)

(define (record-persist! rec)
  (match rec
    [(diaper notes _ _ type) (query-exec (fetch-db-instance) "insert into diaper_log (type, notes) values ($1, $2)" type notes)]
    [(breast-feed notes _ _ event side) (query-exec (fetch-db-instance) "insert into breastfeed_log (event, side, notes) values ($1, $2, $3)" event side notes)]
    [(bottle-feed notes _ _ event quantity) (query-exec (fetch-db-instance) "insert into bottlefeed_log (event, quantity, notes) values ($1, $2, $3)" event quantity notes)]))

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

;;; Convenience functions
(define (record-dirty! [notes ""]) (record-persist! (diaper notes "dirty")))
(define (record-wet! [notes ""]) (record-persist! (diaper notes "wet")))
(define (record-breastfeed-start! side [notes ""]) (record-persist! (breast-feed notes "start" side)))
(define (record-breastfeed-stop! side [notes ""]) (record-persist! (breast-feed notes "stop" side)))
(define (record-bottlefeed-start! quantity [notes ""]) (record-persist! (bottle-feed notes "start" quantity)))
(define (record-bottlefeed-stop! quantity [notes ""]) (record-persist! (bottle-feed notes "stop" quantity)))
