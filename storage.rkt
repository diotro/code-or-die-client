#lang racket

(provide
 ;; current-mongo : [Parameter (list String Number String)]
 current-mongo

 ;; clear-storage : -> Void
 clear-storage!
 
 ;; (register-collection id pk [id ....])
 ;; registers the first collection, with accompanying functions:
 ;; "id"->db
 ;; db->"id"
 ;; db->single-"id"
 ;; For each key:
 ;;  - id->retrieve-"key"
 ;;  - id->retrieve-single-"key"
 register-collection)


(require db/mongodb)
(module+ test
  (require rackunit))


;; [Parameter (list String Number String)]
(define current-mongo
  (make-parameter (list "localhost" 27017 "code-or-die-test")
                  (λ (mongo)
                    (unless (and (list? mongo)
                                 (= (length mongo) 3)
                                 (string? (first mongo))
                                 (integer? (second mongo))
                                 (string? (third mongo)))
                      (raise-argument-error
                       'mongo "(list hostname port db-name)" mongo))
                    (current-mongo-conn
                     (create-mongo #:host (first (current-mongo))
                                   #:port (second (current-mongo))))
                  mongo)
                  ))

(define current-mongo-conn
  (make-parameter (create-mongo #:host (first (current-mongo))
                                #:port (second (current-mongo))) identity))



(define-syntax (register-collection stx)
  (syntax-case stx ()
    [(_ coll pk [keys ...])
     (let ([make-id (λ (template . ids)
                      (let ([template (apply format template (map syntax->datum ids))])
                        (datum->syntax stx (string->symbol template))))])
       (with-syntax ([c (symbol->string (syntax->datum #'coll))]
                     [id->db (make-id "~a->db" #'coll)]
                     [db->id (make-id "db->~a" #'coll)]
                     [db->single-id (make-id "db->single-~a" #'coll)]
                     )
         #`(begin (define (id->db value) (store! c #'pk value))
                  (define (db->id)
                    (apply values (sequence->list (find-stored c))))
                  (define (db->single-id id) (find-single-stored c id))
                  #,@(map (λ (x) #`(define #,(make-id "~a->retrieve-~a" x #'coll)
                                     (λ (v) (apply values
                                                   (sequence->list
                                                    (find-stored-by c '#,x v))))))
                          (cons #'pk (syntax->list #'(keys ...))))
                  #,@(map (λ (x) #`(define #,(make-id "~a->retrieve-single-~a" x #'coll)
                                     (curry find-single-stored-by c '#,x)))
                          (cons #'pk (syntax->list #'(keys ...))))
                  )))]))



(module+ test
  (current-mongo (list "localhost" 27017 "testtestasdfaksjdl"))
  (clear-storage!)
  (register-collection c id [])
  (c->db (hash 'id 3))
  (check-equal? (first (call-with-values db->c list)) (hash 'id 3))
  (define c1 (id->retrieve-c 3))
  (check-equal? c1 (hash 'id 3)))

;; -> Void
;; clears the current database
(define (clear-storage!)
  (call-with-mongo! mongo-db-drop))



;; store! : String Symbol BSONDocument -> Void
;; stores the given BSONDocument in the given collection
(define (store! collection pk value)
  (call-with-mongo-collection
   collection
   (λ (mongo)
     (if (hash-has-key? value pk)
         (mongo-collection-repsert! mongo (hash pk (hash-ref value pk)) value)
         (mongo-collection-insert! mongo value)))))


;; find-stored : String BSONDocument -> [Listof JSExpr]
(define (find-stored collection [query (hash)])
  (define found
    (call-with-mongo-collection
     collection
     (λ (coll)
       (with-handlers ([exn:fail? (λ (x) empty-stream)])
         (mongo-collection-find coll query
                                ; don't include _id field
                                #:selector (hash '_id 0))))))
  
  (define (hash-vec->list h)
    (for/fold ([out (hash)])
              ([(key value) (in-hash h)])
      (define l-value
        (cond [(vector? value) (vector->list value)]
              [(hash? value) (hash-vec->list value)]
              [else value]))
      (hash-set out key l-value)))
  
  (sequence-map hash-vec->list found))


;; find-single-stored : String BSONDocument -> [Maybe BSONDocument]
(define (find-single-stored collection [query (hash)])
  (define result (find-stored collection query))
  (if (stream-empty? result) #f (stream-first result)))


;; find-by : String Symbol JSExpr -> [Listof BSONDocument]
(define (find-stored-by collection key value)
  (find-stored collection (hash key value)))


;; find-single-by : String Symbol JSExpr -> [Maybe BSONDocument]
(define (find-single-stored-by collection key value)
  (find-single-stored collection (hash key value)))


(module+ test
  (current-mongo '("localhost" 27017 "test-test-test-test-test"))
  (clear-storage!)
  
  (define TEST-COL "test-for-code-or-die")
  (store! TEST-COL 'id (hasheq 'id 1 'val 3))
  (sleep .3)
  (check-equal? (stream-length (find-stored TEST-COL (hash 'val 3))) 1)
  (check-true (hash? (find-single-stored TEST-COL (hash 'val 3))))

  (store! TEST-COL 'id  (hasheq 'id 2 'val 3))
  (check-equal? (stream-length (find-stored TEST-COL (hash 'val 3))) 2)
  
  (call-with-mongo-collection! TEST-COL mongo-collection-drop!))


;;---------------------------------------------------------------------------------------------------
;; Helper Functions

;; call-with-mongo : [X] [MongoDB -> X] -> X
;; calls the given function on the current mongo db
(define (call-with-mongo f)
  (define db (mongo-db (current-mongo-conn) (third (current-mongo))))
  
  (define out-cursor (f db))
  (cond [(void? out-cursor) (void)]
        [(mongo-cursor? out-cursor)
         (sequence->list out-cursor)]
        [else out-cursor]))


;; call-with-mongo! : [MongoDB -> Any] -> Void
;; calls the given function on the current mongo db for it's effect
(define (call-with-mongo! f)
  (call-with-mongo (λ (mongo) (void (f mongo)))))


;; call-with-mongo-collection : [X] String [MongoCollection -> X] -> X
;; calls the given function on the given collection in the current mongo db
(define (call-with-mongo-collection collection f)
  (call-with-mongo (λ (mongo) (f (mongo-collection mongo collection)))))


;; call-with-mongo-collection! : String [MongoCollection -> Any] -> Void
;; calls the given function on the given collection in the current mongo db for effect
(define (call-with-mongo-collection! collection f)
  (call-with-mongo-collection collection (λ (mongo) (void (f mongo)))))

