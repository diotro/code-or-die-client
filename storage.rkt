#lang racket

(provide
 current-mongo

 ;; String -> Void
 clear-storage!
 
 ;; String BSONDocument -> Void
 store!

 ;; String BSONDocument -> BSONDocument
 find-stored

 find-single-stored)


(require db/mongodb)
(module+ test
  (require rackunit))



(define current-mongo (make-parameter (list "localhost" 27017 "code-or-die-test")
                                      (λ (mongo)
                                        (unless (and (list? mongo)
                                                     (= (length mongo) 3)
                                                     (string? (first mongo))
                                                     (integer? (second mongo))
                                                     (string? (third mongo)))
                                          (raise-argument-error 'mongo
                                                                "list of hostname, port, and db name"
                                                                mongo))
                                        mongo)))


(define (clear-storage!)
  (call-with-mongo! mongo-db-drop))

;; store! : String BSONDocument -> Void
;; stores the given BSONDocument in the given collection
(define (store! collection value)
  (when value
    (call-with-mongo-collection
     collection
     (λ (mongo) (mongo-collection-repsert! mongo
                                           (hash 'id (hash-ref value 'id))
                                           value)))))


;; find-stored : String BSONDocument -> [Listof BSONDocument]
(define (find-stored collection query)
  (call-with-mongo-collection
   collection
   (λ (coll) (mongo-collection-find coll query
                                    ; don't include _id field
                                    #:selector (hash '_id 0)))))


;; find-single-stored : String BSONDocument -> [Maybe BSONDocument]
(define (find-single-stored collection query)
  (define result (find-stored collection query))
  (if (cons? result)
      (first result)
      #f))


(module+ test
  (current-mongo '("localhost" 27017 "test-test-test-test-test"))
  #;(clear-storage!)
  
  (define TEST-COL "test-for-code-or-die")
  (sleep 1)
  (store! TEST-COL (hasheq 'id 1 'val 3))
  (check-equal? (hash-ref (find-single-stored TEST-COL (hash 'val 3)) 'id) 1)

  (store! TEST-COL (hasheq 'id 2 'val 3))
  (check-equal? (length (find-stored TEST-COL (hash 'val 3))) 2)
  
  (call-with-mongo-collection! TEST-COL mongo-collection-drop!))

;;---------------------------------------------------------------------------------------------------
;; Helper Functions

;; call-with-mongo : [X] [MongoDB -> X] -> X
;; calls the given function on the current mongo db
(define (call-with-mongo f)
  (define mongo (create-mongo #:host (first (current-mongo))
                              #:port (second (current-mongo))))
  (define db (mongo-db mongo (third (current-mongo))))
  
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

