#lang racket
(provide (all-defined-out))

(require "api-commands.rkt"
         "storage.rkt"
         "messages.rkt"
         "shared.rkt")
(module+ test
  (require rackunit))

(provide clear-storage!
         current-mongo)


;; ---------------------------------------------------------------------------------------------------
;; Higher Order operations

;; ->+ : [X -> Y] [Y -> Z] [X Z -> R] -> [X -> R]
;; applies the first two functions to the input data to get a result.
;; combines the result with the original input
(define (->+ selector processor combinator)
  (copy-name processor
             (λ (input)
               (define out
                 (call-with-values (thunk (processor (selector input)))
                                   (λ v (map (λ (x) (combinator input x)) v))))
               (apply values out))))

(module+ test
  (define ADD1-LIST (->+ identity add1 list))
  (check-equal? (ADD1-LIST 3) (list 3 4))
  (check-equal? ((->+ second add1 (replace second)) '(1 2)) '(1 3)))


;; place-last : [Or Any [List-of Any]] Any -> [List-of Any]
;; adds the given element to the end of the given list
(define (place-last add-to last)
  (cond [(list? add-to) (append add-to (list last))]
        [else (list add-to last)]))


;; replace : [List-of X -> X] -> [ [List-of X] X => [List-of X] ]
;; replaces the given value with the value selected by selector
(define (replace selector)
  (λ (list input)
    (define replace (selector list))
    (define-values [before after] (splitf-at list (λ (x) (not (equal? x replace)))))
    `(,@before ,input ,@(rest after))))

(module+ test
  (check-equal? ((replace third) '(1 2 3 4 5) 7) '(1 2 7 4 5)))


;; parallel : Integer [X -> Y] : (list N [X -> Y])
;; uses n components in parallel to perform op
(define (parallel n op)
  (list n op))

;; fork : [List-of [X -> Void]] -> [X -> Void]
;; applies each handler to the given input
(define (fork . handlers)
  (define name (for/fold ([name "fork:"])
                         ([handler handlers])
                 (string-append name "/"
                                (symbol->string (object-name handler)))))
  (procedure-rename (λ (input) (for-each (λ (h) (h input)) handlers))
                    (string->symbol name)))


;; handle : [X -> Y] [List-of [Y -> Void]] -> [X -> Void]
;; produces a function that accepts an X, transforms it to a Y, then
;; applies each of the processors
(define (handle #:do processor . transformers)
  (define name (for/fold ([name (string-append "handle:"
                                               (symbol->string (object-name processor))
                                               ":")])
                         ([t transformers])
                 (string-append name "/"
                                (symbol->string (object-name t)))))
  (procedure-rename (λ (input)
                      (processor ((apply compose (reverse transformers)) input)))
                    (string->symbol name)))


;; every : Number [X -> Y] -> [X -> Y]
(define (every seconds func)
  (copy-name func (λ args (sleep seconds) (apply func args))))


;; ->channel : String -> [Any -> Void]
;; outputs the given data to the given channel
(define (->channel c)
  (procedure-rename (λ (x) (void (broadcast c x)))
                    (string->symbol (string-append "to-channel:" c))))

;; channel-> : String -> [-> Any]
;; reads input from the given channel
(define (channel-> c)
  (curry receive c))


;; ->filter-> : [X -> Y] -> [X -> [Or X (values)]
(define (->filter-> predicate?)
  (procedure-rename (λ (x) (if (predicate? x) x (values)))
                    (string->symbol (string-append "filter:"
                                                   (symbol->string (object-name predicate?))))))


;; ->log-> : String [X -> Y] -> [X -> Y]
;; logs the input to the given channel, then applies the function
(define (->log-> c [func identity])
  (procedure-rename (λ input
                      (map (curry broadcast c) input)
                      (apply func input))
                    (string->symbol (string-append "log:" c))))


;; procedure? procedure? -> procedure?
;; gives the second function the name of the first function
(define (copy-name f1 f2)
  (procedure-rename f2 (object-name f1)))

;; ---------------------------------------------------------------------------------------------------
;; Inputs

;; -> [List-of Integer]
;; What systems are visible?
(define (api->system-ids)
  (apply values (get-systems)))


;; -> [List-of Integer]
;; what ships do I own?
(define (api->ship-ids)
  (apply values (map (λ (h) (hash-ref h 'id)) (get-ships))))


;; ---------------------------------------------------------------------------------------------------
;; Intermediate steps

;; Integer -> System
;; finds all available informatino on the given system
(define (system-id->system-info system)
  (define detail (get-system-info system))
  (if (symbol? detail) (values) detail))

;; System -> Boolean
;; does the given system have less than ten orders?
(define (system-less-than-ten-orders? system)
  (and (hash? system) 
       #;(hash-has-key? system 'orders)
       #;(< (length (hash-ref system 'orders)) 10)
       ))

;; Integer -> Ship
;; finds all information on the given ship from the api
(define (ship-id->ship-info id)
  (get-ship id))


;; Integer -> [List-of ShipOrder]
;; what orders does the given ship have?
(define (ship-id->ship-orders id)
  (hash-ref (get-ship id) 'orders))

;; Ship -> Boolean
;; does the given ship have any orders?
(define (no-ship-orders? ship-info)
  ;; sometimes '() gets entered as #hash(), so check cons case
  (not (cons? ship-info)))

;; Ship -> System
;; finds information on the system the given ship is currently located at
(define (ship-info->system-info ship-info)
  (define s (id->retrieve-single-system-info (hash-ref ship-info 'location)))
  (if s s (values)))

;; System -> [List-of Route]
;; creates the routes for the given system
(define (system-info->routes system-info)
  (map (λ (h) (hash-set h 'origin (hash-ref system-info 'id)))
              (hash-ref system-info 'routes)))


;; [List-of Route] -> [List-of Route]
;; keeps only the routes not owned by me
(define (routes->routes-not-owned routes)
  (filter (λ (r)
            (define sys (id->retrieve-single-system-info (hash-ref r 'destination)))
            (and sys
                 (string? (hash-ref sys 'controller))
                 (not (string=? (hash-ref sys 'controller) (current-civ-name)))))
          routes))


;; (list Ship System [Listof Route]) -> (list Integer (list ShipOrder ShipOrder))
;; takes the given ship, system, and routes from the system, and produces
;; orders that will command the given ship to conquer the given system
(define (ship+system+routes->target+ship-orders data)
  (match-define (list ship system routes) data)
  (cond [(empty? routes) (values)]
        [else (define target (hash-ref (list-ref routes (random (length routes))) 'destination))
              (define ship-id (hash-ref ship 'id))
  
              (define move-order (hasheq 'order "ftl" 'destination target 'id ship-id))
              (define seize-order (hasheq 'order "seize" 'id ship-id))
              (list (hash 'target target) (list move-order seize-order))]))


(define (system-info->build-order system-info)
  (hasheq 'id (hash-ref system-info 'id)
          'civ (current-civ-name)
          'order "build"
          'count (hash-ref system-info 'production)))


;; ---------------------------------------------------------------------------------------------------
;; DB Operations

(register-collection system-info id [armies routes])
(register-collection ship-info id [])
(register-collection conquer-attempt target [])
(register-collection routes id [origin destination length])

;; ---------------------------------------------------------------------------------------------------
;; Outward bound API requests

;; SystemOrder -> Void
;; sends the given system order to the api
(define (system-order->api order)
  (add-system-order (hash-ref order 'id)
                    (hash-remove order 'id)))

;; ShipOrder -> Void
;; sends the given ship order to the api
(define (ship-order->api order)
  (add-ship-order (hash-ref order 'id)
                  (hash-remove order 'id)))


;; ShipOrder -> Void
;; sends the given ship order to the api
(define (ship-orders->api orders)
  (map ship-order->api orders))





