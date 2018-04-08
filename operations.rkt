#lang racket
(provide (all-defined-out))

(require "api-commands.rkt"
         "storage.rkt"
         "messages.rkt")
(module+ test
  (require rackunit))

(provide clear-storage!
         current-mongo
         current-api-key
         current-api-location
         current-civ-name)

;; ---------------------------------------------------------------------------------------------------
;; Higher Order operations

;; ->+ : [X -> Y] [Y -> Z] [X Z -> R] -> [X -> R]
;; applies the firs two functions to the input data to get a result.
;; combines the result with the original input to output
(define (->+ selector processor combinator)
  (λ (input)
    (define out
      (call-with-values (thunk (processor (selector input)))
                        (λ v (map (λ (x) (combinator input x)) v))))
    (apply values out)))

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


;; fork : [List-of [X -> Void]] -> [X -> Void]
;; applies each handler to the given input
(define (fork . handlers)
  (λ (input) (for-each (λ (h) (h input)) handlers)))


;; handle : [X -> Y] [List-of [Y -> Void]] -> [X -> Void]
;; produces a function that accepts an X, transforms it to a Y, then
;; applies each of the processors
(define (handle #:do processor . transformers)
  (λ (input)
    (processor ((apply compose (reverse transformers)) input))))


;; every : Number [X -> Y] -> [X -> Y]
(define (every seconds func)
  (λ args (sleep seconds) (apply func args)))


;; ->channel : String -> [Any -> Void]
;; outputs the given data to the given channel
(define (->channel c)
  (void (curry broadcast c)))


;; ->filter : [X -> Y] -> [X -> [Or X (values)]
(define (->filter predicate)
  (λ (x) (if (predicate x) x (values))))


;; log : String [X -> Y] -> [X -> Y]
;; logs the input to the given channel, then applies the function
(define (log c [func identity])
  (λ input (broadcast c input) (apply func input)))

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



;; Integer -> System
;; finds all available informatino on the given system
(define (system-id->system-info system)
  (define detail (get-system-info system))
  (if (symbol? detail) (values) (values detail)))



;; Integer -> Ship
;; finds all information on the given ship from the api
(define (ship-id->ship-info id)
  (get-ship id))


;; Integer -> [List-of ShipOrder]
;; what orders does the given ship have?
(define (ship-id->ship-orders id)
  (hash-ref (get-ship id) 'orders))


;; Ship -> System
;; finds information on the system the given ship is currently located at
(define (ship-info->system-info ship-info)
  (define s (id->retrieve-single-system-info (hash-ref ship-info 'location)))
  (if s s (values)))
  


;; System -> [List-of Route]
;; literally just extracts the key routes from the given system
(define (system-info->routes system-info)
  (hash-ref system-info 'routes))

;; [List-of Route] -> [List-of Route]
;; keeps only the routes not owned by me
(define (routes->routes-not-owned routes)
  (filter (λ (r)
            (define sys (id->retrieve-single-system-info (hash-ref r 'destination)))
            (if sys (string=? (hash-ref sys 'controller) (current-civ-name)) #t))
          routes))


;; (list Ship System [Listof Route]) -> (list Integer (list ShipOrder ShipOrder))
;; takes the given ship, system, and routes from the system, and produces
;; orders that will command the given ship to conquer the given system
(define (ship+system+routes->target+ship-orders data)
  (match-define (list ship system routes) data)
  (define target (hash-ref (list-ref routes (random (length routes))) 'destination))
  (define ship-id (hash-ref ship 'id))
  
  (define move-order (hasheq 'order "ftl" 'destination target 'id ship-id))
  (define seize-order (hasheq 'order "seize" 'id ship-id))
  (list (hash 'target target) (list move-order seize-order)))


;; ---------------------------------------------------------------------------------------------------
;; DB Operations

(register-collection system-info id [])
(register-collection ship-info id [])
(register-collection conquer-attempt target [])


(define (system-info->build-order system-info)
  (define production (hash-ref system-info 'production))
      
  (hasheq 'id (hash-ref system-info 'id)
          'civ (current-civ-name)
          'order "build"
          'count production))


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





