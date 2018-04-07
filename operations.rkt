#lang racket
(provide (all-defined-out))

(require "api-commands.rkt"
         "storage.rkt"
         "messages.rkt")

(provide clear-storage!
         current-api-key
         current-api-location
         current-civ-name)

;; ---------------------------------------------------------------------------------------------------
;; Higher Order operations

(define (->+ . funcs)
  (λ (input)
    (define out
      (call-with-values (thunk ((apply compose (reverse funcs)) input))
                        (λ v (map (λ (x) (append input (list x))) v))))
    (apply values out)))

(define (fork . handlers)
  (λ (input) (for-each (λ (h) (h input)) handlers)))

(define (handle selector . processors)
  (λ (input)
    (define data (selector input))
    (for-each (λ (p) (p data)) processors)))

(define (out c)
  (curry broadcast c))

(define (log c)
  (λ (input) (broadcast c input) input))

(define-syntax-rule (map-values func value-producer)
  (call-with-values (thunk value-producer) (λ v (map func (apply list v)))))
  
;; ---------------------------------------------------------------------------------------------------
;; API Operations

;; Direct Input
(define (api->system-ids)
  (apply values (get-systems)))

(define (system-id->system-info system)
  (define detail (get-system-info system))
  (if (symbol? detail) (values) (values detail)))



;; Ship Processing
(define (api->ship-ids)
  (apply values (map (λ (h) (hash-ref h 'id)) (get-ships)))) 


(define (ship-id->ship-info id)
  (get-ship id))


(define (ship-id->ship-orders id)
  (hash-ref (get-ship id) 'orders))


(define (ship-info->system-info ship-info)
  (call-with-values
   (thunk (system-id->retrieve-system-info (hash-ref ship-info 'location)))
   (λ v (apply values v))))


(define (system-info->routes system-info)
  (hash-ref system-info 'routes))

(define (ship+system+routes->target+ship-orders data)
  (match-define (list ship system routes) data)
  (define target (hash-ref (list-ref routes (random (length routes))) 'destination))
  (define ship-id (hash-ref ship 'id))
  
  (define move-order (hasheq 'order "ftl" 'destination target 'id ship-id))
  (define seize-order (hasheq 'order "seize" 'id ship-id))
  (list target (list move-order seize-order)))


;; ---------------------------------------------------------------------------------------------------
;; DB Operations

(define (system-info->db info)
  (store! "system-info" info))

(define (db->systems-info)
  (apply values (find-stored "system-info" (hash))))


(define (system-id->retrieve-system-info system-id)
  (define s (find-single-stored "system-info" (hasheq 'id system-id)))
  (if s s (values)))


(define (ship-info->db info)
  (store! "ship-info" info))


(define (db->ship-info)
  (apply values (find-stored "ship-info" (hash))))


(define (conquer-attempt->db system)
  (store! "conquer-attempt" (hasheq 'system system)))

(define (ship-id->retrieve-ship-info ship-id)
  (find-single-stored "ship-info" (hasheq 'id ship-id)))


(define (system-info->build-order system-info)
  (define production (hash-ref system-info 'production))
      
  (hasheq 'id (hash-ref system-info 'id)
          'civ (current-civ-name)
          'order "build"
          'count production))


(define (system-order->api order)
  (add-system-order (hash-ref order 'id)
                    (hash-remove order 'id)))

(define (ship-order->api order)
  (add-ship-order (hash-ref order 'id)
                  (hash-remove order 'id)))






