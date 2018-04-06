#lang racket
(provide
 clear-storage!

 
 api->system-ids
 api->ship-ids
 db->systems-info
 
 system-id->system-info
 ship-id->ship-info
 ship-info->db
 
 system-info->build-order
 system-order->api
 system-info->db
 system-id->system-info)

(require "api-commands.rkt"
         "storage.rkt")

(provide current-api-key
         current-api-location
         current-civ-name)


;; TODO
;; [ ] make macro
;; [ ] add contracts

;; ---------------------------------------------------------------------------------------------------
;; API Operations

;; Direct Input
(define (api->system-ids)
  (apply values (get-systems)))

(define (system-id->system-info system)
  (define detail (get-system-info system))
  (if (symbol? detail) (values) (values detail)))

(define (api->ship-ids)
  (apply values (map (λ (h) (hash-ref h 'id)) (get-ships)))) 


;; Ship Processing
(define (ship-id->ship-info id)
  (get-ship id))




;; ---------------------------------------------------------------------------------------------------
;; DB Operations

(define (system-info->db info)
  (store! "system-info" info))

(define (db->systems-info)
  (apply values (find-stored "system-info" (hash))))


(define (system-id->retrieve-system-info system-id)
  (and system-id (find-single-stored "system-info" (hash 'id system-id))))


(define (ship-info->db info)
  (store! "ship-info" info))


(define (db->ship-info)
  (apply values (find-stored "ship-info" (hash))))


(define (ship-id->retrieve-ship-info ship-id)
  (and ship-id (find-single-stored "ship-info" (hash 'id ship-id))))


(define (system-info->build-order system-info)
  (cond [(not system-info) (values)]
        [else (define production (hash-ref system-info 'production))
      
              (define order (hash
                             'id (hash-ref system-info 'id)
                             'civ (current-civ-name)
                             'order "build"
                             'count production))
              order]))


(define (system-order->api order)
  (add-system-order (hash-ref order 'id)
                    (hash-remove order 'id)))






