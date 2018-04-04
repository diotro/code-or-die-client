#lang racket
(provide
 clear-storage!

 
 get-system-ids
 system-id->system-info
 system-info->build-order!
 send-order-to-system
 store-system-info
 system-id->system-info
 retrieve-systems-info)

(require "api-commands.rkt"
         "storage.rkt")
(provide current-api-key)


(define (get-system-ids)
  (apply values (get-systems)))


(define (retrieve-systems-info)
  (apply values (find-stored "system-info" (hash))))


(define (system-id->system-info system)
  (define detail (get-system-info system))
  (if (symbol? detail) (values) (values detail)))


(define (system-info->build-order! system-info)
  (cond [(not system-info) '()]
        [else (define production (hash-ref system-info 'production))
      
              (define order (hash
                             'id (hash-ref system-info 'id)
                             'order "build"
                             'count production))
              order]))


(define (send-order-to-system order)
  (add-system-order (hash-ref order 'id)
                    (hash-remove order 'id)))


(define (store-system-info info)
  (store! "system-info" info))


(define (system-id->retrieve-system-info system-id)
  (and system-id (find-single-stored "system-info" (hash 'id system-id))))


