#lang racket

(require "components.rkt"
         "operations.rkt")


            
(define (make-printing-pipeline prefix . funcs)
  (define (printing func)
    (lambda x
      (display prefix) (display func) (displayln x)
      (apply func x)))
  (apply make-pipeline (map printing funcs)))


;; --------------------------------------------------------------------------------------------------
;; Setup

(clear-storage!)
(clear-channels!)

(current-api-key "one")
(current-civ-name "one")
(current-api-location "http://localhost:80")

#;
(message-broker-hostnames '(("localhost" 6379)
                            ("localhost" 6380)
                            ("localhost" 6381)
                            ("localhost" 6382)))

(define PIPELINES
  (list (make-pipeline api->system-ids
                       system-id->system-info
                       system-info->db)

        (make-pipeline db->systems-info
                       system-info->build-order
                       system-order->api)
        
        (make-pipeline api->ship-ids
                       ship-id->ship-info
                       ship-info->db)
        
        (make-pipeline db->ship-info
                       list
                       (->+ first ship-info->system-info)
                       (->+ second system-info->routes)
                       ship+system+routes->target+ship-orders
                       (fork (handle first conquer-attempt->db)
                             (handle second (curry map ship-order->api))))
        ))



(for-each (λ (pipeline) (send pipeline run!)) PIPELINES)
(sleep 10)
(for-each (λ (pipeline) (send pipeline stop!)) PIPELINES)















