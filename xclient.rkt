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
                            ("localhost" 6370)
                            ("localhost" 6371)
                            ("localhost" 6372)))

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
        ))




(for-each (λ (pipeline) (send pipeline run!)) PIPELINES)
(sleep 3)
(for-each (λ (pipeline) (send pipeline stop!)) PIPELINES)

