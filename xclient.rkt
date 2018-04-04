#lang racket

(require "components.rkt"
         "operations.rkt")


;; ---------------------------------------------------------------------------------------------------
;; Setup


(clear-storage!)
(current-api-key "one")
#;(clear-channels!)
#;(message-broker-hostnames '(("localhost" 6379)
                              ("localhost" 6370)
                              ("localhost" 6371)
                              ("localhost" 6372)))


;; --------------------------------------------------------------------------------------------------
;; Read system ids, get details, emit to systems-detail

            
(define (make-printing-pipeline prefix . funcs)
  (apply make-pipeline (map (λ (func)
                              (lambda x (display prefix) (display func) (displayln x)
                                (apply func x))) funcs)))

(define SYSTEM-DETAIL-PIPELINE
  (make-pipeline get-system-ids
                 system-id->system-info
                 store-system-info))

(define MAKE-BUILD-ORDER
  (make-pipeline retrieve-systems-info
                 system-info->build-order!
                 send-order-to-system))

(define TEST-PIPELINE
  (make-pipeline (thunk 3)
                 identity
                 identity
                 identity
                 identity
                 identity
                 void))

(send TEST-PIPELINE run!)
#|
(send SYSTEM-DETAIL-PIPELINE run!)
(send MAKE-BUILD-ORDER run!)
(sleep 4)

(send SYSTEM-DETAIL-PIPELINE stop!)
(send MAKE-BUILD-ORDER stop!)
|#
