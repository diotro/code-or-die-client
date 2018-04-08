#lang racket

(require "components.rkt"
         "operations.rkt")





(current-api-key "one")
(current-civ-name "one")
(current-api-location "http://localhost:80")
(current-mongo (list "localhost" 27017 "code-or-die-test"))

#;
(message-broker-hostnames '(("localhost" 6379)
                            ("localhost" 6380)
                            ("localhost" 6381)
                            ("localhost" 6382)))

(define PIPELINES
  (list
   ;; store up-to-date informatino on each system in the database
   (make-pipeline (every .5 api->system-ids)
                  system-id->system-info
                  system-info->db)

   ;; store up-to-date information on each ship in the database
   (make-pipeline (every .5 api->ship-ids)
                  ship-id->ship-info
                  ship-info->db)

   ;; send build orders to each system in the database
   (make-pipeline (every 2 db->system-info)
                  system-info->build-order
                  system-order->api)

   ;; extract information on ships, send them to conquer adjacent systems
   (make-pipeline db->ship-info
                  (->+ identity ship-info->system-info place-last)
                  (->+ second system-info->routes place-last)
                  (log "before-filter")
                  (->+ third routes->routes-not-owned (replace third))
                  (log "data")
                  ship+system+routes->target+ship-orders
                  (fork (handle first #:do conquer-attempt->db)
                        (handle second #:do ship-orders->api)))

   
   #;(make-pipeline api->ship-ids
                    ship-id->ship-info
                    (log "ship1" ship-info->system-info)
                    system-info->routes
                    (->channel "routes"))
   ))


(define (run!)
  (clear-storage!)
  (clear-channels!)
  (for-each (λ (pipeline) (send pipeline run!)) PIPELINES)
  (sleep 15)
  (for-each (λ (pipeline) (send pipeline stop!)) PIPELINES))


(run!)















