#lang racket
(require "components.rkt"
         "operations.rkt"
         "setup.rkt"
         "shared.rkt")

(load-config "test-config.json")

(define PIPELINES
  (list
   ;; store up-to-date information on each system in the database
   (make-pipeline "store-system-info"
		  (every .25 api->system-ids)
                  system-id->system-info
                  system-info->db)

   ;; store up-to-date information on each ship in the database
   (make-pipeline "store-ship-info"
		  (every .25 api->ship-ids)
                  ship-id->ship-info
                  ship-info->db)

   ;; send build orders to each system in the database
   (make-pipeline "build-orders" 
		  (every .5 db->system-info)
		  system-info->build-order
                  (parallel 3 system-order->api))

   ;; extract information on ships, send them to conquer adjacent systems
   (make-pipeline "conquer-systems"
		  db->ship-info
                  (->filter-> no-ship-orders?)
                  (->+ identity ship-info->system-info place-last)
                  (->+ second system-info->routes place-last)
                  (->+ third routes->routes-not-owned (replace third))
                  ship+system+routes->target+ship-orders
                  (fork (handle first #:do conquer-attempt->db)
                        (handle second #:do ship-orders->api)))

   ;; create a mapping from each system to systems within a minute's travel
   (make-pipeline "store-routes"
		  db->system-info
                  system-info->routes
                  (curry apply values)
                  routes->db)
   ))

(define (run!)
  (clear-storage!)
  ;(clear-channels!)
  (for-each (λ (pipeline) (send pipeline run!)) PIPELINES)
  (sleep 10)
  (for-each (λ (pipeline) (send pipeline stop!)) PIPELINES))



(void (run!))















