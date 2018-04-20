#lang racket
(require "components.rkt"
         "operations.rkt"
         "setup.rkt"
         "shared.rkt"
         plot
         pict
         redis
         threading
         racket/gui)

(load-config "test-config.json")

(define PIPELINES
  (list
   ;; store up-to-date information on each ship in the database
   (make-pipeline "store-ship-info"
                  (every .1 api->ship-ids)
                  ship-id->ship-info
                  ship-info->db)

   (make-pipeline "store-system-info"
                  (every .1 api->system-ids)
                  system-id->system-info
                  system-info->db)

   ;; send build orders to each system in the database
   (make-pipeline "build-orders" 
                  (every .5 db->system-info)
                  (->filter-> (λ (x) (not (symbol? x))))
                  (->filter-> system-less-than-ten-orders?)
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
                  (every 1 db->system-info)
                  system-info->routes
                  (curry apply values)
                  routes->db)
   ))

(define (run!)
  (clear-storage!)
  (for-each (λ (pipeline) (send pipeline run!)) PIPELINES))

(define (stop!)
  (for-each (λ (pipeline) (send pipeline stop!)) PIPELINES))

(run!)
;; ---------------------------------------------------------------------------------------------------
;; Plotting section

(plot-width 150)
(plot-height 150)

(define (find-pipeline pipeline-name)
  (findf (λ (x) (string-contains? (get-field name x) pipeline-name)) PIPELINES))


(define (spin-up channel)
  (match-define (list _ pipeline-name index) (string-split channel ":"))
  (when (string->number index)
    (send (find-pipeline pipeline-name) 
          spin-up! (add1 (string->number index)))
    (displayln (length (get-field components (find-pipeline pipeline-name))))))

(define FRAME (new frame% [label "Pipeline Vis"]
                   [min-width 1200]
                   [min-height 900]))
(define channels (map bytes->string/utf-8 (KEYS (string-append LOGGING-CHANNEL ":*"))))
(define pipelines (~> channels
                      (map (λ (x) (string-split x ":")) _)
                      (map second _)
                      list->set
                      set->list))


;; -> [List-of pict%]
(define (get-graphs pipeline)

  (define renderers
    (for/list ([channel (filter (λ (x) (string-contains? x pipeline)) channels)])
      (get-graph/channel channel)))

  renderers)


(define (render-plot channel plotter)
  (send (plot-snip plotter
                   #:title channel
                   #:x-label ""
                   #:y-label "") get-bitmap))


;; String -> (list String Renderer2D)
(define (get-graph/channel channel)
  (define counts (map (compose string->number bytes->string/utf-8)
                      (LRANGE channel 0 (LLEN channel))))
  (render-plot channel
               (lines (map vector (build-list (length counts) add1) counts))))

(define PLOTS
  (for/list ([p pipelines])
    (define pipe-channels (sort (filter (λ (x) (string-contains? x p)) channels) string<?))
    (define pane (new horizontal-pane% [parent FRAME]))
    (map (λ (channel)
           (new (class canvas%
                  (super-new)
                  (define/override (on-event event)
                    ; if it was a mouse click, spin up a new component for this element
                    (when (send event button-down?)
                      (spin-up channel))))
                [parent pane]
                [paint-callback (λ (p dc)
                                  (send dc draw-bitmap 
                                        (get-graph/channel channel)
                                        0 0))]
                ))
         pipe-channels)))

(define (update-plots)
  (for-each (λ (pipeline)
              (for-each (λ (plot) (send plot on-paint)) pipeline))
            PLOTS))


;; ---------------------------------------------------------------------------------------------------
;; Run!


(send FRAME show #t)
(define PLOT-UPDATE-TIMER
  (new timer% [notify-callback update-plots] [interval 1000]))










