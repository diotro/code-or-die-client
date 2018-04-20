#lang racket


(require plot
         pict
         redis
         threading
         racket/gui
         "shared.rkt")

(plot-width 150)
(plot-height 150)



(define FRAME (new frame% [label "Pipeline Vis"]))
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
  (plot-pict plotter #:title channel
             #:x-label ""
             #:y-label ""))

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
           (new canvas%
                [parent pane]
                [paint-callback (λ (p dc)
                                  (draw-pict (get-graph/channel channel) dc 0 0))]))
         pipe-channels)))

(define (update-plots)
  (for-each (λ (pipeline)
              (for-each (λ (plot) (send plot on-paint)) pipeline))
            PLOTS))


(define PLOT-UPDATE-TIMER
  (new timer% [notify-callback update-plots] [interval 2000]))


(send FRAME show #t)






