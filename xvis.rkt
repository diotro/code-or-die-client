#lang racket


(require plot
         redis
         "shared.rkt")

(require 2htdp/universe)

(define channels (map bytes->string/utf-8 (KEYS (string-append LOGGING-CHANNEL ":*"))))

(define renderers
  (for/list ([channel (list->set channels)])
    (define counts (map (compose string->number bytes->string/utf-8)
                        (LRANGE channel 0 (LLEN channel))))
      (list channel
            (lines (map vector (build-list (length counts) add1) counts)))))
    

(map (λ (r) (plot (second r) #:title (first r))) (sort renderers string<? q#:key first))

