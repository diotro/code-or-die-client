#lang racket

(require "messages.rkt")
(require plot)
(require redis)

(define LOGS (LRANGE "pipeline-log" 0 (LLEN "pipeline-log")))
(define CLEAN-LOGS (map (compose
                         (λ (l) (list (first l) (string->number (second l))))
                         (curryr string-split "\\t")
                         (curryr string-trim "\"")
                         (λ (s) (with-output-to-string (thunk (display s))))
                         bytes->string/utf-8) LOGS))

;; [Hash String [Listof Number]]
;; representing the length of the queue at each time the component processed
(define LOGS-OVER-TIME
  (for/fold ([counts (hash)])
            ([log CLEAN-LOGS])
    (define prev (hash-ref counts (first log) '()))
    (hash-set counts (first log)
              (cons (vector (length prev) (second log)) prev))))


(define RENDERERS
  (filter identity
          (hash-map LOGS-OVER-TIME
                    (λ (f queue-lengths)
                      (if (>= (length queue-lengths) 3)
                          (lines (in-list (reverse queue-lengths)) #:label f)
                          #f)))))

(parameterize ([plot-width 250]
               [plot-height 250])
  (for-each (λ (x) (display (plot x))) RENDERERS))
