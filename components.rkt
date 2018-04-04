#lang racket
(provide
 
 make-pipeline)


(require "messages.rkt")
(provide message-broker-hostnames
         clear-channels!)

(module+ test
  (require rackunit))


(define (make-pipeline . inputs)
  (make-object pipeline% inputs))

(module+ test
  (define PIPELINE-OUT #f)
  (define pipe (make-pipeline (thunk 3) add1 add1 (λ (x) (set! PIPELINE-OUT x))))
  (send pipe run!)
  (sleep 1)
  (send pipe stop!)
  (check-equal? PIPELINE-OUT 5))
                 
(define pipeline%
  (class object%
    (super-new)

    (init pipes)
    (field [components (apply pipes->components pipes)])

    ;; running-thread : [Maybe ThreadDescriptor]
    ;; represents the current thread the component is running in
    (field [running-thread #f])

    ;; run! : -> Void
    ;; starts each component in this pipeline
    (define/public (run! [delay .5])
      (stop!)

      (define (go)
        (for-each (λ (component) (send component init!)) components)
        (let loop ()
          (for-each (λ (component) (send component tick!)) components)
          (sleep delay)
          (loop)))
      
      (set! running-thread (thread go)))
    
    ;; stop! : -> Void
    ;; stops the data from flowing
    (define/public (stop!)
      (when running-thread
        (kill-thread running-thread))
      (set! running-thread #f))
    ))


;; (list [-> X1] [X1 -> X2] [X2 -> X3] .... [Xn -> Void]) -> [List-of component%]
;; Takes a list of functions, the first having no input, the rest taking
;; the output of the previous as input, and produces a pipeline of components
;; that communicate using redis.
(define (pipes->components . pipeline)
  (define input (first pipeline))
  (define output (last pipeline))
  (define pipes (reverse (rest (reverse (rest pipeline)))))
  

  ;; [->] [List-of [->]] [->]  ->  [List-of component%]
  (define (assemble-pipeline input pipes output)
    (cond [(empty? (rest pipes))
           (list (make-component input (first pipes) output))]
          [else
           (define cur-out (random-message-channel-name))
           (cons (make-component input (first pipes) cur-out)
                 (assemble-pipeline cur-out (rest pipes) output))]))

  (assemble-pipeline input pipes output))


(module+ test
  (define PIPE1-OUT #f)
  (define PIPE1 (pipes->components (λ () 3)
                                   add1
                                   (λ (n) (set! PIPE1-OUT n))))
  (check-equal? (length PIPE1) 1)
  (send* (first PIPE1) [init!] [tick!])
  (check-equal? PIPE1-OUT 4)

  (define PIPE2-OUT #f)
  (define PIPE2 (pipes->components (thunk "hello")
                                   string-length ; 5
                                   add1 ; 6
                                   (λ (x) (* x 2)) ; 12
                                   (λ (x) (build-list x identity))
                                   length ; 12
                                   (λ (x) (set! PIPE2-OUT x))))
  (check-equal? (length PIPE2) 5)
  
  (for-each (λ (pipe) (send* pipe [init!] [tick!])) PIPE2)
  (check-equal? PIPE2-OUT 12) 
  )


(define (make-component in op out)
  (new component% [input in] [operation op] [output out]))
  


(define component%
  (class object%
    (super-new)
    
    ;; input : [-> [Or JSExpr 'empty-channel]]
    ;; a thunk that returns a value from the channel this component is listening to
    (init-field input)
    (field [input-channel input]) 

    ;; operation : [X -> Y]
    ;; the operation to perform on data
    (init-field operation)
    
    ;; output : [JSExpr -> Void]
    ;; a function to be used to push values to a channel
    (init-field output)
    (field [output-channel output])
    
    (define/public (init!)
      (when (string? input)
        (set! input (random-message-channel #:channel input-channel)))
      (when (string? output)
        (set! output (random-message-channel #:channel output-channel))))

    (define/public (stop!)
      (set! input input-channel)
      (set! output output-channel))

    ;; tick! : -> Void
    ;; runs one step of the operation of this component
    (define/public (tick!)
      (call-with-values
       input
       (λ data
         (for-each (λ (next-data) 
                     (call-with-values (thunk (operation next-data))
                                       (λ data (for-each (λ (d) (output d)) data))))
                   data))))
    ))



    



















    




