#lang racket
(require "messages.rkt"
         "shared.rkt")

(provide make-pipeline
         clear-channels!)

(module+ test
  (require rackunit))


;; (list [-> X1] [X1 -> X2] [X2 -> X3] .... [Xn -> Void]) -> [List-of component%]
;; Takes a list of functions, the first having no input, the rest taking
;; the output of the previous as input, and produces a pipeline of components
;; that communicate using redis.
(define (make-pipeline name . inputs)
  (make-object pipeline% name inputs))

(module+ test
  (define PIPELINE-OUT #f)
  (define pipe (make-pipeline "bnlasdfe" (thunk 3) add1 add1 (λ (x) (set! PIPELINE-OUT x))))
  (send pipe run!)
  (sleep .5)
  (send pipe stop!)
  (check-equal? PIPELINE-OUT 5)

  (define P2-OUT #f)
  (define P2 (make-pipeline "pipeasdf" (thunk 3) add1 add1 (list 2 (λ (x) (set! P2-OUT x)))))
  (check-equal? (length (get-field components P2)) 5)
  (send P2 run!)
  (sleep 1)
  (send pipe stop!)
  (check-equal? P2-OUT 5)
  )


(define pipeline%
  (class object%
    (super-new)
    (init-field name)
    (init pipes)
    (field [components (apply pipes->components (cons name pipes))])

    ;; running-thread : [Maybe ThreadDescriptor]
    ;; represents the current thread the component is running in
    (field [running-threads '()])
   
    ;; component-descriptors : [Listof (list Input Operation Output Name)]
    ;; apply make-component to these to get components
    (field [component-descriptors 
            (sort #:key (λ (x) (if (and (list? x) (list? (second x)))
                                   (string->number (second (second x))) -1))
                  (set->list 
                   (list->set (map (λ (c) (list (get-field broker c)
                                                (get-field input-channel c)
                                                (get-field operation c)
                                                (get-field output-channel c)
                                                (get-field pipeline-name c)))
                                   components)))
                  <)])

    ;; run! : -> Void
    ;; starts each component in this pipeline
    (define/public (run! [delay 0.1])
      (set! running-threads (map thread-go components)))

    (define (thread-go component)
      (thread
       (thunk
        (message-broker-hostnames (list (get-field broker component)))
        (send component init!)
        (let loop ()
          (send component tick!)
          (loop)))))

    ;; stop! : -> Void
    ;; stops this pipeline, shutting down each component
    ;; and killing the running threads
    (define/public (stop!)
      (for-each kill-thread running-threads)
      (for-each (λ (component) (send component stop!)) components)
      (set! running-threads '()))

    ;; spin-up! : Integer -> Void
    ;; spin up a new component reading from the nth channel and writing to the n+1th
    (define/public (spin-up! channel-index)
      (define component (apply make-component (list-ref component-descriptors channel-index)))
      (displayln (list-ref component-descriptors channel-index))
      (set! components (cons component components))
      (when (cons? running-threads)
        (set! running-threads (cons (thread-go component) running-threads))))
    ))

;; String [->]... -> [List-of Component%]
;; creates components from the given set of functions
;; TODO: [ ] use multiple brokers per pipeline
(define (pipes->components name . pipeline)
  ;; [->] [List-of [->]] [->]  ->  [List-of component%]
  (define (assemble-pipeline broker input pipes output index)
    (cond [(empty? pipes)
           (pipe->components broker input output (λ (x) (void x)))]
          [else
           (define cur-out `(,name ,(number->string index)))
           (append (pipe->components broker input (first pipes) cur-out)
                   (assemble-pipeline broker cur-out (rest pipes) output (add1 index)))]))

  ;; [->] [Or [->] (list N [->])] [X ->] -> [List-of component%]
  (define (pipe->components broker input pipe output)
    (cond [(list? pipe)
           (build-list (first pipe) (λ (_) (make-component broker input (second pipe) output name)))]
          [else (list (make-component broker input pipe output name))]))

  (define input (first pipeline))
  (define output (last pipeline))
  ;; remove input and output
  (define pipes (reverse (rest (reverse (rest pipeline)))))
  (define first-channel (list name "0"))
  (define first-broker (message-broker))
  (cons (make-component first-broker input identity first-channel name)
        (assemble-pipeline first-broker first-channel pipes output 1)))


(module+ test
  (define PIPE1-OUT #f)
  (define PIPE1 (pipes->components "c1"
                                   (λ () 3)
                                   add1
                                   (λ (n) (set! PIPE1-OUT n))))
  (check-equal? (length PIPE1) 3)
  (send* (first PIPE1) [init!] [tick!])
  (send* (second PIPE1) [init!] [tick!])
  (send* (third PIPE1) [init!] [tick!])
  (check-equal? PIPE1-OUT 4)

  (clear-channels!)
  (define PIPE2-OUT #f)
  (define PIPE2 (pipes->components "pipe"
                                   (thunk "hello")
                                   string-length ; 5
                                   add1 ; 6
                                   (λ (x) (* x 2)) ; 12
                                   (λ (x) (build-list x identity))
                                   length ; 12
                                   (λ (x) (set! PIPE2-OUT x))))
  (check-equal? (length PIPE2) 7)

  (for-each (λ (pipe) (send* pipe [init!] [tick!])) PIPE2)
  (check-equal? PIPE2-OUT 12)
  (clear-channels!)
  (define PIPE-OUT3 #f)
  (define PIPE3 (pipes->components "pipe2"
                                   (thunk 3)
                                   add1
                                   add1
                                   (list 2 (λ (x) (set! PIPE-OUT3 x)))))
  (for-each (λ (p) (send* p [init!] [tick!])) PIPE3)
  (check-equal? PIPE-OUT3 5)
  )



(define (make-component broker in op out name)
  (new component% [broker broker] [input in] [operation op] [output out] [pipeline-name name]))

(define component%
  (class object%
    (super-new)

    (init-field broker)

    ;; input : [-> [Or JSExpr 'empty-channel]]
    ;; a thunk that returns a value from the channel this component is listening to
    (init-field input)
    (field [input-channel input]) 

    ;; operation : [JSExpr -> JSExpr]
    ;; the operation to perform on data
    (init-field operation)

    ;; output : [JSExpr -> Void]
    ;; a function to be used to push values to a channel
    (init-field output)
    (field [output-channel output])

    ;; pipeline-name : String
    ;; the name of the pipeline this component is in
    (init-field pipeline-name)

    (define/public (init!)
      ;; delay initialization until you are in your own thread
      ;; so that redis connections are created in the thread they are used in
      (when (list? input)
        (set! input (message-channel (first input) (second input) broker)))
      (when (list? output)
        (set! output (message-channel (first output) (second output) broker))))

    (define/public (stop!)
      (set! input input-channel)
      (set! output output-channel))

    ;; tick! : -> Void
    ;; runs one step of the operation of this component
    (define/public (tick!)
      (init!)
      (define cwv call-with-values)
      (define (process item)
        (cwv (thunk (operation item))
             (λ data (map output data))))

      (cwv input (λ data (for-each process data)))

      (unless (procedure? input-channel) 
        (define count (channel-size (apply channel-name input-channel)))
        (when (number? count)
          (void (broadcast (string-append
                            LOGGING-CHANNEL ":"
                            (first input-channel) ":" 
                            (second input-channel))
                           count)))))
    ))



