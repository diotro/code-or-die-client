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
  (define pipe (make-pipeline (thunk 3) add1 add1 (λ (x) (set! PIPELINE-OUT x))))
  (send pipe run!)
  (sleep .5)
  (send pipe stop!)
  (check-equal? PIPELINE-OUT 5)

  (define P2-OUT #f)
  (define P2 (make-pipeline (thunk 3) add1 add1 (list 2 (λ (x) (set! P2-OUT x)))))
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

    ;; run! : -> Void
    ;; starts each component in this pipeline
    (define/public (run! [delay 0])
      (define broker (list (message-broker)))
      (define (thread-go component)
	(thread
	  (thunk
	    (message-broker-hostnames broker)
	    (send component init!)
	    (let loop ()
	      (send component tick!)
	      (loop)))))

      (set! running-threads (map thread-go components)))

    ;; stop! : -> Void
    ;; stops this pipeline, shutting down each component
    ;; and killing the running threads
    (define/public (stop!)
      (for-each (λ (component) (send component stop!)) components)
      (for-each kill-thread running-threads)
      (set! running-threads '()))
    ))


(define (pipes->components name . pipeline)
  ;; [->] [List-of [->]] [->]  ->  [List-of component%]
  (define (assemble-pipeline input pipes output index)
    (cond [(empty? pipes)
	   (pipe->components input output (λ (x) (void x)))]
	  [else
	    (define cur-out `(,name ,(number->string index)))
	    (define pipe (first pipes))

	    (append (pipe->components input (first pipes) cur-out)
		    (assemble-pipeline cur-out (rest pipes) output (add1 index)))]))

  ;; [->] [Or [->] (list N [->])] [X ->] -> (list component%)
  (define (pipe->components input pipe output)
    (cond [(list? pipe)
	   (build-list (first pipe) (λ (_) (make-component input (second pipe) output name)))]
	  [else (list (make-component input pipe output name))]))

  (define input (first pipeline))
  (define output (last pipeline))
  ;; remove input and output
  (define pipes (reverse (rest (reverse (rest pipeline)))))
  (define first-channel (list name "0"))
  (cons (make-component input identity first-channel name)
	(assemble-pipeline first-channel pipes output 1)))


(module+ test
  (define PIPE1-OUT #f)
  (define PIPE1 (pipes->components (λ () 3)
				   add1
				   (λ (n) (set! PIPE1-OUT n))))
  (check-equal? (length PIPE1) 3)
  (send* (first PIPE1) [init!] [tick!])
  (send* (second PIPE1) [init!] [tick!])
  (send* (third PIPE1) [init!] [tick!])
  (check-equal? PIPE1-OUT 4)

  (define PIPE2-OUT #f)
  (define PIPE2 (pipes->components (thunk "hello")
				   string-length ; 5
				   add1 ; 6
				   (λ (x) (* x 2)) ; 12
				   (λ (x) (build-list x identity))
				   length ; 12
				   (λ (x) (set! PIPE2-OUT x))))
  (check-equal? (length PIPE2) 7)

  (for-each (λ (pipe) (send* pipe [init!] [tick!])) PIPE2)
  (check-equal? PIPE2-OUT 12)

  (define PIPE-OUT3 #f)
  (define PIPE3 (pipes->components (thunk 3)
				   add1
				   add1
				   (list 2 (λ (x) (set! PIPE-OUT3 x)))))
  (for-each (λ (p) (send* p [init!] [tick!])) PIPE3)
  (check-equal? PIPE-OUT3 5)
  )



(define (make-component in op out name)
  (new component% [input in] [operation op] [output out] [pipeline-name name]))

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

    ;; pipeline-name : String
    ;; the name of the pipeline this component is in
    (init-field pipeline-name)

    (define/public (init!)
      ;; delay initialization until you are in your own thread
      ;; so that redis connections are created in the thread they are used in
      (when (list? input)
	(set! input (message-channel (first input) (second input))))

      (when (list? output)
	(set! output (message-channel (first output) (second output)))))

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

      (define count (queue-length input-channel))
      (define op-name (with-output-to-string (thunk (display operation))))
      (when (and (number? count) op-name)
	(void (broadcast LOGGING-CHANNEL (string-append input-channel "\t" (number->string count))))))
    ))


















