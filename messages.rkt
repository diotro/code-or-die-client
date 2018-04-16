#lang racket

(provide message-broker-hostnames
         message-broker

         message-channel
         clear-channels!
         list-all-channels

         broadcast
         receive
	 channel-name-for
         queue-length
         )

(require redis
         json
         "shared.rkt")

(module+ test
  (require rackunit))


;; message-broker-hostnames : [parameter? [list String Integer]]
(define message-broker-hostnames
  (make-parameter '(("localhost" 6379)) identity))


;; -> (list String Integer)
;; returns the hostname and port number of a random message broker host
(define (message-broker)
  (list-ref (message-broker-hostnames)
            (random (length (message-broker-hostnames)))))

;; -> Void
;; deletes all data in all message brokers
(define (clear-channels!)
  (for-each-redis (λ (conn)
                    (define keys (KEYS (as-pipeline "*") #:rconn conn))
                    (for-each DEL keys))))

;; -> [List-of String]
;; retusnt the name of each channel in redis
(define (list-all-channels)
  (apply append (map-redis (λ (conn) (KEYS (as-pipeline "*"))))))


;; [RedisConnection -> Void] -> Void
;; applies the given function to each message broker
(define (for-each-redis op)
  (for-each (λ (host) (op (connect #:host (first host) #:port (second host))))
            (message-broker-hostnames)))


;; [X] [RedisConnection -> X] -> [List-of X]
;; applies the given function on a redis connection for each message broker
(define (map-redis op)
  (map (λ (host) (op (connect #:host (first host) #:port (second host))))
       (message-broker-hostnames)))

(define (queue-length channel)
  (if (procedure? channel)
      (void)
      (LLEN channel)))

;; [#:channel String] -> [ -> ]
;; given a channel name (or generating one randomly),
;; returns a function that:
;;  - if called on 0 arguments will receive data from that channel
;;  - if called on 1 argument will broadcast data to that argument
(define (message-channel pipeline-name channel-name)
  (define host (message-broker))
  (define conn (connect #:host (first host) #:port (second host)))
  (define channel (channel-name-for pipeline-name channel-name))
  (define (process . data)
    (cond
      [(empty? data) (or (receive channel #:redis conn #:default #f) (values))]
      [(empty? (rest data)) (broadcast channel #:redis conn (first data))]))
  process)

(module+ test
  (clear-channels!)
  (define c (message-channel "a" "b"))
  (void (c "hi"))
  (check-equal? (c) "hi")
  (clear-channels!))

(define (channel-name-for pipeline-name channel-name)
  (string-append PIPELINE-PREFIX pipeline-name ":" channel-name)) 


;;--------------------------------------------------------------------------------------------------
;; Messages

;; broadcast : String JSExpr -> Integer
;; Puts the given data in the given channel
(define (broadcast channel data #:redis [conn (current-redis-connection)])
  (parameterize ([current-redis-connection conn])
    (RPUSH channel (jsexpr->string data))))


;; receive : String [Int] -> [Or JSExpr 'empty-channel]
;; returns a jsexpr if there is one. Blocks for the given wait time,
;; or three seconds if none is provided
(define (receive channel [wait 3]
                 #:redis [conn #f]
                 #:default [default 'empty-channel])
  (parameterize ([current-redis-connection conn])
    (define response (BLPOP channel wait))
    (cond [(not response) default]
          [(bytes? (second response)) (bytes->jsexpr (second response))]
          [else (format "invalid message in channel ~a" (bytes->string/utf-8 response))])))

(module+ test
  (clear-channels!)
  (define C1 "pipeline-channel:c1")
  (void (broadcast C1 "hello")) 
  (check-equal? (receive C1) "hello")


  (void (broadcast C1 "hi"))
  (void (broadcast C1 "by"))
  (check-equal? (receive C1) "hi")
  (check-equal? (receive C1) "by")
  (clear-channels!)
  )






