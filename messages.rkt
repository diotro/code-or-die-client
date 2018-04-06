#lang racket

(provide message-broker-hostnames
         message-broker
         
         random-message-channel
         random-message-channel-name

         clear-channels!
         list-all-channels
         )

(require redis
         json)

(module+ test
  (require rackunit))


(define SCHEMA-PREFIX "schema:")
(define CHANNEL-PREFIX "channel:")
(define PIPELINE-PREFIX "pipeline-channel:")


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
                    (define keys (KEYS (string-append PIPELINE-PREFIX "*") #:rconn conn))
                    (for-each DEL keys))))

;; -> [List-of String]
;; retusnt the name of each channel in redis
(define (list-all-channels)
  (apply append (map-redis (λ (conn) (KEYS (string-append PIPELINE-PREFIX "*"))))))


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


;; [#:channel String] -> [ -> ]
;; given a channel name (or generating one randomly),
;; returns a function that:
;;  - if called on 0 arguments will receive data from that channel
;;  - if called on 1 argument will broadcast data to that argument
(define (random-message-channel #:channel [channel (random-message-channel-name)])
  (define host (message-broker))
  (define conn (connect #:host (first host) #:port (second host)))
  (define (process . data)
    (cond
      [(empty? data) (or (receive channel #:redis conn #:default #f) (values))]
      [(empty? (rest data)) (broadcast channel (first data) #:redis conn)]))
  process)

(module+ test
  (clear-channels!)
  (define c (random-message-channel))
  (void (c "hi"))
  (check-equal? (c) "hi")
  (clear-channels!))

;; -> String
;; produces the name of a random redis channel that is not in use
(define (random-message-channel-name)
  (define (random-char _) (integer->char (random 97 123)))
  (define random-part (apply string (build-list 30 random-char)))
  (string-append PIPELINE-PREFIX random-part))

(module+ test
  (check-true (> (string-length (random-message-channel-name)) 30))
  (check-not-equal? (random-message-channel-name) (random-message-channel-name)))

;;--------------------------------------------------------------------------------------------------
;; Messages

;; broadcast : String JSExpr -> Integer
;; Puts the given data in the given channel
(define (broadcast channel data #:redis [conn (current-redis-connection)])
  (parameterize ([current-redis-connection conn])
    ;; TODO [ ] schema
    #;(ensure-meets-schema channel data)
    (RPUSH channel (jsexpr->string data))))


;; receive : String [Int] -> [Or JSExpr 'empty-channel]
;; returns a jsexpr if there is one. Blocks for the given wait time,
;; or three seconds if none is provided
(define (receive channel [wait 3]
                 #:redis [conn #f]
                 #:default [default 'empty-channel])
  (parameterize ([current-redis-connection (or conn (current-redis-connection))])
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






