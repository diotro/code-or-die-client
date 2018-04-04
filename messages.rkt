#lang racket

(provide get-channel-schema
         set-channel-schema!

         broadcast
         receive

         message-broker-hostnames
         message-broker
         random-message-channel
         random-message-channel-name
         clear-channels!
         )

(require redis
         json)

(module+ test
  (require rackunit))


(define SCHEMA-PREFIX "schema:")


(define message-broker-hostnames (make-parameter '(("localhost" 6379)) identity))
(define (message-broker)
  (list-ref (message-broker-hostnames)
            (random (length (message-broker-hostnames)))))

;;--------------------------------------------------------------------------------------------------
;; Get/set

(define (clear-channels!)
  (void (FLUSHDB)))

;; redis-get : String -> JSExpr
;; gets the value under the given name
(define (redis-get key)
  (string->jsexpr (GET key)))


;; redis-set! : String JSExpr -> Void
;; sets the value under the given name to the given JSExpr
(define (redis-set! key value)
  (SET key (jsexpr->string value)))


(define (random-message-channel #:channel [channel (random-message-channel-name)])
  (define host (message-broker))
  (define conn (connect #:host (first host) #:port (second host)))
  (define (process . data)
    (cond
      ; No data given, so receive data 
      [(empty? data) (define r (receive channel #:redis conn #:default #f))
                     (if r r (values))]
      [(empty? (rest data)) (broadcast channel (first data) #:redis conn)]))
  process)

;; -> String
;; produces the name of a random redis channel that is not in use
(define (random-message-channel-name)
  (define (random-char _) (integer->char (random 97 123)))
  (define random-part (apply string (build-list 30 random-char)))
  (string-append "channel:" random-part))

(module+ test
  (check-equal? (string-length (random-message-channel-name)) 38)
  (check-not-equal? (random-message-channel-name) (random-message-channel-name)))

;;--------------------------------------------------------------------------------------------------
;; Messages

;; broadcast : String JSExpr -> Integer
;; Puts the given data in the given channel
(define (broadcast channel data #:redis conn)
  (parameterize ([current-redis-connection (or conn (current-redis-connection))])
    #;(ensure-meets-schema channel data)
    #;(warn-if-channel-full channel)
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


;; ensure-meets-schema : String JSExpr -> Boolean
;; checks if the schema of the given channel matches the given JSExpr
(define (ensure-meets-schema channel data)
  (define schema (get-channel-schema channel))
  (when (not schema)
    (raise-argument-error 'channel "channel with schema" channel))
  (when (not (meets-schema? data schema))
    (raise-argument-error 'data (schema->string schema) data)))



;; get-channel-schema : String -> [Maybe Schema]
;; gets the schema for the given chanel
(define (get-channel-schema channel)
  (define schema (GET (string-append SCHEMA-PREFIX channel)))
  (if schema
      (string->schema (bytes->string/utf-8 schema))
      #f))


;; set-channel-schema! : String JSExpr -> Void
;; sets the schema of the given channel
(define (set-channel-schema! channel schema)
  (SET (string-append SCHEMA-PREFIX channel) (jsexpr->string schema))
  (void))


;; add-channel-type! : String Symbol [List-of String] -> Void
;; adds the given type as an allowed type in the given channel
(define (add-channel-type! channel type schema)
  (set-channel-schema!
   channel
   (hash-set (get-channel-schema channel) 'types
             (hash-set (hash-ref (get-channel-schema channel) 'types)
                       type schema))))


;; warn-if-channel-full : String -> Void
;; emites a warning of the given channel has too many unprocessed messages
(define (warn-if-channel-full channel)
  (when (> (LLEN channel) 1000)
    (PUBLISH "warnings" (format "channel ~a has too many messages" channel))))



;;--------------------------------------------------------------------------------------------------
;; Schema


;; A Schema is a (make-schema JSONString String ChannelTypes)
(struct schema [json description types] #:transparent)
;; json is the json string that this channel was created from
;; description is a description of the channel
;; types are the predicates that must approve of data being passed to the channel,
;;     joined with the name of the type of data


;; A ChannelTypes is a [Hash Symbol [JSExpr -> Boolean]]
;; where the symbols are the names of the type
;; and the function returns true on instances of that type


;; string->schema : JSONString -> [Maybe Schema]
;; converts the given string to a schema, if possible
(define (string->schema json-string)
  (define json (string->jsexpr json-string))
  (define description (hash-ref json 'description #f))
  (define predicates (create-predicates (hash-ref json 'types #f)))

  (if (and json-string description predicates)
      (schema json-string description predicates)
      #f))

(module+ test
  (define SCHEMA-STRING1
    (jsexpr->string
     (hash 'description "desc"
           'types (hash 'num '("number")))))
  (define SCHEMA-NUMBER (string->schema SCHEMA-STRING1))
  (check-equal? (schema-json SCHEMA-NUMBER) SCHEMA-STRING1)
  (check-equal? (schema-description SCHEMA-NUMBER) "desc")
  (check-equal? (hash-count (schema-types SCHEMA-NUMBER)) 1)
  (define NUM-PRED (hash-ref (schema-types SCHEMA-NUMBER) 'num))
  (check-equal? (NUM-PRED "hi") #f)
  (check-equal? (NUM-PRED 3) #t))


;; jsexpr->schema : JSExpr -> [Maybe Schema]
(define jsexpr->schema (compose string->schema jsexpr->string))

;; create-predicates : [Hash Symbol [List-of JSExpr]] -> [Hash Symbol [JSExpr -> Boolean]]
;; converts each type description into a usable type
(define (create-predicates pred-strings)
  
  (define types
    (for/fold ([types (hash)])
              ([(type-name pred-str) (in-hash pred-strings)])
      (hash-set types type-name (strs->pred pred-str))))
  
  (if (andmap identity (hash-values types))
      types
      #f))

(module+ test
  (check-true (hash? (create-predicates (hash 'a '("any")))))
  (check-true (hash? (create-predicates (hash 'a '("number")))))
  
  (define (get-pred hash)
    (hash-ref (create-predicates hash) 'a))
  (check-true ((get-pred (hash 'a '("number"))) 3))
  (check-false ((get-pred (hash 'a '("number"))) #f)))


;; strs->pred : [List-of String] -> [JSExpr -> Boolean]
;; converts the given list of requirements into a predicate
(define (strs->pred str)
  (define (str->pred str)
    (match str
      ["any" (λ (js) #t)]
      ["number" number?]
      ["string" string?]
      [(list "list" x) (λ (data) (and (list? data) (andmap (str->pred x) data)))]))
  
  (define preds (map str->pred str))
  (if (andmap identity preds)
      (λ (js) (andmap (λ (pred) (pred js)) preds))
      #f))


;; schema->string : Schema -> String
;; converts the given schema to a string representation
(define (schema->string schema)
  (schema-json schema))

(module+ test
  (check-equal? (schema->string SCHEMA-NUMBER) SCHEMA-STRING1))


;; meets-schema? : JSExpr Schema -> [Maybe Symbol]
;; which type is the given jsexpr? returns #f if matches no type in the schema
(define (meets-schema? data schema)
  (for/first ([(type predicate) (in-hash (schema-types schema))]
              #:when (predicate data))
    type))

(module+ test
  (define SCHEMA-NUM (schema "" "" (hash 'num number?)))
  (check-equal? (meets-schema? 3 SCHEMA-NUM) 'num)
  (check-equal? (meets-schema? -100 SCHEMA-NUM) 'num)
  (check-false (meets-schema? #f SCHEMA-NUM))
  (check-false (meets-schema? '() SCHEMA-NUM))

  (define SCHEMA-LON (schema "" ""
                             (hash 'list-of-num
                                   (λ (x) (and (list? x) (andmap number? x))))))
  (check-false (meets-schema? 3 SCHEMA-LON))
  (check-false (meets-schema? -100 SCHEMA-LON))
  (check-false (meets-schema? #f SCHEMA-LON))
  (check-false (meets-schema? '(4 #f "no num") SCHEMA-LON))
  (check-equal? (meets-schema? '() SCHEMA-LON) 'list-of-num)
  (check-equal? (meets-schema? '(3 10) SCHEMA-LON) 'list-of-num)
  (check-equal? (meets-schema? '(4 10) SCHEMA-LON) 'list-of-num))









