#lang racket

(require json
         "operations.rkt"
         "shared.rkt"
         "messages.rkt")

(provide
 ; String -> Void
 ; accepts the name of a json file as it's input, and sets parameters
 ; input file must have keys:
 ; - 'api-key
 ; - 'civ-name
 ; - 'api-location
 ; - 'persistent-store
 ; - 'message-brokers
 load-config)

(define (load-config filename)
  (with-input-from-file filename
    (thunk (define CONFIG (read-json))
  
           (current-api-key          (hash-ref CONFIG 'api-key))
           (current-civ-name         (hash-ref CONFIG 'civ-name))
           (current-api-location     (hash-ref CONFIG 'api-location))
           (current-mongo            (hash-ref CONFIG 'persistent-store))
           (message-broker-hostnames (hash-ref CONFIG 'message-brokers))
           (void))))

