#lang racket

(provide (all-defined-out))
(require "api-lib.rkt"
         json
         net/url)

;; A system id is either a string or an integer
(define system-id? (or/c string? integer?))

;; A ship id is always an integer
(define ship-id? integer?)

;; The actual API functions
(def-api get-rules GET  "/")
(def-api set-api-key POST "/set-api-key")
(def-api get-civ-info GET "/civ-info")

(def-api get-systems GET "/systems")
(def-api get-system-info GET "/systems/~a" system-id?)

(def-api get-system-names GET "/systems/names/~a" system-id?)
(def-api add-system-name POST "/systems/~a/add-name/~a" system-id? string?)

(def-api get-system-orders GET "/systems/~a/orders" system-id?)
(def-api add-system-order PUT "/systems/~a/orders" system-id?)
(def-api set-system-orders POST  "/systems/~a/orders" system-id?)
(def-api delete-system-orders GET "/systems/~a/orders" system-id?)

(def-api delete-system-order-at DELETE "/systems/~a/orders/~a" system-id? integer?)

(def-api get-ships GET "/ships")
(def-api get-ship GET "/ships/~a" ship-id?)

(def-api get-ship-orders GET "/ships/~a/orders" ship-id?)
(def-api add-ship-order PUT "/ships/~a/orders" ship-id?)
(def-api set-ship-orders POST  "/ships/~a/orders" ship-id?)
(def-api delete-ship-orders GET "/ships/~a/orders" ship-id?)

(def-api delete-ship-order-at DELETE "/ships/~a/orders/~a" ship-id? integer?)




(define current-api-location (make-parameter "http://localhost:80" identity))
(define current-api-key (make-parameter "" identity))

;; def-api creates (and adds a contract to) a function that
;; talks to the api
(define-syntax (def-api stx)
  (syntax-case stx (GET PUT POST DELETE)
    [(_ name GET url contracts ...)
     #'(api-route name (-> contracts ... jsexpr?) GET-url url)]
    [(_ name PUT url contracts ...)
     #'(api-route name (-> contracts ... jsexpr? string?) PUT-url url)]
    [(_ name POST url contracts ...)
     #'(api-route name (-> contracts ... jsexpr? string?) POST-url url)]
    [(_ name DELETE url contracts ...)
     #'(api-route name (-> contracts ... jsexpr?) DELETE-url url)]))

(define-syntax (api-route stx)
  (syntax-case stx ()
    [(_ name contract http-function url)
     #'(define/contract (name . args) contract
         (http-function (apply format (cons url args))))]))


;; GET : String -> JSExpr
;; finds the value at the given url in the api
(define (GET-url url)
  (string->jsexpr (port->string
                   (get-pure-port (in-api url) (api-header)))))


;; PUT-url : String JSExpr -> String
;; puts the jsexpr at the given url
(define (PUT-url url data)
  (port->string
   (put-pure-port (in-api url)
                  (string->bytes/utf-8 (jsexpr->string data))
                  (api-header))))


;; POST-url : String JSExpr -> String
;; posts the jsexpr to the given url
(define (POST-url url data)
  (port->string
   (post-pure-port (in-api url)
                   (string->bytes/utf-8 (jsexpr->string data))
                   (api-header))))


;; DELETE-url : String -> String
;; sends a http delete request to the given url
(define (DELETE-url url)
  (port->string
   (delete-pure-port (in-api url)
                     (api-header))))


;; in-api : creates the url to get the given sub url of the api
;; given "/ships", expect a url pointing to "api-url/ships"
(define (in-api url)
  (string->url (string-append (current-api-location) url)))

;; api-header : -> [Listof String]
;; creates the default headers
(define (api-header)
  (list (string-append "api-key:" (current-api-key))))
