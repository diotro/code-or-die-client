#lang racket

(provide (all-defined-out))
(require json
         net/url)

;; ---------------------------------------------------------------------------------------------------
;; Contracts

(define system-id/c (or/c string? integer?))

(define ship-id/c integer?)


;; TODO:
;; [ ] Stronger contracts
(define system-order/c (hash/c any/c any/c))
(define system-info/c (hash/c any/c any/c))
(define ship-order/c (hash/c any/c any/c))
(define ship-info/c (hash/c any/c any/c))

;; ---------------------------------------------------------------------------------------------------
;; Parameters

(define current-api-location (make-parameter "http://localhost:80" identity))
(define current-api-key (make-parameter "" identity))
(define current-civ-name (make-parameter "" identity))


;; ---------------------------------------------------------------------------------------------------
;; Syntax for API functions
;; syntax: (def-api id? GET String (contract? ...) contract?)
;; produces a function bound to id that makes a GET request to the given api
(define-syntax (def-api stx)
  (syntax-case stx (GET PUT POST DELETE)
    [(_ name GET url (in-contracts ...) out-contract)
     #'(define/contract (name . args) (-> in-contracts ... out-contract)
         (GET-url (apply format (cons url args))))]
    [(_ name PUT url (in-contracts ...))
     #'(define/contract (name . args) (-> in-contracts ... any/c)
         (PUT-url (apply format (cons url (take args (sub1 (length args)))))
                  (list-ref args (sub1 (length args)))))]
    [(_ name POST url (in-contracts ...))
     #'(define/contract (name . args) (-> in-contracts ... any/c)
         (POST-url (apply format (cons url (take args (sub1 (length args)))))
                   (list-ref args (sub1 (length args)))))]
    [(_ name DELETE url (in-contracts ...))
     #'(define/contract (name . args) (-> in-contracts ...)
         (DELETE-url (apply format (cons url args))))]))

;; ---------------------------------------------------------------------------------------------------
;; The actual API functions
(def-api get-rules GET  "/" () string?)
(def-api set-api-key POST "/set-api-key" ())
(def-api get-civ-info GET "/civ-info" () string?)

(def-api get-systems GET "/systems" () (listof system-id/c))
(def-api get-system-info GET "/systems/~a" (system-id/c) system-info/c)

(def-api get-system-names GET "/systems/names/~a" (system-id/c) [listof string?])
(def-api add-system-name POST "/systems/~a/add-name/~a" (system-id/c string?))

(def-api get-system-orders GET "/system/~a/orders" (system-id/c) [listof system-order/c])
(def-api add-system-order PUT "/system/~a/orders" (system-id/c system-order/c))
(def-api set-system-orders POST  "/system/~a/orders" (system-id/c [listof system-order/c]))
(def-api delete-system-orders DELETE "/system/~a/orders" (system-id/c))
(def-api delete-system-order-at DELETE "/system/~a/orders/~a" (system-id/c integer?))

(def-api get-ships GET "/ships" () [listof [hash/c any/c any/c]])
(def-api get-ship GET "/ship/~a" (ship-id/c) ship-info/c)

(def-api get-ship-orders GET "/ship/~a/orders" (ship-id/c) [listof ship-order/c])
(def-api add-ship-order PUT "/ship/~a/orders" (ship-id/c ship-order/c))
(def-api set-ship-orders POST  "/ship/~a/orders" (ship-id/c [listof ship-order/c]))
(def-api delete-ship-orders DELETE "/ship/~a/orders" (ship-id/c))
(def-api delete-ship-order-at DELETE "/ship/~a/orders/~a" (ship-id/c integer?))

;; ---------------------------------------------------------------------------------------------------
;; Utility functions

;; in-api : creates the url to get the given sub url of the api
;; given "/ships", expect a url pointing to "api-url/ships"
(define (in-api url)
  (string->url (string-append (current-api-location) url)))

;; api-header : -> [Listof String]
;; creates the default headers
(define (api-header)
  (list (string-append "api-key:" (current-api-key))))

;; ---------------------------------------------------------------------------------------------------
;; HTTP functions

;; GET-url : String -> JSExpr
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