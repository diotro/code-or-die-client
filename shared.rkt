#lang racket
(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------------------
;; Parameters

;; current-api-location : [parameter? String]
(define current-api-location (make-parameter "http://localhost:80" identity))

;; current-api-key : [parameter? String]
(define current-api-key (make-parameter "" identity))

;; current-civ-name : [parameter? String]
(define current-civ-name (make-parameter "" identity))


;; ---------------------------------------------------------------------------------------------------
;; Constants

(define CHANNEL-PREFIX "channel:")
(define as-channel (curry string-append CHANNEL-PREFIX))

(define PIPELINE-PREFIX "pipeline:")
(define as-pipeline (curry string-append PIPELINE-PREFIX))

(define LOGGING-CHANNEL "logs")
