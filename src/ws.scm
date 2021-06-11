#lang scheme


;   https://docs.racket-lang.org/rfc6455/index.html


(require net/rfc6455)
(ws-serve* #:port 8081
           (ws-service-mapper
            ["/test" ; the URL path (regular expression)
             [(subprotocol) ; if client requests subprotocol "subprotocol"
              (lambda (c) (ws-send! c "You requested a subprotocol"))]
             [(#f) ; if client did not request any subprotocol
              (lambda (c) (ws-send! c "You didn't explicitly request a subprotocol"))]]))


(require net/url)
(define c (ws-connect (string->url "ws://localhost:8081/test")))
(ws-send! c "Hello world!")
(ws-send! c "Hello world!")
(ws-send! c "Hello world!")
(ws-send! c "Hello world!")
(ws-send! c "Hello world!")
