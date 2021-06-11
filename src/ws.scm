#lang scheme


;   https://docs.racket-lang.org/rfc6455/index.html


(require net/rfc6455)
(ws-serve* #:port 8081
           (ws-service-mapper
            ["/path" ; the URL path (regular expression)
             [(subprotocol) ; if client requests subprotocol "subprotocol"
              (lambda (c)

                (ws-send! c "You requested a subprotocol")
                (display "ws server protocol msg received \n")


              )]
             [(#f) ; if client did not request any subprotocol
              (lambda (c)

                (ws-send! c "You didn't explicitly request a subprotocol")
                (display "ws server no protocol msg received \n")

              )]]))


(require net/url)
(define client-a (ws-connect (string->url "ws://localhost:8081/path")))
(define client-b (ws-connect (string->url "ws://localhost:8081/path")))
(define client-c (ws-connect (string->url "ws://localhost:8081/path")))
(ws-send! client-a "Hello world!")
(ws-send! client-a "Hello world!")
(ws-send! client-b "Hello world!")
(ws-send! client-b "Hello world!")
