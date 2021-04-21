#lang scheme

(require "../../../lib/colorize.scm")
(require "../../../lib/str.scm")
(require sha)
(require json)

(py_s "\n\nkoyn v1.0")

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

(define block%
   (class object%
      ;--------------------------------------> CONSTRUCTOR
      ; data -> json hash
      (init-field data)
      (field

        (index 0)
        (previousHash "0")

        (date (current-seconds))
        (nonce 0)
        (hash (send this calculateHash))

      )
      (super-new)
      ;--------------------------------------
      (define/public (calculateHash)
         (pg_s "\ncalculateHash ->")

         (display "index -> ")
         (display index)
         (display "\n")

         (display "previousHash -> ")
         (display previousHash)
         (display "\n")

         (display "date -> ")
         (display date)
         (display "\n")

         (display "data -> ")
         (display data)
         (display "\n")

         (display "nonce -> ")
         (display nonce)
         (display "\n")

         (define concat_data (string-append
             (number->string index)
             previousHash
             (number->string date)
             (jsexpr->string data)
             (number->string nonce)
         ))

         (pg_s "concat ->")
         (display concat_data)
         (display "\n")

         ;(#[string]) -> string to bytes
         (define hash_test (bytes->hex-string (sha256-bytes
           #"test"
         )))
         (display "hash_test -> ")
         (display hash_test)
         (display "\n")

         (define return_hash (bytes->hex-string (sha256-bytes
           (string->bytes/utf-8 concat_data)
         )))
         (display "return_hash -> ")
         (display return_hash)
         (display "\n")

         return_hash

      )
      ;--------------------------------------
      (define/public (get-hash) hash)
      ;--------------------------------------

   )
)



;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

(define data_block_a (make-hash))
(hash-set! data_block_a 'content "test")
(define block_a (new block%
  (data data_block_a)
))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

(pg_s "end ->")
(display block_a)
(display "\n")
(display (send block_a get-hash))
(display "\n\n")






















;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
