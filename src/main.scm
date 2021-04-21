#lang scheme

(require "../../../lib/util.scm")
(require "../../../lib/colorize.scm")
(require "../../../lib/str.scm")
(require dyoo-while-loop)
(require sha)
(require json)

(py_s "\n\nkoyn v1.0\n")

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
         (pm_s "calculateHash ->")

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

         ;  (#[string]) -> string to bytes
         ;  (define hash_test (bytes->hex-string (sha256-bytes
           ;  #"test"
         ;  )))
         ;  (display "hash_test -> ")
         ;  (display hash_test)
         ;  (display "\n")

         (define hash_return (bytes->hex-string (sha256-bytes
           (string->bytes/utf-8 concat_data)
         )))
         (display "hash_return -> ")
         (display hash_return)
         (display "\n")

         hash_return

      )
      ;--------------------------------------
      (define/public (get-hash)
          (pm_s "get-hash ->")
          hash
      )
      ;--------------------------------------
      (define/public (mineBlock difficulty)
          (pm_s (string-append "mineBlock -> difficulty:" difficulty))
          (define cont 0)
          (while (not (startsWith hash difficulty))
            (+set! nonce 1)
            (set! hash (send this calculateHash))
            void
          )
          (py_s "Mined Block Success")
          (pr_s (string-append "nonce:" (number->string nonce)))
          (pr_s (string-append "hash:" hash))
          void
      )
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
(pg_s "\nend ->")
(display block_a)
(display "\n\n")

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

(send block_a get-hash)
(display "\n")
(send block_a mineBlock "000")
(display "\n")






















;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
