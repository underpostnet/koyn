#lang scheme

; Developed By Francisco Verdugo <fcoverdugoa@underpost.net>
; https://github.com/underpostnet/koyn

(require "../../../lib/util.scm")
(require "../../../lib/colorize.scm")
(require "../../../lib/str.scm")
(require dyoo-while-loop)
(require sha)
(require json)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

(define Block%
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
         ; (pm_s "calculateHash ->")

         ; (display "index -> ")
         ; (display index)
         ; (display "\n")

         ; (display "previousHash -> ")
         ; (display previousHash)
         ; (display "\n")

         ; (display "date -> ")
         ; (display date)
         ; (display "\n")

         ; (display "data -> ")
         ; (display data)
         ; (display "\n")

         ; (display "nonce -> ")
         ; (display nonce)
         ; (display "\n")

         (define concat_data (string-append
             (number->string index)
             previousHash
             (number->string date)
             (jsexpr->string data)
             (number->string nonce)
         ))

         ; (pg_s "concat ->")
         ; (display concat_data)
         ; (display "\n")

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
         ; (display "hash_return -> ")
         ; (display hash_return)
         ; (display "\n")

         hash_return

      )
      ;--------------------------------------
      (define/public (get-hash)
          ; (pm_s "get-hash ->")
          hash
      )
      ;--------------------------------------
      (define/public (mineBlock difficulty)
          (pm_s (string-append "mineBlock -> difficulty:" difficulty))
          (while (not (startsWith hash difficulty))
            (+set! nonce 1)
            (set! hash (send this calculateHash))
            void
          )
          (printInfo "Mined Block Success" nonce previousHash hash index data date)
          void
      )
      ;--------------------------------------> PREVIOUS HASH
      (define/public (set-previousHash new_previousHash)
          (set! previousHash new_previousHash)
          void
      )
      (define/public (get-previousHash) previousHash)
      ;--------------------------------------> INDEX
      (define/public (set-index new_index)
          (set! index new_index)
          void
      )
      (define/public (get-index) index)
      ;--------------------------------------
      (define/public (get-nonce) nonce)
      ;--------------------------------------
      (define/public (get-data) data)
      ;--------------------------------------
      (define/public (get-date) date)
      ;--------------------------------------

   )
)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

;  (define data_block_a (make-hash))
;  (hash-set! data_block_a 'content "test")
;  (define block_a (new Block%
;    (data data_block_a)
;  ))
;  (pg_s "\nend ->")
;  (display block_a)
;  (display "\n\n")

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

;  (send block_a get-hash)
;  (display "\n")
;  (send block_a mineBlock "000")
;  (display "\n")

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

(define generateData (lambda ()
    (define data_return (make-hash))
    (hash-set! data_return 'content "test")
    data_return
  )
)

(define printInfo (lambda (msg nonce previousHash hash index data date)
    (py_s msg)
    (pr_s (string-append "nonce:" (number->string nonce)))
    (pr_s (string-append "previousHash:" previousHash))
    (pr_s (string-append "hash:" hash))
    (pr_s (string-append "index:" (number->string index)))
    (pr_s (string-append "data:" (jsexpr->string data)))
    (pr_s (string-append "date:" (number->string date)))
  )
)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

(define BlockChain%
   (class object%
      ;--------------------------------------> CONSTRUCTOR
      (init-field
         difficulty
         name
      )
      (field
        (chain (list (send this createGenesis)))
      )
      (super-new)
      ;--------------------------------------
      (define/public (createGenesis)
        (define block_return (new Block%
          (data (generateData))
        ))
        (printInfo "Generate Genesis Block"
          (send block_return get-nonce)
          (send block_return get-previousHash)
          (send block_return get-hash)
          (send block_return get-index)
          (send block_return get-data)
          (send block_return get-date)
        )
        block_return
      )
      ;--------------------------------------
      (define/public (get-latestBlock)
        (list-ref chain 0)
      )
      ;--------------------------------------
      (define/public (addBlock block)

        (define new_index
          (+ (send (send this get-latestBlock) get-index) 1)
        )
        (send block set-index new_index)

        (define new_previousHash
          (send (send this get-latestBlock) get-hash)
        )
        (send block set-previousHash new_previousHash)

        (send block mineBlock difficulty)

        (push block chain)

        void
      )
      ;--------------------------------------
      (define/public (get-chain) chain)
      ;--------------------------------------
      (define/public (get-name) name)
      ;--------------------------------------
      (define/public (get-difficulty) difficulty)
      ;--------------------------------------
      (define/public (checkValid)
        (define valid #t)
        (for ((i (in-range 1 (l chain))))

          (define currentBlock (list-ref (reverse chain) i))
          (define previousBlock (list-ref (reverse chain) (- i 1)))

          (pg_s "data block checkValid ->")
          (pr_s (send currentBlock get-previousHash))
          (pr_s (send previousBlock get-hash))

          (if (not (equal? (send currentBlock get-hash) (send currentBlock calculateHash)))
           (set! valid #f) void)

          (if (not (equal? (send currentBlock get-previousHash) (send previousBlock get-hash)))
           (set! valid #f) void)

        )
        valid
      )

   )
)



;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

(py_s "\nkoyn v1.0\n")

(define koyn (new BlockChain%
  (name "Koyn")
  (difficulty "0000")
))

(display "\n")

(send koyn addBlock (new Block% (data (generateData))))
(send koyn addBlock (new Block% (data (generateData))))
(send koyn addBlock (new Block% (data (generateData))))
(send koyn addBlock (new Block% (data (generateData))))

(py_s "\nKoyn BlockChain Data ->")
(send koyn get-chain)

(py_s "\nKoyn checkValid ->")
(send koyn checkValid)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------






















;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
