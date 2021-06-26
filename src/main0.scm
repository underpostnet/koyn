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
      (init-field rewardAddress)  ; public key to sha256 hash
      (init-field dataTransaction)
      (field

        (index 0)
        (previousHash "0")
        (reward 0)

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
             rewardAddress
             (number->string reward)
             (jsexpr->string dataTransaction)
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
      (define/public (mineBlock difficulty mineLog)
          (pm_s (string-append "mineBlock -> difficulty:" difficulty))
          (while (not (startsWith hash difficulty))
            (+set! nonce 1)
            (set! hash (send this calculateHash))
            (if mineLog (pm_s hash) void)
            void
          )
          (printInfo "Mined Block Success" this)
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
      ;--------------------------------------> REWARD
      (define/public (set-reward new_reward)
          (set! reward new_reward)
          void
      )
      (define/public (get-reward) reward)
      ;--------------------------------------
      (define/public (get-nonce) nonce)
      ;--------------------------------------
      (define/public (get-data) data)
      ;--------------------------------------
      (define/public (get-date) date)
      ;--------------------------------------
      (define/public (get-rewardAddress) rewardAddress)
      ;--------------------------------------
      (define/public (get-dataTransaction) dataTransaction)
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

(define printInfo (lambda (msg block)
    (py_s msg)
    (pr_s (string-append "nonce:" (number->string (send block get-nonce))))
    (pr_s (string-append "previousHash:" (send block get-previousHash)))
    (pr_s (string-append "hash:" (send block get-hash)))
    (pr_s (string-append "index:" (number->string (send block get-index))))
    (pr_s (string-append "data:" (jsexpr->string (send block get-data))))
    (pr_s (string-append "date:" (number->string (send block get-date))))
    (pr_s (string-append "reward:" (number->string (send block get-reward))))
    (pr_s (string-append "rewardAddress:" (send block get-rewardAddress)))
    (pr_s (string-append "dataTransaction:" (jsexpr->string (send block get-dataTransaction))))
  )
)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

(define BlockChain%
   (class object%
      ;--------------------------------------> CONSTRUCTOR
      (init-field
        name
        averageBlockTime
        supply
        maxDivisor
        address
        mineLog
      )
      (field
        (chain (list (send this createGenesis)))
      )
      (super-new)
      ;--------------------------------------
      (define/public (createGenesis)
        (define block_return (new Block%
          (data (generateData)) ; CYBERIA ON-LINE DATA BASE
          (dataTransaction (generateData)) ; [{emisor,receptor,cantidad,tiempo,firma emisor}]
          (rewardAddress address) ; constante inmodificable para saber de donde vienen las monedas
        ))
        (send block_return set-reward supply)
        ; set reward -> supply
        ; los otros tiene que venir firmados por la addres main como ultimo transaccion del arreglo
        ; el unico data transaction vacio el es primero
        (printInfo "Generate Genesis Block" block_return)
        block_return
      )
      ;--------------------------------------
      (define/public (get-latestBlock)
        (list-ref chain 0)
      )
      ;--------------------------------------
      (define/public (addBlock block)


        ; metodo seteo automatico

        (define new_index
          (+ (send (send this get-latestBlock) get-index) 1)
        )
        (send block set-index new_index)

        ; send block set reward calcular reward

        (define new_previousHash
          (send (send this get-latestBlock) get-hash)
        )
        (send block set-previousHash new_previousHash)

        ; ----------------------------------


        (send block mineBlock difficulty mineLog) ; aca se insrta la dificultad

        (push block chain)

        ; recalcular dificultad crear un metodo ?

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

          (printInfo "data block checkValid ->" currentBlock)
          (pg_s (send currentBlock get-previousHash))
          (pg_s (send previousBlock get-hash))

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

(define blockChainAddress "9by4Bvfhlchr9z0N")
(define minerAddress "9by4Bvfhlchr9z0N")

(define koyn (new BlockChain%
  (name "Koyn")
  (averageBlockTime (* 1000 60 10))
  (supply 21000000)
  (maxDivisor 100000000)
  (address blockChainAddress)
  (mineLog #t)
  (version "1.0.0")
))

; reward(numBlock) function 0 -> initReward limit
; difficulty() si con 3 ceros fueron 2 min entonces x 0 seran 10 min
; cada cuando bloque se recalcula
; el reward se calcula solo se debe salir del constructor de blques

(send koyn addBlock
  (new Block%
    (data (generateData))
    (rewardAddress minerAddress)
    (dataTransaction (generateData))
  )
)

(send koyn addBlock
  (new Block%
    (data (generateData))
    (rewardAddress minerAddress)
    (dataTransaction (generateData))
  )
)

(send koyn addBlock
  (new Block%
    (data (generateData))
    (rewardAddress minerAddress)
    (dataTransaction (generateData))
  )
)

(send koyn addBlock
  (new Block%
    (data (generateData))
    (rewardAddress minerAddress)
    (dataTransaction (generateData))
  )
)

(send koyn checkValid)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------






















;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
