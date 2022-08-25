#lang plai
;; Problema 1
(define (filtra-lista list procedure?)
  (if (empty? list)
      null
      (let
          (
           [head (first list)]
           [filtered-tail (filtra-lista (cdr list) procedure?)]
           )
        (if (procedure? head)
            (cons head filtered-tail)
            filtered-tail
            )
        )
      )
  )
;; Problema 2
;; Problema 3
;; Problema 4
(define descendente?
  (lambda nums
    (if (< (length nums) 2)
        #t
        (if (< (first nums) (second nums))
               #f
               (apply descendente? (rest nums))
               )
        )
    )
  )