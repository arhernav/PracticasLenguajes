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

(define (tipos-lista l)
  (cond
    [(empty? l) "emptyList"]
    [(cons (getType (car l)) (tipos-lista (cdr l)))]
   )
)


(define (getType x)
	(cond
		[(boolean? x) "boolean"]
                [(number? x) "number"]
		[(char? x) "char"] 
                [(string? x) "string"]
                [(symbol? x) "symbol"]
                [(keyword? x ) "keyword"]
                [empty "list"]
                [(list?) x "list"]
                [(pair? x) "pair"]
                [(null? x) "emptyList"]
		[else "otro"]
	)
)




;; Problema 3


(define (raro? numero)
  
   (define m (map string->number(map string (string->list (number->string numero)))))

   (if (= (length m) 1 ) 
     (= (exp (first m)) numero)
    ;La siguiente linea solo funciona con numeros de aridad 2, no se como hacerlo recursivo
     (= (+ ( exp(first m)) (exp(first(rest m)))) numero))
  ;(= ( map + (map exp m)) numero) esto no jala por que no obtiene los numeros de la lista a sumar, pero es la forma corta del anterior
  )

 ;auxiliar obtener exp cubico
(define (exp num)
  (* num (* num num) ))

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
;; Problema 5
;; Problema 6



(define (primo? numb)
;esto no da #f en todos los que no son primos, no se como iterar un stream para que se evaule el mod de numb de todos los elementos
  ;antes de el (sequence->list (in-range 2 numb)) 
  (if(= (mood numb 2) 0 ) #f #t )
  )
;auxiliar obtener modulo
   (define (mood a b)
      (-  a (* b (floor(/ a b))) )
      )
;; Problema 7
;; Problema 8
;; Problema 9
;; Problema 10

(define-type Nat [Cero] [Suc (n Nat?)])
(define-type AE [N (n number?)])

(define (extender-suc-geom list)
  (cond
    [(not (list? list)) '()]
    [(empty? list) '()]
    [(suc-geom? list)
     (let (
           [ratio (/ (second list) (first list))]
           [f (first list)])
       (geometrize list (~v ratio) 1 (~v f))
       )
     ]
    [else '()]
  )
)

;; determina si una lista es una sucesión geométrica.
(define (suc-geom? list)
  {let
      ([ratio (/ (first list) (second list))])
    [suc-geom-aux? list ratio]
    }
  )
;;auxiliar
(define (suc-geom-aux? list ratio)
  (if (< (length list) 2)
      #t
      (cond
        [(= (second list) 0) #f] ;; Este caso evita que la función se muera si le pasas una lista con algún 0 en una posición distinta a la cabeza.
        [(= ratio (/ (first list) (second list)))
            [suc-geom-aux? (cdr list) ratio]
        ]
        [else #f]
        )
      )
  )

;; Convierte la lista a la sintaxis esperada.
(define (geometrize list r i f)
  (if (empty? list) '()
      (cons
       (string-append f "*" r "^(" (~v i) "-1)")
       (geometrize (cdr list) r (+ i 1) f)
      )
  )
)