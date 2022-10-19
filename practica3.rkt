#lang plai

;; definiciones básicas
(define-type AST
	[id (i symbol?)]
	[num (n number?)]
	[bool (b boolean?)]
	[op (f procedure?) (args (listof AST?))]
	[with (bindings (listof binding?)) (body AST?)]
	[with* (bindings (listof binding?)) (body AST?)]
	[fun (params (listof symbol?)) (body AST?)]
	[app (fun AST?) (args (listof AST?))]
)

(define-type Binding
	[binding (id symbol?) (value AST?)]
)

;; ejercicio 1
(define (parse sexp)
    (define (parse-op opsexp)
        (let (
            [operador (case (first opsexp)
                        [(+) +]
                        [(-) -]
                        [(*) *]
                        [(/) /]
                        [(modulo) modulo]
                        [(expt) expt]
                        [(not) not])])
            (op operador (map parse (rest opsexp)))
        )
    )

    (define (parse-params (params (listof symbol?))) params)
    (define (parse-bindings (bindings (listof (listof pair?))))
        (map (lambda (b)
               (if (symbol? (first b))
                   (binding (first b) (parse (second b)))
                   (error "Identificador inválido.")
                   )
               )
             bindings
    ))
    (define (parse-args (args (listof AST?))) args)
  
    (cond
        [(symbol? sexp)
            (case sexp
                [(T) (bool #t)]
                [(F) (bool #f)]
                [else (id sexp)]
            )]
        [(number? sexp) (num sexp)]
        [(eq? 'fun (first sexp))
            (fun
                (parse-params (second sexp))
                (parse (third sexp)))
        ]
        [(eq? 'with (first sexp))
            (with (parse-bindings (second sexp)) (parse (third sexp)))]
        [(eq? 'with* (first sexp))
            (with* (parse-bindings (second sexp)) (parse (third sexp)))]
        [(eq? 'app (first sexp))
            (app (parse (second sexp)) (parse-args (third sexp)))]
        [(list? sexp) (case (first sexp)
            [(+ - * / modulo expt not) (parse-op sexp)]
        )]
    )
)

;; ejercicio 2
(define (subst (fwae-ast AST?) (sub-id symbol?) (valor AST?))
    (cond
        ; La expresión ES un ID y uede ser el que estoy buscando
        [(id? fwae-ast) (if (eq? sub-id (id-i fwae-ast))
            valor
            fwae-ast
        )]
        #| La expresión PUEDE tener IDs y hay que buscar en ellos a sub-id
           La composicion de funciones en la que aqui primero se invoca primero a <subst>, hace
           el interprete gloton, esto se debe a que realiza la substitución al momento de analizar
           el id de la sub-expresion, esto lo realiza con todos los argumentos de la operación.
          |#
        [(op? fwae-ast) (op (op-f fwae-ast)
            (map (lambda (sub-exp) (subst sub-exp sub-id valor)) (op-args fwae-ast)))]
        #| Caso homóloga al anterior
           La composicion de funciones en la que aqui primero se invoca primero a <subst>, hace
           el interprete gloton, esto se debe a que realiza la substitución al momento de analizar
           el id de la sub-expresion, esto lo realiza con todos los argumentos de la apliación.
          |#
     
        [(app? fwae-ast) (
             (map (lambda (sub-exp) (subst sub-exp sub-id valor)) (op-args fwae-ast)))]
        ; La expresión NO puede tener IDs
        [else fwae-ast]
    )
)

;; ejercicio 3
(define (interp fwae-ast)
  (type-case AST fwae-ast
    [num (n) n]
    [bool (b) b]
    [id (_) (error "Variable Libre")]
    [op (p args)
        ;(if (eq? (procedure-arity p) (length args))
            {apply p [map (lambda (arg) (interp arg)) args]}
            ;{error "Operación con un número de argumentos incorrecto"}
            ;)
        ]
    [fun (_ __) fwae-ast]
       #|  La composicion de funciones en la que aqui primero se invoca primero a <interp>, hace
           el interprete gloton, esto se debe a que realiza la interpretacion al momento de analizar
           el id del binding, esto lo realiza con todos los argumentos del with, y para relaizar esta interpretación
            se aplica primero sa substitución de la expresion.
          |#
    [with (bindings body)
          (interp (foldl
                   {lambda (binding expr)
                     (subst expr (binding-id binding) (binding-value binding))}
                   body
                   bindings
                   ))]
       #|  La composicion de funciones en la que aqui primero se invoca primero a <interp>, hace
           el interprete gloton, esto se debe a que realiza la interpretacion al momento de analizar
           el id del binding, esto lo realiza con todos los argumentos del with*.
          |#
    [with* (bindings body) (interp (with {with*-aux (car bindings) (cdr bindings)} body))]
    #|  La composicion de funciones en la que aqui primero se invoca primero a <interp>, hace
        el interprete gloton, esto se debe a que realiza la interpretacion al momento de analizar
        el id del binding, esto lo realiza con todos los argumentos de la apliación, y para realizar
         esta interpretación se substituye el parametro del argumento con su argumento.
        |#
    [app (f args)
         (if (fun? f)
             (if (eq? (length (fun-params f) (length args)))
                 (interp
                  (foldl
                   {lambda (param arg sust)
                     (subst sust (binding-id param) (arg))}
                   (fun-body f)
                   (fun-params f)
                   (args)
                   ))
                 (error "Número de parametros incorrecto para la función aplicada.")
                 )
             {error (~a (~v f) " no es una función.")})]
    )
)
; función auxiliar para el with anidado. Realiza las sustituciones pertinentes en las variables
   #|  La composicion de funciones en la que aqui primero se invoca primero a <subst>, hace
           el interprete gloton, esto se debe a que realiza la substitución al momento de analizar
           el valor del binding, esto lo realiza con todos los argumentos del with*.
          |#
(define (with*-aux sustitutor resto)
  (let
      ([r (map
          {lambda (b) (subst (binding-value b) (binding-id sustitutor) (binding-value sustitutor))}
          resto
          )
      ])
    [with*-aux (first r) (cdr r)]
    )
  )


; Punto extra areaelipse: AST-num, AST-num → number
(define (areaelipse (A num?) (B num?))
  (* (interp A) (interp B) 3.15)
  )