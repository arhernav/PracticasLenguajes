#lang plai

;; Integrantes:
;;	Armando Hernández Navarro - 317340347
;;	Juan Carlos Zenteno Pompa - 316251608
;;	Francisco Javier Becerril Lara - 317114490

(define-type AST
  [id (i symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [op (f procedure?) (args (listof AST?))]
  [op-bool (f symbol?) (lhs AST?) (rhs AST?)]
  [branch (test AST?) (then AST?) (else AST?)]
  [multi-branch (conds (listof branch-cond?)) (else AST?)]
  [with (bindings (listof binding?)) (body AST?)]
  [with* (bindings (listof binding?)) (body AST?)]
  [fun (params (listof symbol?)) (body AST?)]
  [lempty]
  [lcons (l AST?) (r AST?)]
  [lcar (lst AST?)]
  [lcdr (lst AST?)]
  [app (fun AST?) (args (listof AST?))]
)

(define-type Binding
  [binding (id symbol?) (value AST?)]
  [branch-cond (test AST?) (then AST?)]
)

(define-type Environment
  [mtSub]
  [aSub (name symbol?) (value AST?) (bSub Environment?)])

(define-type CFWAEL-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [listV (l (listof CFWAEL-Value?))]
  [closureV (param (listof symbol?)) (body AST?) (env Environment?)])

;; Ejercicio 1
(define (parse sexp)
  ; Auxiliar que parsea los operadores
  (define (parse-op opsexp)
    (let
        ([operador (case (first opsexp)
                     [(+) +]
                     [(-) -]
                     [(*) *]
                     [(/) /]
                     [(modulo) modulo]
                     [(expt) expt]
                     [(not) not]
                     [(<) <]
                     [(<=) <=]
                     [(=) =]
                     [(>) >]
                     [(>=) >=]
                     [else (error "Operación no soportada: " (first opsexp))]
                     )])
      (op operador (map parse (rest opsexp)))))
  (define (parse-params (params (listof symbol?))) params)

  ; Parsea los bindings de una función
  (define (parse-bindings (bindings (listof (listof pair?))))
    (map
     (lambda (b)
       (if (symbol? (first b))
           (binding (first b) (parse (second b)))
           (error "Identificador inválido."))
       ) bindings))
  ;; Parsea los condicionales
  (define (parse-conds (conds (listof pair?)))
    (map (lambda (p)
           (let
               ([test (first p)]
                [then (second p)])
             (branch-cond (parse test) (parse then)))) conds))
  ;; Análisis de casos
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp)
     (case sexp
       [(T) (bool #t)]
       [(F) (bool #f)]
       [(lempty) (lempty)]
       [else (id sexp)]
       )]
    [(empty? sexp) (error "Segmento vacío.")]
    [(list? sexp)
     (case (first sexp)
       [(fun)
        (fun
         (if (andmap symbol? (second sexp))
             {second sexp}
             {error "Variables inválidas en la función."})
         (parse (third sexp)))]
       [(if)
        (branch (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))]
       [(cond)
        (multi-branch (parse-conds (second sexp)) (parse (third sexp)))]
       [(with)
        (with (parse-bindings (second sexp)) (parse (third sexp)))]
       [(with*)
        (with* (parse-bindings (second sexp)) (parse (third sexp)))]
       [(app)
        (app (parse (second sexp)) (map parse (third sexp)))]
       [(lcons)
        (lcons (parse (second sexp)) (parse (third sexp)))]
       [(lcar)
        (lcar (parse (second sexp)))]
       [(lcdr)
        (parse (second sexp))]
       [(or and)
        (op-bool (first sexp) (parse (second sexp)) (parse (third sexp)))]
       [else (parse-op sexp)]
       )
     ]
    [else (error "Error sintáctico, valor no parseable.")])
  )


;; Ejercicio 2
(define (desugar (s-fwael-expr AST?))
  ;; Auxiliar que recibe una funcion currificada y realiza las aplicaciones.
  (define (desugar-app arguments func)
    (if (fun? func)
        (app (fun (fun-params func) (desugar-app (cdr arguments) (fun-body func))) (list (car arguments)))
        (app func (map desugar arguments))))
  (type-case AST s-fwael-expr
    [id (_) s-fwael-expr]
    [num (_) s-fwael-expr]
    [bool (_) s-fwael-expr]
    [op (f args) (op f (map desugar args))]
    [op-bool (f lhs rhs)
             (op-bool f (desugar lhs) (desugar rhs))]
    [branch (test then else) (branch (desugar test) (desugar then) (desugar else))]
    [multi-branch (conds else)
                  (foldr
                   (lambda (conditional acc)
                     (branch (branch-cond-test conditional) (branch-cond-then conditional) acc))
                   else conds)]
    [with (bindings body)
          (foldr
           (lambda (b acc)
             (app (fun (list (binding-id b)) acc) (list (desugar (binding-value b)))))
           (desugar body)
           bindings
           )]
    [with* (bindings body)
           (foldr
            (lambda (b acc)
              (app (fun (list (binding-id b)) acc) (list (desugar (binding-value b)))))
            (desugar body)
            bindings)]
    [fun (params body)
         (foldr
            (lambda (arg acc)
              (fun (list arg) acc))
            (desugar body) params)]
    [lempty () lempty]
    [lcons (l r) (lcons (desugar l) (desugar r))]
    [lcar (lst) (lcar (desugar lst))]
    [lcdr (lst) (lcdr (desugar lst))]
    [app (f args) (desugar-app args (desugar f))] 
    ))

;; Ejercicio 3
(define (subst (fwael-expr AST?) (sub-id symbol?) (env Environment?))
  (define (find-inenv (env Environment?))
    (type-case Environment env
      [mtSub () (error "Variable libre:" sub-id)]
      [aSub (name value bsub)
            (if (eq? name sub-id) value
                (find-inenv bsub))]
      ))
  (type-case AST fwael-expr
    [id (i) (if (eq? i sub-id) (find-inenv env) (fwael-expr))]
    [num (_) fwael-expr]
    [bool (_) fwael-expr]
    [op (f args) (op f (map (lambda (expr) (subst expr sub-id env)) args))]
    [op-bool (f lhs rhs) (op-bool f (subst lhs sub-id env) (subst rhs sub-id env))]
    [branch (test then else) (branch (subst test sub-id env) (subst then sub-id env) (subst else sub-id env))]
    [lempty () lempty]
    [lcons (l r) (lcons (subst l sub-id env) (subst r sub-id env))]
    [lcar (l) (subst l sub-id env)]
    [lcdr (l) (subst l sub-id env)]
    [fun (params body) (fun params (subst body sub-id env))]
    [app (f args)
         (app (subst f sub-id env)
              (map (lambda (expr) (subst expr sub-id env)) args))]
    
    [else (error "Expresión aún con azúcar sintáctica:" fwael-expr)]))

;; Ejercicio 4
(define (interp fwael-expr env)
  ; regresa los valores operables envueltos en el tipo FWAEL-Value
  (define (unwrap (v CFWAEL-Value?))
    (type-case CFWAEL-Value v
      [numV (n) n]
      [boolV (b) b]
      [listV (l) l]
      [closureV (params body env) (fun (fun params body) env)]
      ))
  ; recibe un valor y trata de envolverlo como un FWAEL-Value
  (define (wrap v env)
    (cond 
      [(bool? v) (boolV (bool-b v))]
      [(boolean? v) (boolV v)]
      [(list? v) (map wrap v)]
      [(fun? v) (closureV (fun-params v) (fun-body v) (env))]
      [(num? v) (numV (num-n v))]
      [(number? v) (numV v)]
      )
    )
  
  (type-case AST fwael-expr
    [id (i) (interp (subst fwael-expr i env) env)]
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [op-bool (f lhs rhs) (wrap (case f
                                 [(or)
                                  (or (unwrap (interp lhs env)) (unwrap (interp rhs env)))]
                                 [(and)
                                  (and (unwrap (interp lhs env)) (unwrap (interp rhs env)))]
                                 ) '())]
    [op (f args)
        (wrap (apply f
               (map (lambda (v)
                      (unwrap (interp v env)))
                    args)) '())]
    [branch (test then else)
            (let ([b (interp test env)])
              (if (and (boolV? b) (boolV-b b)) (interp then (env)) (interp else (env))))]
    [fun (params body) (closureV params body env)]
    [app (f arg)
         (cond
           [(fun? f)
            (interp (fun-body f) (aSub (car (fun-params f)) (car arg) env))]
           [(id? f)
            (interp (app (subst f (id-i f) env) arg) env)]
           [else
            (error "Incompatibilidad de aridad" arg f)])
         ]
    [lcons (l r) (listV (cons (interp l env) (unwrap (interp r env))))]
    [lcar (l) (if (listV? l) (car (listV-l l)) (error "No es una lista"))]
    [lcdr (l) (if (listV? l) (cdr (listV-l l)) (error "No es una lista"))]
    [lempty () (listV null)]
    [else (error "Azúcar sintactica detectada" fwael-expr)]
    )
  )