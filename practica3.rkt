#lang plai

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
    (cond
        [(symbol? sexp)
            (case sexp
                [(T) (bool #t)]
                [(F) (bool #f)]
                [else (id sexp)]
            )]
        [(number? sexp) (num sexp)]
        [(list? sexp) (case (first sexp)
            [(+ - * / modulo expt not) (parse-op sexp)]
        )]
    )
)

(define (subst fwae-ast sub-id valor)
    (cond
        ; La expresión ES un ID y uede ser el que estoy buscando
        [(id? fwae-ast) (if (eq? sub-id (id-i fwae-ast))
            valor
            fwae-ast
        )]
        ; La expresión PUEDE tener IDs y hay que buscar en ellos a sub-id
        [(op? fwae-ast) (op (op-f fwae-ast)
            (map (lambda (arg) (subst arg sub-id valor)) (op-args fwae-ast)))]
        ; La expresión NO pude tener IDs
        [else fwae-ast]
    )
)

(define (interp fwae-ast)
    (cond
        [(with? fwae-ast) (interp (foldl
			(lambda (bdg expr-res)
				(subst expr-res (binding-id bdg) (binding-value bdg))
			)
			(with-body fwae-ast)
            (with-bindings fwae-ast)))]
    )
)
