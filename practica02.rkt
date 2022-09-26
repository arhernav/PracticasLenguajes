#lang plai

;; Ejercicio 1
(define-type Figura
  [triangulo (a number?) (b number?) (c number?)]
  [rectangulo (a number?) (b number?)]
  [rombo (l number?) (d number?) (D number?)]
  [paralelogramo (a number?) (b number?) (h number?)]
  [elipse (a number?) (b number?)]
  )
;; Ejercicio 2
(define (perimetro (f Figura?))
  (type-case Figura f
    [tirangulo (a b c) (+ a b c)]
    [rectangulo (a b) (* 2 (+ a b))]
    [rombo (l d D) (* 4 l)]
    [paralelogramo (a b h) (* 2 (+ a b))]
    [elipse (a b) (* 2 pi (sqrt (/ (+ (expt a 2) (expt b 2)) 2)))])
  )

(define (area (f Figura?))
  (type-case Figura f
    [tirangulo (a b c) (let ([s (/ (+ a b c) 2)])
                         (sqrt [* s (- s a) (- s b) (- s c)]))]
    [rectangulo (a b) (* a b)]
    [rombo (l d D) (* D d 0.5)]
    [paralelogramo (a b h) (* b h)]
    [elipse (a b) (* pi a b)])
  )

;; Ejercicio 3

(define-type SubTrenLoc
  [locomotora (arrastre exact-nonnegative-integer?)]
  [locRec (locB locomotora?) (restLoc SubTrenLoc?)]
  )

(define-type Vagon
  [pasajeros (capacidad exact-nonnegative-integer?)]
  [restaurante (mesas exact-nonnegative-integer?) (personal exact-nonnegative-integer?)]
  [dormitorio (camas exact-nonnegative-integer?)]
  )

(define-type SubTrenVagon
  [vagBase (vagB Vagon?)]
  [vagRec (vagB Vagon?) (vagRes SubTrenVagon?)]
  )

(define-type Tren
  [trenI (motor SubTrenLoc?) (vagones SubTrenVagon?)]
  [trenD (vagones SubTrenVagon?) (motor SubTrenLoc?)]
  [tren-V (vagones SubTrenVagon?)]
  [tren-L (motor SubTrenLoc?)]
)




;; Ejercicio 4
