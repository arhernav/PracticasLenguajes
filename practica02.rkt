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

(define (num-pasajeros (t Tren?))
  (type-case Tren t
    [trenI (motor vagones) (pasajeros-vagones vagones)]
    [trenD (vagones motor) (pasajeros-vagones vagones)]
    [trenID (motorI vagones motorD) (pasajeros-vagones vagones)]
    [trenV (vagones) (pasajeros-vagones vagones)]
    [trenL (motor) 0]
   )
  )

(define (pasajeros-vagon (v Vagon?))
  (type-case Vagon v
    [pasajeros (cap) cap]
    [restaurante (mes per) 0]
    [dormitorio (cam) 0]
    )
  )

(define (pasajeros-vagones (subVag SubTrenVagon?))
  (type-case SubTrenVagon subVag
    [vagBase (vagB) (pasajeros-vagon vagB)]
    [vagRec (vagB vagR) (+ (pasajeros-vagon vagB) (pasajeros-vagones vagR))]
    )
  )


(define (sin-cama (t Tren?))
  (type-case Tren t
    [trenI (motor vagones) (- (pasajeros-vagones vagones) (camas-vagones vagones))]
    [trenD (vagones motor) (- (pasajeros-vagones vagones) (camas-vagones vagones))]
    [trenID (motorI vagones motorD) (- (pasajeros-vagones vagones) (camas-vagones vagones))]
    [trenV (vagones) (- (pasajeros-vagones vagones) (camas-vagones vagones))]
    [trenL (motor) 0]
    )
  )

(define (camas-vagon (v Vagon?))
  (type-case Vagon v
    [pasajeros (cap) 0]
    [restaurante (mes per) 0]
    [dormitorio (cam) cam]
    )
  )

(define (camas-vagones (subVag SubTrenVagon?))
  (type-case SubTrenVagon subVag
    [vagBase (vagB) (camas-vagon vagB)]
    [vagRec (vagB vagR) (+ (camas-vagon vagB) (camas-vagones vagR))]
    )
  )



;(Tren?
 ;  (TrenID
  ;  [SubTrenLoc ()]
   ; [SubTrenVagon ()]
    ;[SubTrenLoc ()]
    ;))

;(vagRec (dormitorio 10) (vagRec (restaurante 20  30) (vagBase (pasajeros 40))))

;(SubTrenLoc? (locRec (locomotora 2) (locRec (locomotora 4) (locomotora 8))))

;(trenID [locRec (locomotora 2) (locRec (locomotora 4) (locomotora 8))] [vagRec (dormitorio 10) (vagRec (dormitorio 20) (vagBase (dormitorio 30)))] [locRec (locomotora 2) (locRec (locomotora 4) (locomotora 8))])
;(Tren? (trenI (locRec (locomotora 10) (locomotora 20)) (vagRec (dormitorio 10) (vagBase (restaurante 10 20)))))
;(Tren? (trenD (vagRec (dormitorio 10) (vagBase (restaurante 10 20))) (locRec (locomotora 10) (locomotora 20))))
;(Tren? (trenV (vagRec (pasajeros 10) (vagBase (restaurante 10 20)))))
;(Tren? (trenL (locRec (locomotora 10) (locomotora 20))))
