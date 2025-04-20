;
#lang racket

(define (fold f a n)
  (if (zero? n)
      a
      (f (- n 1) (fold f a (- n 1)))))

(define (factorial n)
  (fold  (lambda (x y) (if (zero? x) 1 (* (+ 1 x) y)))
        1
        n))

;Выражено с помощью лямбда исчисления: \n.\m.\x.\y.n(m(x y))
(define (pow m n)
  ((fold (lambda (a b) (lambda (x) (* x (b x))))
         (lambda (x) 1)
          n)
    m))

