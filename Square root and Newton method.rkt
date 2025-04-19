;Построение абстракций на примере численных методов
#lang racket


;Вычисление квадратного корня, через достаточную погрешность
;Как метод Герона Александрийского

;По заданию
(define epsilon 0.001)
(define (square x) (* x x))
(define (close-enough? x y)(< (abs (- x y)) epsilon))
(define (average x y) (/ (+ x y) 2))
(define (improve y x) (average y (/ x y)))

(define (sqrt-loop y x)
        (if (close-enough? (square y) x)
            y
            (sqrt-loop (improve y x) x)))

(define (sqrt x) (sqrt-loop 1.0 x))

;По SICP
(define (average-damp f) (lambda (x)(average x (f x)))) ;Торможние среднем
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

(define (sqrt-fixed-point x)
  (fixed-point (average-damp (lambda (y) (/ x y))) x))



;SCIP №1.7,
;вычисление кв. корня через фундаментальную последовательность (достаточно малая разность 2-х членов подряд)
(define (sqrt-fundamental-sequence x)
  (define (iter first-member second-member)
    (if (< (abs (- first-member second-member)) epsilon)
        second-member
        (iter second-member (improve second-member x))))
  (iter 1.0 (improve 1.0 x)))
;По итогу, при eps = 0.001, выигрывает этот подход в точности везде (и на малых 0.3 и больших 99999)





    
