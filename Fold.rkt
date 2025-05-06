;Задания по функции Fold 
#lang racket

 
(define (fold f a n)
  (if (zero? n)
      a
      (f (- n 1) (fold f a (- n 1)))))


;Функция многократного применения (или как кратность применения или как нумерал Черча)
(define (apply-n f x n)
  (if (zero? n)
      x
      (apply-n f (f x) (- n 1))))

;Реализуем через неё fold
(define (fold-by-apply-n f a n)
  (define (accum pair)
    (cons (+ 1 (car pair)) (f (car pair) (cdr pair))))
  (cdr (apply-n accum (cons 0 a) n)))


;Реализация функций через fold

;Факториал
(define (factorial n)
  (fold (lambda (x y) (if (zero? x) 1 (* (+ 1 x) y)))
        1
        n))

;Возведение в степень
;Выражено с помощью лямбда исчисления: \n.\m.\x.\y.n(m(x y))
(define (pow m n)
  ((fold (lambda (a b) (lambda (x) (* x (b x))))
         (lambda (x) 1)
          n)
    m))


;Правая и левая свертка

;Реализация хвостовой рекурсией
(define (reversed list)
  (define (iter list-1 list-2)
    (if (null? list-1)
        list-2
        (iter (cdr list-1) (cons (car list-1) list-2))))
  (iter list '() ))

(define (foldl f x0 list)
  (define (iter accum l)
    (if (null? l)
        accum
        (iter (f accum (car l)) (cdr l))))
  (iter x0 list))

(define (foldr f x0 list)
  (define (iter accum l)
    (if (null? l)
        accum
        (iter (f (car l) accum) (cdr l))))
  (iter x0 (reversed list)))


;Реаллизация нехвостовой рекусрсией
(define (foldr-rec f x0 list)
  (if (null? list)
      x0
      (f (car list) (foldr-rec f x0 (cdr list)))))


;Реализация через лев. и прав. свертки функций
(define (length list)
  (foldr (lambda (x counter) (+ 1 counter))
         0
         list))

(define (map f lst)
  (foldr (lambda (x accum) (append (list (f x)) accum))
         '()
         lst))

(define (filter predicate lst)
  (foldr (lambda (x accum) (if (predicate x) (append (list x) accum) accum))
         '()
         lst))

(define (horners-scheme poly  x)
  (car (foldl (lambda (accum coef) (cons (+ (car accum)  (* coef (expt x (cdr accum)))) (- (cdr accum) 1)))
         (cons 0 (- (length poly) 1))
         poly)))