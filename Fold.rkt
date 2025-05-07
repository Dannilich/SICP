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

;№2.38
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


;№2.33
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


;№2.39
;Реализация переворота списка через fold правый и левый
(define (reverse-foldl lst)
  (foldl (lambda (accum x) (cons x accum))
         '()
         lst))

;Через foldr никак не иначе через добавление в конец
(define (snoc l a) 
  (if (null? l)
      (cons a '())
      (cons (car l) (snoc (cdr l) a))))

(define (reverse-foldr lst)
  (foldr (lambda (x accum) (snoc accum x))
         '()
         lst))


;№2.34
;Реализация вычисления многочлена в точке через схему Горнера
(define (horners-scheme poly x)
  (foldl (lambda (accum coef) (+ (* accum x) coef))
         0
         poly))

;Проба другим способом
(define (anouther-poly-eval poly  x)
  (car (foldl (lambda (accum coef) (cons (+ (car accum)  (* coef (expt x (cdr accum)))) (- (cdr accum) 1)))
         (cons 0 (- (length poly) 1))
         poly)))


;№2.35
;Подсчёт листьев дерева
(define (leafs tree)
  (if (null? tree)
      '()
      (if (pair? tree)
          (append (leafs (car tree)) (leafs (cdr tree)))
          (list tree))))

(define (leafs-count tree)
  (foldr (lambda (x accum) (+ (length x) accum))
         0
         (map leafs tree)))
