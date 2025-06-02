;Решение задач по теме "Побочный эффект и присваивание"
#lang racket
(require r5rs)
(print-as-expression #f)
(print-mpair-curly-braces #f)


;№3.1
(define (make-accumulator value)
  (lambda (income)
    (begin (set! value (+ value income)) value)))

;№3.2
(define (make-monitored f)
  (define calls-count 0)
  (lambda (args)
    (cond [ (equal? args 'how-many-calls?) calls-count ]
               [(equal? args 'reset-count) (set! calls-count 0) 0]
               [else (set! calls-count (+ 1 calls-count)) (f args)])))

;№3.3, 3.4, 3.7
(define (make-account balance acc-password)

  (define false-password-series 0)
  (define (call-the-cops) (error "*** Звуки полицейской сирены ***"))

  
  (define (dispatch entered-password comand)
    (if (equal? entered-password acc-password)
        (begin (set! false-password-series 0)
                   (cond [(equal? comand "withdraw")  withdraw ]
                             [(equal? comand "deposit") deposit ]
                             [else  (error "Неизвестная команда: " false-password-series)]
                         ))
        (if (>= false-password-series 7)
            call-the-cops
            (begin
              (set! false-password-series (+ 1 false-password-series))  (error "Неправильный пароль! \nПопыток: " false-password-series))))
    )
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        (error "Недостаточно денег")))

  (define (deposit amount)
    (begin (set! balance (+ balance amount)) balance))

  
  dispatch
  )


(define (make-joint base-acc password-of-base-acc password-of-new-acc)
  (lambda (password comand)
    (if (equal? password-of-new-acc password )
          (base-acc password-of-base-acc comand)
           (error "Неправильный пароль!"))
    )
  )


;Реализовать версии с побочным эф.
(define (snoc! lst x)
  (cond [(null? lst) (error "Список не должен быть пустым")]
             [(null? (cdr lst)) (set-cdr! lst (cons x '()))]
             [else (snoc! (cdr lst) x)])
  )

(define (append! list1 list2)
  (when (not(null? list2))
              (cond [(null? list1)  (error "1-ый cписок не должен быть пустым")]
                         [(null? (cdr list1)) (set-cdr! list1 (cons (car list2) '())) (append! (cdr list1) (cdr list2)) ]
                         [else (append! (cdr list1) list2)])
       )  
  )

(define (reverse! l)
  (define (loop i j)
    (if (null? i)
        j
        (let ([k (cdr i)])
          (set-cdr! i j)
          (loop k i))))
  (loop l null))

(define (delete! x lst)
  (cond [(or (null? lst) (null? (cdr lst))) (void)]
             [(equal? (cadr lst) x) (set-cdr! lst (cddr lst)) ]
             [else (display (cddr lst))(delete! (cdr lst) x)])
 )
#|
(define a (list 1 2 3 2))
(define b (list 4 5))

(delete! 5 b)
b
(delete! 2 a)
a
(delete! 2 a)
a
|#
