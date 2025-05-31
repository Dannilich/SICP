;Задачки по теме "Побочный эффект и присваивание"
#lang racket

;№3.1
(define (make-accumulator value)
  (lambda (income)
    (begin (set! value (+ value income)) value)))

;№3.2
(define (make-monitored f)
  (define calls-count 0)
  (lambda (args)
    (if (equal? args "how-many-calls?")
        calls-count
        (begin (set! calls-count (+ 1 calls-count))
               (f args)))))

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