#lang racket
(provide (all-defined-out))

; problem 1
(define (sequence low high stride)
  (if (> low high) null
      (cons low (sequence (+ low stride) high stride))))

; problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; problem 3
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; problem 4
(define (stream-for-n-steps s n)
  (if (<= n 0) null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; problem 5
(define (funny-number-stream)
  (define (th x)
    (cons (if (= 0 (remainder x 5)) (- x) x)
          (lambda ( ) (th (+ x 1)))))
  (th 1))

; problem 6
(define (dan-then-dog)
  (define (th x)
    (cons (if (even? x) "dog.jpg" "dan.jpg")
          (lambda ( ) (th (+ x 1)))))
  (th 1))

; problem 7
(define (stream-add-zero s)
  (define (th x)
    (cons (cons 0 (car (x)))
          (lambda ( ) (th (cdr (x))))))
  (lambda ( ) (th s)))

; problem 8
(define (cycle-lists xs ys)
  (define (th n)
    (cons (cons (list-nth-mod xs n)
                (list-nth-mod ys n))
          (lambda ( ) (th (+ n 1)))))
  (lambda ( ) (th 0)))

; problem 9
(define (vector-assoc v vec)
  (define (iter n)
    (if (= n (vector-length vec)) #f
        (let ([elem (vector-ref vec n)])
          (cond [(not (pair? elem)) (iter (+ n 1))]
                [(equal? v (car elem)) elem]
                [#t (iter (+ n 1))]))))
  (iter 0))

; problem 10
(define (cached-assoc xs n)
  (let* ([memo (make-vector n #f)]
         [next 0]
         [f (lambda (v)
              (cond [(vector-assoc v memo) (vector-assoc v memo)]
                    [(assoc v xs) (begin
                                    (vector-set! memo next (assoc v xs))
                                    (set! next (remainder (+ next 1) n))
                                    (assoc v xs))]
                    [#t #f]))])
    f))
