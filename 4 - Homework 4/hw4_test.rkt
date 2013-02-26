#lang racket

(require rackunit "hw4.rkt")

; problem 1 tests
(check-equal? (sequence 0  5 1) '(0 1 2 3  4 5) "sequence #1")
(check-equal? (sequence 3 11 2) '(3 5 7 9 11)   "sequence #2")
(check-equal? (sequence 3  8 3) '(3 6)          "sequence #3")
(check-equal? (sequence 3  2 1) '( )            "sequence #4")

; problem 2 tests
(check-equal? (string-append-map '("a" "b" "c") "-1")
              '("a-1" "b-1" "c-1") "string-append-map #1" )
(check-equal? (string-append-map '("a") "-1")
              '("a-1") "string-append-map #2" )
(check-equal? (string-append-map null "-1")
              '( ) "string-append-map #3" )

; problem 3 tests
(check-equal? (list-nth-mod '("a" "b" "c") 0) "a" "list-nth-mod #1")
(check-equal? (list-nth-mod '("a" "b" "c") 2) "c" "list-nth-mod #2")
(check-equal? (list-nth-mod '("a" "b" "c") 4) "b" "list-nth-mod #3")

(check-exn (regexp "list-nth-mod: negative number") 
           (lambda ( ) (list-nth-mod '("a" "b" "c") -1))
           "error 'list-nth-mod: negative number' not thrown #4")
(check-exn (regexp "list-nth-mod: empty list") 
           (lambda ( ) (list-nth-mod '( ) 0))
           "error 'list-nth-mod: empty list' not thrown #5")

; problem 4 tests
(define (natural-numbers)
  (define (th x)
    (cons x (lambda ( ) (th (+ x 1)))))
  (th 1))

(check-equal? (stream-for-n-steps natural-numbers 5)
              '(1 2 3 4 5) "stream-for-n-steps #1")
(check-equal? (stream-for-n-steps natural-numbers 10)
              '(1 2 3 4 5 6 7 8 9 10) "stream-for-n-steps #2")

; problem 5 tests
(check-equal? (stream-for-n-steps funny-number-stream 16)
              '(1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16)
              "funny-number-stream #1")
(check-equal? (stream-for-n-steps funny-number-stream 0)
              '( ) "funny-number-stream #2")
(check-equal? (stream-for-n-steps funny-number-stream 1)
              '(1) "funny-number-stream #3")

; problem 6 tests
(check-equal? (stream-for-n-steps dan-then-dog 4)
              '("dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg")
              "dan-then-dog #1")
(check-equal? (stream-for-n-steps dan-then-dog 1)
              '("dan.jpg") "dan-then-dog #2")
(check-equal? (stream-for-n-steps dan-then-dog 2)
              '("dan.jpg" "dog.jpg") "dan-then-dog #3")

; problem 7 tests
(check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 2)
              '((0 . "dan.jpg") (0 . "dog.jpg"))
              "stream-add-zero #1")
(check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 4) 
              '((0 . "dan.jpg") (0 . "dog.jpg") (0 . "dan.jpg") (0 . "dog.jpg"))
              "stream-add-zero #2")
(check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 0) 
              '( ) "stream-add-zero #3")

; problem 8 tests
(check-equal? (stream-for-n-steps (cycle-lists '(1 2 3) '("a" "b")) 4) 
              '((1 . "a") (2 . "b") (3 . "a") (1 . "b"))
              "cycle-lists #1")
(check-equal? (stream-for-n-steps (cycle-lists '(1 2 3 4) '(5 6 7)) 6)
              '((1 . 5) (2 . 6) (3 . 7) (4 . 5) (1 . 6) (2 . 7))
              "cycle-lists #2")

; problem 9 tests
(define vec '((1 . "a") (2 . "b") (3 . "c") (4 . "d") (5 . "e")))

(check-equal? (vector-assoc 5 (list->vector vec))
              (cons 5 "e") "vector-assoc #1")
(check-equal? (vector-assoc 3 (list->vector vec))
              (cons 3 "c") "vector-assoc #2")
(check-equal? (vector-assoc 6 (list->vector vec))
              #f "vector-assoc #3")
(check-equal? (vector-assoc 5 (list->vector '(1 2 3 4 5))) 
              #f "vector-assoc #4" )
(check-equal? (vector-assoc 7 (list->vector '(1 2 3 4 5 (7 . 8)))) 
              (cons 7 8) "vector-assoc #5" )
(check-equal? (vector-assoc 3 (list->vector '(1 2 (3 . 7) 4 5 (7 . 8)))) 
              (cons 3 7) "vector-assoc #6" )

; problem 10 tests
(define ctf (cached-assoc '((1 . 2) (3 . 4) (5 . 6) (7 . 8) (9 . 10)) 3 ))

(check-equal? (ctf  3) (cons 3  4) "cached-assoc #1")
(check-equal? (ctf  5) (cons 5  6) "cached-assoc #2")
(check-equal? (ctf  9) (cons 9 10) "cached-assoc #3")
(check-equal? (ctf 11) #f          "cached-assoc #4")
