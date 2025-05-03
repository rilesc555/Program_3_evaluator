#lang racket

(require data/either)
(require data/monad) ; <-- Required for monadic do* and either-m

; either-m is automatically defined when data/either and data/monad are required

(define (safe-div x y) ; num num -> (either string? num?)
  (if (= y 0)
      (failure "division by zero")
      (success (/ x y))))

(define (eval expr) ; expr -> (either string? num?)
  (cond
    [(equal? (first expr) 'num)
     (success (second expr))]

    [(or (equal? (first expr) 'div)
         (equal? (first expr) 'add)
         (equal? (first expr) 'sub)
         (equal? (first expr) 'mult))
     ; Use monadic do* with the either monad (either-m)
     (do
       [x <- (eval (second expr))] ; Bind successful result of (eval...) to x
       [y <- (eval (third expr))]  ; Bind successful result of (eval...) to y
       ; This body only executes if both evals above were 'success'
       (cond
         [(equal? (first expr) 'div) (safe-div x y)] ; safe-div already returns either
         [(equal? (first expr) 'add) (success (+ x y))] ; Wrap result in success
         [(equal? (first expr) 'sub) (success (- x y))] ; Wrap result in success
         [else (success (* x y))] ; Wrap result in success
         ))]

    [else
     (failure "unknown operation")])) ; unknown operation

;; --- Testing ---
(eval '(num 5))
(eval '(add (num 5) (mult (num 2) (num 3))))
(eval '(sub (num 20) (div (add (mult (num 4) (num 5)) (num 10)) (num 6))))
(eval '(div (num 5) (sub (num 5) (num 5))))
(eval '(unknown-op (num 1) (num 2)))
(eval '(add (num 5) (div (num 1) (num 0))))