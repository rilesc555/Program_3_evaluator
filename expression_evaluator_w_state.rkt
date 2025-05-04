#lang racket

(require data/either)
(require data/monad) ; <-- Required for monadic do* and either-m

(define (safe-div x y st) ; num num -> (either string? num?)
  (if (= y 0)
      (failure (cons "division by zero" st))
      (success (cons (/ x y) st))))

(define in-list? ; item list -> bool
  (λ (x lst)
    (not (false? (member x lst)))))

(define (valid-id? val)
  (define str
    (cond
      [(string? val) val]
      [(symbol? val) (symbol->string val)]
      [else ""])) ; If not string or symbol, treat as invalid

  (if (regexp-match? #rx"^[A-Za-z][A-Za-z0-9_-]*$" str)
      #t
      #f))

(define (eval expr st); expr -> (either string? num?)
  (cond
    ;; handle (num ...)
    [(equal? (first expr) 'num)
     (if (and (= (length expr) 2) (number? (second expr)))
     (values (success (cons (second expr) st)))
     (values (failure "invalid num expression")))]

    ;; handle (id ...)
    [(equal? (first expr) 'id)
     (if (= (length expr) 2)
          (if (and
               (hash-has-key? st (second expr))
               (not (equal? (hash-ref st (second expr)) 'undefined)))
              (success (cons (hash-ref st (second expr)) st))
              (failure "nonexist value"))
     (failure "invalid id expression"))]

    ;; handle define
    [(equal? (first expr) 'define)
     (if (valid-id? (second expr))
      (cond
       [(= (length expr) 2)
         (let* ([var (second expr)]
                [new-st (hash-set st var 'undefined)])
           (success (cons 'undefined new-st)))]
       [(= (length expr) 3)
        (let ([var (second expr)]
              [val-expr (third expr)])
          (do
              [result <- (eval val-expr st)]
            (let* ([val (car result)]
                   [new-st (hash-set (cdr result) var val)])
              (success (cons val new-st)))))]
         (failure (cons "invalid define expression" st)))
      (failure (cons "invalid id name" st)))]

    ;; handle assign
    [(equal? (first expr) 'assign)
     (let ([var (second expr)]
           [val-expr (third expr)])
       (if (and (valid-id? var)
                (hash-has-key? st var)
                (not (equal? (hash-ref st var) 'undefined)))
           (do
             [result <- (eval val-expr st)]
             (let* ([val (car result)]
                    [new-st (hash-set (cdr result) var val)])
               (success (cons val new-st))))
           (failure (cons "assignment to undefined variable" st))))]

    [(equal? (first expr) 'remove)
     (let ([var (second expr)])
       (if (hash-has-key? st var)
           (let ([new-st (hash-remove st var)])
             (success (cons 'undefined new-st)))
           (begin
             (printf "Error: remove ~a: variable not defined, ignoring\n" var)
             (success (cons 'undefined st)))))]

    ;; handle basic operations 
    [(in-list? (first expr) '(div add sub mult))
     (do
       [result1 <- (eval (second expr) st)] ; Bind successful result of (eval...) to x
       [result2 <- (eval (third expr) (cdr result1))]  ; Bind successful result of (eval...) to y

       (let ([x (car result1)]
             [y (car result2)]
             [new_state (cdr result2)])
         (print x)
       ; This body only executes if both evals above were 'success'
       (cond
         [(equal? (first expr) 'add) (success (cons (+ x y) new_state))] ; Wrap result in success
         [(equal? (first expr) 'sub) (success (cons (- x y) new_state))] ; Wrap result in success
         [(equal? (first expr) 'mult) (success (cons (* x y) new_state))]
         [(equal? (first expr) 'div)
          (do
            [div-result <- (safe-div x y new_state)]
            (success div-result))]
         ;; (success (cons (safe-div x y) new_state))] ; safe-div already returns either
         )))]
    

    [else
     (failure "unknown operation")])) ; unknown operation


(eval '(remove b) (hash ' 5))
;; Should return (failure ...)


