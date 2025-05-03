#lang racket

(require data/either) ; Use the built-in either library

; safe-div now returns (right num) or (left string)
; Note: It doesn't handle state directly; eval will manage state around it.
(define (safe-div x y)  ; num num -> (either string number)
  (if (= y 0)
      (failure "division by zero")
      (success (/ x y))))

; Keep in-list? unchanged
(define in-list?
  (λ (x lst)
    (not (false? (member x lst)))))

; Keep valid-id? unchanged
(define (valid-id? id)
  (and (symbol? id)
       (let ([str (symbol->string id)])
         (and (> (string-length str) 0)
              (char-alphabetic? (string-ref str 0))
              (for/and ([c (string->list str)])
                (or (char-alphabetic? c)
                    (char-numeric? c)
                    (char=? c #\_)
                    (char=? c #\-)))))))

; eval function using data/either
; Returns: (right (list value state)) or (left (list error-message state))
(define (eval expr state)
  (cond
    ; Number literal
    [(equal? (first expr) 'num)
     (success (list (second expr) state))]

    ; Variable reference
    [(equal? (first expr) 'id)
     (let ([var-name (second expr)]
           [lookup (assoc var-name state)])
       (cond
         [(not lookup)
          (failure (list (format "variable '~a' not found" var-name) state))]
         [(eq? (cdr lookup) 'undefined)
          (failure (list (format "variable '~a' is undefined" var-name) state))]
         [else
          (success (list (cdr lookup) state))]))]

    ; Variable definition
    [(equal? (first expr) 'define)
     (let ([var-name (second expr)])
       (cond
         [(not (valid-id? var-name))
          (failure (list (format "'~a' is not a valid identifier" var-name) state))]
         [(assoc var-name state)
          (failure (list (format "variable '~a' already defined" var-name) state))]
         [(= (length expr) 2) ; Just declare without value
          (success (list 'undefined (cons (cons var-name 'undefined) state)))]
         [else ; Declare with value from an expression
          (let ([result (eval (third expr) state)])
            (if (failure? result)
                result ; Propagate failure (already has state)
                (let* ([value (first (right-value result))]
                       ; State might have changed during sub-evaluation
                       [new-state (second (right-value result))] 
                       ; Add the new definition to the potentially updated state
                       [final-state (cons (cons var-name value) new-state)])
                  (right (list value final-state)))))]))]

    ; Variable assignment
    [(equal? (first expr) 'assign)
     (let* ([var-name (second expr)])
       (if (not (assoc var-name state))
           (left (list (format "variable '~a' not defined for assignment" var-name) state))
           ; Evaluate the expression first
           (let ([result (eval (third expr) state)])
             (if (left? result)
                 result ; Propagate failure
                 (let* ([new-val (first (right-value result))]
                        ; State might have changed during sub-evaluation
                        [current-state (second (right-value result))] 
                        ; Update the variable in the potentially changed state
                        [final-state (map (lambda (pair)
                                            (if (equal? (car pair) var-name)
                                                (cons var-name new-val)
                                                pair))
                                          current-state)])
                   (right (list new-val final-state)))))))]

    ; Remove variable
    [(equal? (first expr) 'remove)
     (let* ([var-name (second expr)])
       (if (not (assoc var-name state))
           (begin
             (displayln (format "Error: remove ~a: variable not defined, ignoring" var-name))
             (right (list 'ok state))) ; Not a failure, return original state
           (let ([new-state (filter (lambda (p) (not (equal? (car p) var-name))) state)])
             (right (list 'ok new-state)))))] ; Return updated state

    ; Arithmetic operations
    [(in-list? (first expr) '(div add sub mult))
     ; Evaluate the first operand
     (let ([x-result (eval (second expr) state)])
       (if (left? x-result)
           x-result ; Propagate failure
           ; If first operand succeeded, evaluate the second
           (let* ([x (first (right-value x-result))]
                  [state-after-x (second (right-value x-result))]
                  [y-result (eval (third expr) state-after-x)])
             (if (left? y-result)
                 y-result ; Propagate failure
                 ; If both operands succeeded
                 (let* ([y (first (right-value y-result))]
                        [final-state (second (right-value y-result))])
                   ; Perform the operation
                   (cond
                     [(equal? (first expr) 'div)
                      (let ([div-result (safe-div x y)])
                        (if (left? div-result)
                            ; Wrap the error from safe-div with the state
                            (left (list (left-value div-result) final-state))
                            ; Wrap the value from safe-div with the state
                            (right (list (right-value div-result) final-state))))]
                     [(equal? (first expr) 'add)
                      (right (list (+ x y) final-state))]
                     [(equal? (first expr) 'sub)
                      (right (list (- x y) final-state))]
                     [else ; 'mult
                      (right (list (* x y) final-state))]))))))]

    ; Unknown operation
    [else (left (list (format "unknown operation: ~a" (first expr)) state))]))


; REPL implementation - updated for data/either structure
(define (repl)
  (let loop ([current-state '()]) ; Start with empty state
    (display "\n>> ")
    (flush-output)
    (let ([input (read)])
      (cond
        [(or (equal? input 'quit) (equal? input '(quit)))
         (displayln "Goodbye!")]
        [else
         (let ([result (eval input current-state)])
           (if (left? result)
               (let ([error-msg (first (left-value result))]
                     [state-at-failure (second (left-value result))])
                 (displayln (format "Failure: ~a" error-msg))
                 (displayln (format "State: ~a" state-at-failure))
                 (loop state-at-failure)) ; Continue with the state as it was before the failed op
               (let ([value (first (right-value result))]
                     [next-state (second (right-value result))])
                 (displayln (format "Success: ~a" value))
                 (displayln (format "State: ~a" next-state))
                 (loop next-state))))])))) ; Continue with the updated state

; Start the REPL
(repl)
