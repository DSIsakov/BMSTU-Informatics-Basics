(define-syntax trace-ex
  (syntax-rules ()
    ((xss expr)
     (begin
       (write 'expr)
       (display " => ")
       (let ((val expr))
         (write val)
         (newline)
         val)))))
  



(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss)))) 
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))


(display "task 1")
(newline)
(newline)
(zip '(1 2 3) '(one two three))















(define-syntax test
  (syntax-rules()
    ((test res exp) (list 'res exp))))

(define (run-test xs)
  (write (car xs))
  (let ((var (eval (car xs) (interaction-environment))))
    (if (equal? var (cadr xs))
        (begin
          (display " => ok")
          (newline)
          #t)
        (begin
          (display " => FAIL")
          (newline)
          (display "  Expected: ")
          (write (cadr xs))
          (newline)
          (display "  Returned: ")
          (write var)
          (newline)
          #f))))


(define (run-tests xs)
  (if (null? xs)
      #t
      (if (run-test (car xs))
          (run-tests (cdr xs))
          (begin
            (run-tests (cdr xs))
            #f))))


(newline)
(display "task 2")
(newline)
(newline)

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1) 
    (else     1)))


(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))


(run-tests the-tests)
















(define (get-head xs index)
  (if (= (length xs) index)
      (append xs (list 1))
      (reverse (list-tail (reverse xs) index))))

(define (ref xs index . elem) 
  (if (null? elem)
      (if (list? xs)
          (if (< index (length xs))
              (list-ref xs index)
              #f)
          (if (vector? xs)
              (if (< index (vector-length xs))
                  (vector-ref xs index)
                  #f)
              (if (string? xs)
                  (if (< index (string-length xs))
                      (string-ref xs index)
                      #f)
                  #f)))
      (if (list? xs)
          (if (<= index (length xs))
              (append (reverse (cdr (reverse (get-head xs index)))) elem (list-tail xs index))
              #f)
          (if (vector? xs)
              (if (<= index (vector-length xs))
                  (list->vector (append (reverse (cdr (reverse (get-head (vector->list xs) index))))
                                        elem
                                        (list-tail (vector->list xs) index)))
                  #f)
              (if (string? xs)
                  (if (integer? (car elem))
                      #f
                      (if (<= index (string-length xs))
                          (list->string (append (reverse (cdr (reverse (get-head (string->list xs) index))))
                                                elem
                                                (list-tail (string->list xs) index)))
                          #f))
                      #f)))))
(newline)
(display "task 3")
(newline)
(newline)
(write (ref '(1 2 3) 1))
(newline)
(write (ref #(1 2 3) 1))
(newline)
(write (ref "123" 1))
(newline)
(write (ref "123" 3))
(newline)
(write (ref '(1 2 3) 1 0))
(newline)
(write (ref #(1 2 3) 1 0))
(newline)
(write (ref #(1 2 3) 1 #\0))
(newline)
(write (ref "123" 1 #\0))
(newline)
(write (ref "123" 1 0))
(newline)
(write (ref "123" 3 #\4))
(newline)
(write (ref "123" 5 #\4))
(newline)








(define (factorize exp)
  (let ((a (cadr (cadr exp))) (b (cadr (caddr exp))))
    (if (and (equal? (caddr (cadr exp)) 2) (equal? (car exp) '-))
        (list '*
              (list '+ a b)
              (list '- a b))
        (if (and (equal? (caddadr exp) 3) (equal? (car exp) '-))
            (list '*
                  (list '- a b)
                  (list '+ '(expt ,a 2)
                        (list '-
                              (list '* a b)
                              '(expt ,b 2))))
            (if (and (equal? (caddadr exp) 3) (equal? (car exp) '+))
                (list '*
                      (list '+ a b)
                      (list '- '(expt ,a) 2)
                      (list '+
                            (list '* a b)
                            '(expt ,b 2)))
                exp)))))

(newline)
(display "task 4")
(newline)
(newline)
(factorize '(- (expt x 2) (expt y 2)))
(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
(eval (list (list 'lambda '(x y) (factorize '(- (expt x 2) (expt y 2)))) 1 2) (interaction-environment))

