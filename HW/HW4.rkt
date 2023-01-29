(define memoized-factorial
  (let ((dict '()))
    (lambda (n)
      (if (< n 2)
          n
          (let ((value (assoc n dict)))
            (if value
                (cadr value)
                (let ((curr (* n (memoized-factorial (- n 1)))))
                  (set! dict (cons (list n curr) dict))
                  curr)))))))




(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b)
     (cons a (delay b)))))


(define (lazy-car p)
  (car p))


(define (lazy-cdr p)
  (force (cdr p)))


(define (lazy-head xs k)
  (if (= k 0)
      '()
      (cons (lazy-car xs)
            (lazy-head (lazy-cdr xs) (- k 1)))))


(define (lazy-ref xs k)
  (if (= k 0)
      (lazy-car xs)
      (lazy-ref (lazy-cdr xs) (- k 1))))


(define (naturals n)
  (lazy-cons n
             (naturals (+ n 1))))


(define (factorials)
  (let loop ((f 1) (n 1))
    (lazy-cons (* f n)
               (loop (* f n) (+ n 1)))))


(define (lazy-factorial n)
  (list-ref (lazy-head (factorials)
                       n)
            (- n 1)))
      
      


(define (read-words)
  (let next ((words '()) (word "") (c (read-char)))
    (if (not (eof-object? c))
        (begin 
          (if (not (or (equal? c #\⋅)
                       (equal? c #\.)
                       (equal? c #\space)
                       (equal? c #\¶)
                       (equal? c #\newline)
                       (equal? c #\return)))
              (set! word (string-append word (make-string 1 c)))
              (if (not (equal? word ""))
                  (begin
                    (set! words (append words (list word)))
                    (set! word ""))))
          (next words word (read-char)))
        words)))




(define-syntax define-struct
  (syntax-rules ()
    ((define-struct struct (field1 ...))
     (begin
       (eval (list 'define
                   (string->symbol (string-append "make-" (symbol->string 'struct)))
                   (lambda (field1 ...)
                     (list (list 'type 'struct) (list 'field1 field1) ...)))
             (interaction-environment))
       (eval (list 'define
                   (string->symbol (string-append (symbol->string 'struct) "?"))
                   (lambda (obj)
                     (and (list? obj)
                          (not (null? obj))
                          (pair? (car obj))
                          (let ((res (assoc 'type obj)))
                            (and res (equal? (cadr res) 'struct))))))
             (interaction-environment))
       (eval (list 'define
                   (string->symbol (string-append (symbol->string 'struct)
                                                  "-"
                                                  (symbol->string 'field1)))
                   (lambda (x)
                     (cadr (assoc 'field1 (cdr x)))))
             (interaction-environment)) ...
       (eval (list 'define
                   (string->symbol (string-append "set-"
                                                  (symbol->string 'struct)
                                                  "-"
                                                  (symbol->string 'field1) "!"))
                   (lambda (obj value)
                     (set-car! (cdr (assoc 'field1 (cdr obj))) value)))
             (interaction-environment)) ... ))))




(define-syntax define-data
  (syntax-rules ()
    ((define-data d-name ((type field1 ...) ...))
     (begin
       (eval (list 'define
                   'type
                   (lambda (field1 ...)
                     (list (list 'd-name 'data)
                           (list 't-name 'type)
                           (list 'field1 field1) ...)))
             (interaction-environment)) ...
       (eval (list 'define
                   (string->symbol (string-append (symbol->string 'd-name) "?"))
                   (lambda (obj)
                     (and (list? obj)
                          (> (length obj) 1)
                          (pair? (car obj))
                          (let ((d-nameres (assoc 'd-name obj)))
                               (and d-nameres (equal? (cadr d-nameres) 'data))))))
             (interaction-environment))))))


(define-syntax match
  (syntax-rules ()
    ((match obj ((type field1 ...) expr) ... )
     (cond
       ((equal? (cadadr obj) 'type)
        (let ((field1 (cadr (assoc 'field1 obj))) ...) expr))
       ...
       (else obj)))))


