(define top 0)


(define-syntax use-assertations
  (syntax-rules ()
    ((use-assertations) (call-with-current-continuation (lambda (stack) (set! top stack))))))


(define-syntax assert
  (syntax-rules ()
    ((assert expr) (if (not expr)
                       (begin (display "FAILED: ")
                              (display 'expr)
                              (newline)
                              (top))))))


(use-assertations)


(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))


(newline)
(newline)
(display "task 1")
(newline)
(newline)
(map 1/x '(1 2 3 4 5))
(map 1/x '(-2 -1 0 1 2))









(define (save-data data path)
  (with-output-to-file path
    (lambda ()
      (write data))))


(define (load-data path)
  (let*  ((port (open-input-file path))
          (value (read port)))
    (close-input-port port)
    value))


(define (count path)
  (if (input-port? path)
      (begin (let ((c (read-char path)))
               (if (eof-object? c)
                   0
                   (if (or (equal? c #\newline)
                           (equal? c #\return))
                       (let ((next-c (peek-char path)))
                         (if (eof-object? next-c)
                             0
                             (if (or (equal? next-c #\newline)
                                     (equal? next-c #\return))
                                 (+ 1 (count path))
                                 (count path))))
                       (count path))))
             )
      (let* ((port (open-input-file path))
             (c (read-char port)))
        (if (eof-object? c)
            0
            (if (or (equal? c #\newline)
                    (equal? c #\return))
                (let ((next-c (peek-char port)))
                  (if (eof-object? next-c)
                      0
                      (if (or (equal? next-c #\newline)
                              (equal? next-c #\return))
                          (count port)
                          (+ 1 (count port)))))
                (count port))))))



(newline)
(newline)
(display "task 2")
(newline)
(newline)
(count "Lab-4.rkt")










(define trib
  (let ((prev '((2 1) (1 0) (0 0))))
    (lambda (n)
      (let ((key (assoc n prev)))
        (if key
            (cadr key)
            (let ((curr (+ (trib (- n 1)) (trib (- n 2)) (trib (- n 3)))))
              (begin (set! prev (cons
                                 (list n curr)
                                 prev))
                     curr)))))))


(newline)
(newline)
(display "task 3")
(newline)
(newline)
(trib 20)










(define-syntax my-if
  (syntax-rules ()
    ((my-if cond? t f)
     (force (or (and cond? (delay t)) (delay f))))))



(newline)
(newline)
(display "task 4")
(newline)
(newline)
(my-if #t 1 (/ 1 0))
(my-if #f (/ 1 0) 1)





(define-syntax my-let
  (syntax-rules ()
    ((my-let ((name1 value1) ...) expr1 ...)
     ((lambda (name1 ...) expr1 ...) value1 ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr1 ...)
     (my-let () expr1 ...))
    ((my-let* ((name1 value1)) expr1 ...)
     (my-let ((name1 value1)) expr1 ...))
    ((my-let* ((name1 value1) (name2 value2) ...) expr1 ...)
     (my-let ((name1 value1))
             (my-let* ((name2 value2) ...) expr1 ...)))))




(newline)
(newline)
(display "task 5")
(newline)
(newline)
(my-let () "jgewrafksehgkr")
(my-let ((x 23498)) x)
(my-let* () "jgewrafksehgkr")
(my-let* ((x 23498)) x)






(define-syntax when
  (syntax-rules ()
    ((when cond? expr1 ...)
     (if cond? (begin expr1 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless cond? expr1 ...)
     (if (not cond?) (begin expr1 ...)))))


(define-syntax for
  (syntax-rules (in as)
    ((for i in xs expr1 ...)
     (let loop ((vals xs))
       (if (not (null? vals))
           (let ((i (car vals)))
             expr1 ...
             (loop (cdr vals))))))
    ((for xs as i expr1 ...)
     (for i in xs expr1 ...))))


(define-syntax while
  (syntax-rules ()
    ((while cond? expr1 ...)
     (let loop ()
       (if cond? (begin expr1 ... (loop)))))))


(define-syntax repeat
  (syntax-rules (until)
    ((repeat (expr1 ...) until cond?)
     (let loop ()
       expr1 ...
       (if (not cond?) (loop))))))


(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl)
     (newline))
    ((cout << endl . expr)
     (begin (newline)
            (cout . expr)))
    ((cout << expr1)
     (display expr1))
    ((cout << expr1 . expr)
     (begin (display expr1)
            (cout . expr)))))


(newline)
(newline)
(display "task 6a")
(newline)
(newline)
(define x 1)
(when   (> x 0) (display "x > 0")  (newline))
(unless (= x 0) (display "x != 0") (newline))
(newline)
(newline)
(display "task 6b")
(newline)
(newline)
(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))
(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))
(newline)
(newline)
(display "task 6c")
(newline)
(newline)
(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))
(newline)
(newline)
(display "task 6d")
(newline)
(newline)
(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))
(newline)
(newline)
(display "task 6e")
(newline)
(newline)
(cout << "a = " << 1 << endl << "b = " << 2 << endl)