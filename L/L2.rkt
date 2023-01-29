(define (count x xs)
  (if (null? xs)
      0
      (if (Eqv? (car xs) x)
          (+ 1 (count x (cdr xs)))
          (count x (cdr xs)))
      ))


(define (delete pred? xs)
  (if (null? xs)
      xs
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (append (list (car xs)) (delete pred? (cdr xs))))))


(define (iterate f x n)
  (if (= n 0)
      (list)
      (append (list x) (iterate f (f x) (- n 1)))))


(define (intersperse e xs)
  (if (null? xs)
      xs
      (if (null? (cdr xs))
          xs
          (append (list (car xs) e) (intersperse e (cdr xs))))))


(define (any? pred? xs)
  (if (null? xs)
      #f
      (if (pred? (car xs))
          #t
          (any? pred? (cdr xs)))))


(define (all? pred? xs)
  (if (null? xs)
      #t
      (if (pred? (car xs))
          (any? pred? (cdr xs))
          #f)))


(define (o . oper)
  (if (null? oper)
      (lambda (x) x)
      (lambda (x)
        ((car oper) ((apply o (cdr oper)) x))
        )
      )
  )