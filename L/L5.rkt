(define (find lexem program i)
  (if (equal? (vector-ref program i) lexem)
      i
      (find lexem program (+ 1 i))))

 
(define (interpret program start-stack)
  (let move ((i 0) (stack start-stack) (stack-- '()) (words '()))
    (if (= i (vector-length program))
        stack
        (let ((word (vector-ref program i)))
          (cond
            ((number? word) (move (+ 1 i) (cons word stack) stack-- words))
            ((equal? word '+) (move (+ 1 i) (cons (+ (cadr stack) (car stack)) (cddr stack)) stack-- words))
            ((equal? word '-) (move (+ 1 i) (cons (- (cadr stack) (car stack)) (cddr stack)) stack-- words))
            ((equal? word '*) (move (+ 1 i) (cons (* (cadr stack) (car stack)) (cddr stack)) stack-- words))
            ((equal? word '/) (move (+ 1 i) (cons (quotient (cadr stack) (car stack)) (cddr stack)) stack-- words))
            ((equal? word 'mod) (move (+ 1 i) (cons (remainder (cadr stack) (car stack)) (cddr stack)) stack-- words))
            ((equal? word '>) (move (+ 1 i) (cons (if (> (cadr stack) (car stack)) -1 0) (cddr stack)) stack-- words))
            ((equal? word '<) (move (+ 1 i) (cons (if (< (cadr stack) (car stack)) -1 0) (cddr stack)) stack-- words))
            ((equal? word '=) (move (+ 1 i) (cons (if (= (cadr stack) (car stack)) -1 0) (cddr stack)) stack-- words))
            ((equal? word 'and) (move (+ 1 i) (cons (if (and (= (car stack) -1) (= (cadr stack) -1)) -1 0) (cddr stack)) stack-- words))
            ((equal? word 'or) (move (+ 1 i) (cons (if (or (= (car stack) -1) (= (cadr stack) -1)) -1 0) (cddr stack)) stack-- words))
            ((equal? word 'neg) (move (+ 1 i) (cons (- (car stack)) (cdr stack)) stack-- words))
            ((equal? word 'not) (move (+ 1 i) (cons (if (= (car stack) 0) -1 0) (cdr stack)) stack-- words))
            ((equal? word 'drop) (move (+ 1 i) (cdr stack) stack-- words))
            ((equal? word 'swap) (move (+ 1 i) (append (list (cadr stack) (car stack)) (cddr stack)) stack-- words))
            ((equal? word 'dup) (move (+ 1 i) (cons (car stack) stack) stack-- words))
            ((equal? word 'over) (move (+ 1 i) (cons (cadr stack) stack) stack-- words))
            ((equal? word 'rot) (move (+ 1 i) (append (list (caddr stack) (cadr stack) (car stack)) (cdddr stack)) stack-- words))
            ((equal? word 'depth) (move (+ 1 i) (cons (length stack) stack) stack-- words))
            ((equal? word 'define) (move (+ 1 (find 'end program i)) stack stack--
                                         (cons (list (vector-ref program (+ 1 i)) (+ i 2)) words)))
            ((or (equal? word 'end) (equal? word 'exit)) (move (car stack--) stack (cdr stack--) words))
            ((equal? word 'if) (move (if (zero? (car stack)) (+ 1 (find 'endif program i)) (+ 1 i)) (cdr stack) stack-- words))
            ((equal? word 'endif) (move (+ 1 i) stack stack-- words))
            ((equal? word 'while) (if (zero? (car stack))
                                      (move (+ 1 (find 'endwhile program i)) stack stack-- words)
                                      (move (+ 1 i) stack (cons i stack--) words)))
            ((equal? word 'endwhile) (move (car stack--) stack (cdr stack--) words))
            ((equal? word 'do) (move (+ 1 i) stack (cons i stack--) words))
            ((equal? word 'until) (move (if (zero? (car stack)) (+ 1 i) (car stack--)) stack (cdr stack--) words))
            ((equal? word 'for) (if (<= (car stack) (cadr stack))
                                    (move (+ 1 i) stack (cons i stack--) words)
                                    (move (+ 1 (find 'endfor program i)) (cddr stack) stack-- words)))
            ((equal? word 'endfor) (move (car stack--) (cons (+ 1 (car stack)) (cdr stack)) (cdr stack--) words))
            (else (move (cadr (assoc word words)) stack (cons (+ 1 i) stack--) words)))))))










(interpret #(   define abs 
                 dup 0 < 
                 if neg endif 
                 end 
                 9 abs 
                 -9 abs      ) (quote ()))

(interpret #(   define =0? dup 0 = end
                 define <0? dup 0 < end
                 define signum
                 =0? if exit endif
                 <0? if drop -1 exit endif
                 drop
                 1
                 end
                 0 signum
                 -5 signum
                 10 signum       ) (quote ()))

(interpret #(   define -- 1 - end
                 define =0? dup 0 = end
                 define =1? dup 1 = end
                 define factorial
                 =0? if drop 1 exit endif
                 =1? if drop 1 exit endif
                 dup --
                 factorial
                 *
                 end
                 0 factorial
                 1 factorial
                 2 factorial
                 3 factorial
                 4 factorial     ) (quote ()))

(interpret #(   define =0? dup 0 = end
                 define =1? dup 1 = end
                 define -- 1 - end
                 define fib
                 =0? if drop 0 exit endif
                 =1? if drop 1 exit endif
                 -- dup
                 -- fib
                 swap fib
                 +
                 end
                 define make-fib
                 dup 0 < if drop exit endif
                 dup fib
                 swap --
                 make-fib
                 end
                 10 make-fib     ) (quote ()))

(interpret #(   define =0? dup 0 = end
                 define gcd
                 =0? if drop exit endif
                 swap over mod
                 gcd
                 end
                 90 99 gcd
                 234 8100 gcd    ) '())