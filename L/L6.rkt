;<цифра>:='1'|'2'|...|'0'
;<число>:=<цифра>|<число><цифра>
;<знак>:='+'|'-'
;<числитель>:=<число>|<знак><число>
;<знаменатель>:=<число>
;<дробь>:=<числитель>'/'<знаменатель>
;<пробел>:=' '|'\n'|'\t'
;<промежуток>:=<пробел>|<промежуток><пробел>
;<конец>:='?'
;<последовательность>:=<дробь><последовательность>|<промежуток><последовательность>|<конец>



(define (check-slash lstr)
  (if (null? lstr)
      -1
      (if (equal? (car lstr) #\/)
          0
          (+ 1 (check-slash (cdr lstr))))))


(define (check-num start lstr num)
  (if (null? lstr)
      num
      (if (< start 0)
          (if (char-numeric? (car lstr))
              (check-num -1 (cdr lstr) (append num (list (car lstr))))
              num)
          (check-num (- start 1) (cdr lstr) num))))
          

(define (check-frac str)
  (let ((lstr (string->list str)))
    (let ((index (check-slash lstr)))
      (if (equal? index -1)
          #f
          (let ((first (car lstr)) (sign 1) (signed 0))
            (if (equal? first #\-)
                (set! sign -1))
            (if (or (equal? first #\-)
                    (equal? first #\+))
                (set! signed 1))
            (let ((numerator '()) (denominator '()))
              (set! numerator (check-num (+ -1 signed) lstr '()))
              (set! denominator (check-num index lstr '()))
              ;     (display numerator) (newline)
              ;     (display denominator) (newline)
              ;     (display lstr) (newline) (newline)
             
              (and (equal? (+ (length numerator)
                              (length denominator)
                              (+ 1 signed))
                           (length lstr))
                   (> (length denominator) 0)
                   (> (length numerator) 0))))))))



(define (scan-frac str)
  (let ((lstr (string->list str)))
    (let ((index (check-slash lstr)))
      (if (equal? index -1)
          #f
          (let ((first (car lstr)) (sign 1) (signed 0))
            (if (equal? first #\-)
                (set! sign -1))
            (if (or (equal? first #\-)
                    (equal? first #\+))
                (set! signed 1))
            (let ((numerator '()) (denominator '()))
              (set! numerator (check-num (+ -1 signed) lstr '()))
              (set! denominator (check-num index lstr '()))
              ;     (display numerator) (newline)
              ;     (display denominator) (newline)
              ;     (display lstr) (newline) (newline)
              (if (and (equal? (+ (length numerator)
                                  (length denominator)
                                  (+ 1 signed))
                               (length lstr))
                       (> (length denominator) 0)
                       (> (length numerator) 0))
                  (/ (* sign
                        (string->number (list->string numerator)))
                     (string->number (list->string denominator)))
                  #f)))))))


(define (srez lstr start end)
  (if (equal? end -1)
      '()
      (if (> start 0)
          (srez (cdr lstr) (- start 1) (- end 1))
          (if (> end 0)
              (append (list (car lstr)) (srez (cdr lstr) start (- end 1)))
              (list (car lstr))))))
                                    


(define (split init-lstr lstr start current)
  (if (null? lstr)
      (if (equal? start current)
          '()
          (list (srez init-lstr start (- current 1))))
      (if (or (equal? (car lstr) #\newline)
              (equal? (car lstr) #\space)
              (equal? (car lstr) #\tab))
          (append (list (srez init-lstr start (- current 1)))
                  (split init-lstr (cdr lstr) (+ current 1) (+ current 1)))
          (split init-lstr (cdr lstr) start (+ current 1)))))


(define (scan-many-fracs str)
  (let ((init-lstr (string->list str)) (flag 1))
    (if (null? init-lstr)
        '()
        (let ((init-fracs (split init-lstr init-lstr 0 0)))
          (let rec ((fracs init-fracs))
            (if (null? fracs)
                '()
                (if (null? (car fracs))
                    (rec (cdr fracs))
                    (let ((frac (scan-frac (list->string (car fracs)))))
                      (if (not frac)
                          (set! flag 0))
                      (let ((result (append (list frac) (rec (cdr fracs)))))
                        (if (equal? flag 0)
                            #f
                            result))))))))))
                        
                           
                      
                                           
                                       
          
          
              
        
                  
            

(check-frac "110/111")
(check-frac "-4/3")    
(check-frac "+5/10")   
(check-frac "5.0/10")  
(check-frac "FF/10")
(newline)
(newline)
(scan-frac "110/111") 
(scan-frac "-4/3")    
(scan-frac "+5/10")    
(scan-frac "5.0/10")   
(scan-frac "FF/10")
(newline)
(newline)
(scan-many-fracs
 "\t1/2 1/3\n\n10/8")
(scan-many-fracs
 "\t1/2 1/3\n\n2/-5")
(newline)
(newline)







(define return 0)


(define (get-tail xs target)
  (if (equal? (car xs) target)
      (cdr xs)
      (get-tail (cdr xs) target)))


(define (get-head xs target)
  (let loop ((xs xs) (acc '()))
    (if (null? xs)
        #f
        (if (equal? (car xs) target)
            acc
            (loop (cdr xs) (append acc (list (car xs))))))))


(define (tail-endif lprogram)
  (let loop ((lprogram lprogram) (depth -1))
    (if (null? lprogram)
        #f
        (let ((word (car lprogram)))
          (cond
            ((and (equal? word 'endif) (equal? depth 0)) (cdr lprogram))
            ((equal? word 'endif) (loop (cdr lprogram) (- depth 1)))
            ((equal? word 'if) (loop (cdr lprogram) (+ depth 1)))
            (else (loop (cdr lprogram) depth)))))))


(define (head xs n)
  (if (or (equal? n -1) (null? xs))
      '()
      (cons (car xs)
            (head (cdr xs) (- n 1)))))


(define (parse-articles lprogram)
  (let loop ((lprogram lprogram))
    (if (not (null? lprogram))
        (let ((word (car lprogram)) (other (cdr lprogram)))
          (if (equal? word 'define)
              (if (null? other)
                  (return #f)
                  (if (member (car other) '(if endif end define))
                      (return #f)
                      (let ((head (get-head (cdr other) 'end)))
                        (if (not head)
                            (return #f)
                            (cons (cons (car other) (list (parse-body head)))
                                  (loop (get-tail (cdr other) 'end)))))))
              (list lprogram)))
        (list lprogram))))


(define (parse-body lprogram)
  (let loop ((lprogram lprogram) (parsed '()) (stack '()))
    (if (not (null? lprogram))
        (let ((word (car lprogram)))
          (cond
            ((equal? word 'if)
             (let ((tail (tail-endif lprogram)))
               (if (not tail)
                   (return #f)
                   (loop tail (append parsed (list (list 'if (loop (cdr lprogram) '() (cons 'if stack))))) stack))))
            ((equal? word 'endif)
             (if (and (not (null? stack)) (equal? (car stack) 'if))
                 parsed
                 (return #f)))
            ((member word '(define end)) (return #f))
            (else (loop (cdr lprogram) (append parsed (list word)) stack))))
        parsed)))


(define (parse program)
  (call-with-current-continuation
   (lambda (stack)
     (set! return stack)
     (let ((lprogram (vector->list program)))
       (if (null? lprogram)
           '(() ())
           (if (equal? (car lprogram) 'define)
               (let ((articles (parse-articles lprogram)))
                 (cons (head articles (- (length articles) 2))
                       (list (parse-body (list-ref articles (- (length articles) 1))))))
           
               (cons '() (list (parse-body lprogram)))))))))











(define (print expr)
  (newline)
  (display expr) (newline)
  (eval expr (interaction-environment)))


(print '(parse #(1 2 +)))
(print '(parse #(x dup 0 swap if drop -1 endif)))
(print '(parse #( define -- 1 - end
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
          4 factorial )))
(print '(parse #(define word w1 w2 w3)))