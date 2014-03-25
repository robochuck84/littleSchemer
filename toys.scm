(define multirember
    (lambda (a lat)
        (cond
            ((null? lat) '())
            ((eq? (car lat) a) (multirember a (cdr lat)))
            (else (cons (car lat) (multirember a (cdr lat)))))))
        

(define multiinsertR
    (lambda (new old lat)
        (cond
            ((null? lat) '())
            ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
            (else (cons (car lat) (multiinsertR new old (cdr lat)))))))
            
(define add1
     (lambda (n)
       (+ n 1)))

(define sub1
    (lambda (n)
        (- n 1)))
        
(define add
    (lambda (n m)
        (cond
            ((zero? n) m)
            (else (add (sub1 n) (add1 m))))))
            
(define tup+
    (lambda (tup1 tup2)
        (cond
            ((null? tup1) tup2)
            ((null? tup2) tup1)
            (else
                (cons (add (car tup1) (car tup2)) 
                    (tup+ (cdr tup1) (cdr tup2)))))))
(define gt
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (gt (sub1 n) (sub1 m))))))

(define lt
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (lt (sub1 n) (sub1 m))))))



(define equals
  (lambda (n m)
    (cond
     ((gt n m) #f)
     ((lt n m) #f)
     (else #t))))

(define pow
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (* n (pow n (sub1 m)))))))

(define sub
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (sub n (sub1 m)))))))

(define divide
  (lambda (n m)
    (cond
     ((lt n m) 0)
     (else (add1 (divide (sub n m) m))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (+ 1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? n) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? n) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((number? (car lat))))))
