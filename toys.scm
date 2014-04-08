(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? (car lat) a) (member? a (cdr lat)))))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? (car lat) a) (member? a (cdr lat)))))))

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

(define *
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (add n (* n (sub1 m)))))))
            
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

(define %
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
     ((null? lat) '())
     ((number? (car lat))
      (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? a (car lat)) (+ 1 (occur a (cdr lat))))
     (else (+ 0 (occur a (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond 
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? a (car l)) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
     (else
      ((cons (insertR* new old (car l)) (insertR* new old (cdr l))))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (+ 1 (occur* a (cdr l))))
       (else (+ 0 (occur* a (cdr l))))))
     (else
      (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) #t)
     ((or (atom? s1) (atom? s2)) #f)
     (else
      (and (eqlist? s1 s2))))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp)))))))))

(define 1st-sub-sexp
  (lambda (sexp)
    (car (cdr sexp))))

(define 2nd-sub-sexp
  (lambda (sexp)
    (car (cdr (cdr sexp)))))

(define operator
  (lambda (sexp)
    (car sexp)))

<<<<<<< HEAD
(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) +)
     ((eq? x '-) -)
     (else  pow))))

=======
>>>>>>> c058c0d... Friends and relations
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
<<<<<<< HEAD
     (else
      ((atom-to-function (operator nexp)) 
       (value (1st-sub-sexp nexp))
       (value (2nd-sub-sexp nexp)))))))
=======
     ((equal? (operator nexp) '(+))
      (+ (value (1st-sub-sexp nexp)) (value (2nd-sub-sexp nexp))))
     ((equal? (operator nexp) '(-))
      (- (value (1st-sub-sexp nexp) (value (2nd-sub-sexp nexp)))))
     (else
      (pow (value (1st-sub-sexp nexp)) (value (2nd-sub-sexp nexp)))))))
>>>>>>> c058c0d... Friends and relations

(define set?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((member? (car l) (cdr l)) #f)
     (else
      (set? (cdr l))))))

(define makeset
  (lambda (l)
    (cond
     ((null? l) '())
     ((member? (car l) (cdr l)) (makeset (cdr l)))
     (else
      (cons (car l) (makeset (cdr l)))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

<<<<<<< HEAD
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))

(define intersectall
  (lambda (l)
    (cond
     ((null? (cdr l)) (car l))
     (else
      (intersect (car l) (intersectall (cdr l)))))))

(define first
  (lambda (pair)
    (car pair)))

(define second
  (lambda (pair)
    (car (cdr pair)))) 

(define build
  (lambda (a b)
    (cons a (cons b (quote ())))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cons (first (car l)) (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define remberf
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) (quote()))
       ((test? (car l) a) (cdr l))
       (else 
	(cons (car l) ((remberf test?) a (cdr l))))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))


(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col (quote()) (quote ())))
     ((eq? (car lat) a)
      (multirember&co a
		      (cdr lat)
		      (lambda (newlat seen)
			(col newlat (cons (car lat) seen)))))
     (else
      (multirember&co a (cdr lat)
		      (lambda (newlat seen)
			(col (cons (cdr lat) newlat) seen))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define even?
  (lambda (n)
    (= (* (% n 2) 2) n)))

