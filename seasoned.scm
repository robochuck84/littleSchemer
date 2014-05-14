(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define add1
     (lambda (n)
       (+ n 1)))

(define sub1
    (lambda (n)
        (- n 1)))

(define multirember
  (lambda (a lat)
    (letrec
	 ((mr (lambda (lat)
	    (cond
	     ((null? lat) '())
	     ((eq? a (car lat)) (mr (cdr lat)))
	     (else (cons (car lat) (mr (cdr lat))))))))
     (mr lat))))

(define multirember-f
  (lambda (test?)
    (letrec
	((m-f
	  (lambda (a lat)
	    (cond
	     ((null? lat) '())
	     ((test? (car lat) a) (m-f a (cdr lat)))
	     (else (cons (car lat) (m-f a (cdr lat))))))))
      m-f)))

(define member?
  (lambda (a lat)
    (letrec
	((yes?
	  (lambda (lat)
	    (cond
	     ((null? lat) #f)
	     ((eq? (car lat) a) #t)
	     (else (yes? (cdr lat)))))))
      (yes? lat))))

(define union
  (lambda (set1 set2)
    (letrec
	((U
	  (lambda (s)
	    (cond
	     ((null? s) set2)
	     ((member? (car s) set2)
	      (union (cdr s) set2))
	     (else (cons (car s) (U (cdr s)))))))
	 (M?
	  (lambda (a lat)
	    (letrec
		((N?
		  (lambda (lat)
		    (cond
		     ((null? lat) #f)
		     ((eq? (car lat) a) #t)
		     (else (N? (cdr lat)))))))
	      (N? lat)))))
      (U set1))))

