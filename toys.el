(quote atom)

(cons 's '(b c d))

(setq a 1)

(defun atom? (x)
  (not (listp x)))

(atom? a)

(defun lat? (l)
  (cond
   ((null l) t)
   ((atom? (car l)) (lat? (cdr l)))
   (t nil)))

(lat? '(a b c))
(car '(a))
(lat? 'a)

(defun member? (a lat)
  (cond
   ((null lat) f)
   (else (or (= (car lat) a) (member? (a (cdr lat)))))))


(member? 'a '(a b c))

(defun remember (a lat)
  (cond
   ((null lat)  '())
   ((= (car lat) a) (cdr lat))
   (default (cons (car lat) (remember a (cdr lat))))))


(remember a '(b a c))


