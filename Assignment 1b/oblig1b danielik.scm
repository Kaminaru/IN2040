;; 1. Par og lister
;; a)-e) is in the a-e.pdf file
;; f) (car(cdr al))       or (cadr al)
;; g) (car(cdr(car al)))  or (cadar al)
;; h) (car(car(cdr al)))  or (caadr al)
(define (ok items)
  (caadr items))

(ok '('(0) '(42 #t) '(3)))
;; i) Bare list:    (list (list 0 42) (list #t bar))
;;    Bare cons:    (cons (cons 0 (cons 42 '())) ((cons #t (cons bar '())) '()))

;; 2. Rekursjon over lister og høyereordens prosedyrer
;; a)

(define (take n items)
  (cond ((null? items) '())
        ((zero? n) '())
        (else (cons (car items)
                    (take (- n 1) (cdr items))))))

(take 3 '(a b c d e f))     ;; -> (a b c)
(take 1 '(a b c d e f))     ;; -> (a)
(take 4 '(a b))             ;; -> (a b)
(take 4 '())                ;; -> ()



;; b) Prosedyrens returverdi er den samme som returverdien for det
;;    sisterekursive kallet; den er halerekursiv
(define (take n items)
  (define (take-iter n in out)
    (cond ((null? in) (reverse out))
          ((zero? n) (reverse out))
          (else (take-iter (- n 1) (cdr in)
                           (cons (car in) out)))))
  (take-iter n items '()))

;; c)
(define (take-while pred items)
  (cond ((pred (car items))
         (cons (car items)
               (take-while pred (cdr items))))
        ((null? items) '())
        (else '())))


(take-while even? '(2 34 42 75 88 103 250))
(take-while odd? '(2 34 42 75 88 103 250))
(take-while (lambda (x) (< x 100)) '(2 34 42 75 88 103 250))

;; d)
(define (map2 proc items1 items2)
  (cond ((null? items1) '())
        ((null? items2) '())
        (else (cons (proc (car items1) (car items2))
                    (map2 proc (cdr items1) (cdr items2))))))

(map2 + '(1 2 3 4) '(3 4 5))
 
;; e)
(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5))


