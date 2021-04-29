(load "prekode3a.scm")


;; 1 a og b
(define (mem memo? proc)
  (cond ((equal? memo? 'memoize)
         (let ((table (make-table)))
           (lambda arg (if (null? arg) proc
                           (let ((previously-comp-res (lookup arg table)))
                             (or previously-comp-res
                                 (let ((res (apply proc arg)))
                                   (insert! arg res table)
                                   res)))))))
        ((equal? memo? 'unmemoize)(proc))))

(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 2)
(fib 4)
(set! fib (mem 'unmemoize fib))
(fib 3)

;; 1 c
;; Mellomberegningene som blir utfør inne i prosedyren blir ikke memoisert.
;; Det er bare det avsluttende resultatet som blir memoisert.
;; Kun argumentet og resultat fra det aller første kallet blir lagret i tabell.


;; 2a)
(define (list-to-stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list) (list-to-stream (cdr list)))))

;; 30 is limit in this procedure. If we remove '29' so it will run to infinity
;; with infinit stream
(define (stream-to-list stream . n)
  (cond ((stream-null? stream)'())
        ((null? n) (cons (stream-car stream) (stream-to-list (stream-cdr stream) 29)))
        ((= (car n) 0)'())
        (else(cons (stream-car stream) (stream-to-list (stream-cdr stream) (- (car n) 1))))))


(define foo (list-to-stream '(1 2 3 4 5)))
(stream-to-list (stream-interval 10 20))
(show-stream nats 15)
(stream-to-list nats 10)

;; 2b)
(define (stream-take n stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((= n 0) the-empty-stream)
        (else (cons-stream (stream-car stream) (stream-take (- n 1) (stream-cdr stream))))))

(define foo (stream-take 10 nats))
foo
(show-stream foo 5)
(show-stream foo 20)
(show-stream (stream-take 15 nats) 10)

;; 2c)
;; Et potensielt problem kan oppstå hvis strømmen er uendelig, da vil remove-duplicates kjøre i uendelig tid.
;; Dette problemet oppsår også i memq, i siste linje. Hvor memq kuddrer potensielt i uendelig tid.


;; 2d)
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (remove-duplicates
                    (stream-filter
                    (lambda (item) (not (equal? item (stream-car stream)))) (stream-cdr stream))))))




;; test for 2d
(define foo (list-to-stream '(1 2 3 4 5 2 2 3 1 6 7)))
foo
(show-stream (remove-duplicates foo))
  


