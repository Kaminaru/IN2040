;; 1. Innkapsling, lokal tilstand og omgivelsesmodellen
;; a)

(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))




(define count 42)
(define c1 (make-counter))
(define c2 (make-counter)) 
(c1)
(c1)
(c1)
count 
(c2)



;; 2. Innkapsling, lokal tilstand, og message passing
;; a)
(display "2a\n")
(define (make-stack list)
  (define (pop!)
    (if (null? list)
        (set! list list)
        (set! list (cdr list))))
  
  (define (push! elems)
    (set! list (append (reverse elems) list )))
  
  (define (action . message)
    (cond ((eq? (car message) 'pop!) (pop!))
          ((eq? (car message) 'push!) (push! (cdr message)))
          ((eq? (car message) 'stack) list)))
  action)


(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack  '()))
(s1 'pop!)
(s1 'stack)
(s2 'pop!)
(s2 'push! 1 2 3 4)
(s2 'stack)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack)

;; b)
(display "2b\n")
(define (pop! s)
  (s 'pop!))

(define (stack s)
  (s 'stack))

(define (push! s . args)
  (define (push-help items)
    (if (not (null? items))
        (begin 
          (s 'push! (car items))
          (push-help (cdr items)))))
  (push-help args))

(pop! s1)
(stack s1)
(push! s1 'foo 'faa )
(stack s1)


;; 3. Strukturdeling og sirkulære lister
;; a)

(define bar (list 'a 'b 'c 'd 'e))
(cdddr bar)


(set-cdr! (cdddr bar) (cdr bar))
bar
(cdr bar)

(list-ref bar 5)
;; (list-ref) henter elementet som ligger på indexen til listen opgitt
;; (list-ref bar 5) henter element på index 5 i listen bar.
;; Med sykel i listen, har man uendelig med indexer, og hvis man itererer gjennom møter man på samme objekt.


;; 3b
(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
bah
(set-car! (car bah) 42)
bah

;; Når (caar bah) blir endret til 42 (med (set-car! (car bah)), blir (cdar bah) også endret,
;; fordi de peker til samme objekt.
;; (car bah) peker til den samme listen som (cdr bah)


;; 3c
(eq? (list-ref bar 2) (list-ref bar 5))
(cdr bar)

(define (cycle? list)
  (define (help single double)
    (cond
      ((null? double) #f)
      ((null? (cdr double)) #f)
      ((eq? single double) #t)
      (else (help (cdr single) (cddr double)))))
  (help list (cdr list)))


(cycle? '(hey ho))
(cycle? '(la la la))
(cycle? bah)
(cycle? bar)

(define (cycle? list) ;; O(-1) sparer utviklingstid
  (not (list? list)))

;; d
;; (list? in) skjekker om 'in' det er en liste,
;; Ettersom sirkulære lister ikke egentlig er lister, og fordi bar er sirkulær evaluerer (list? bar) -> #f
;; 