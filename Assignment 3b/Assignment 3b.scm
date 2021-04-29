(load "evaluator.scm")

(set! the-global-environment (setup-environment))
(mc-eval '(+ 1 2) the-global-environment)

;; 1a)
;;(define (foo cond else)
;;  (cond ((= cond 2) 0)
;;        (else (else cond))))
;;
;;(define cond 3) 
;;(define (else x) (/ x 2))
;;(define (square x) (* x x))
;;
;;(foo 2 square) returnerer 0 fordi (= cond 2) 0) skal returnere 0 naar 2 er parameter
;;(foo 4 square) returnerer 16 fordi (else (else cond)))) kaller på (square x) som tar kvadratet av tallet, aka 4*4 = 16
;;(cond ((= cond 2) 0)
;;      (else (else 4))) returnerer 2, grunnet kallet paa else og 4 som deler 4 paa 2.

;; Evaluatoren evaluerer cond og else som parametre og ikke som en vanlig condition.
;; Naar det blir kalt paa disse senere i funksjonen evaluerers de til et kall paa square eller else

;; 2a)
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
        ;;      her kan vi legge til flere primitiver.
        (list '1+
              (lambda (x) (+ x 1))) ;; Endringer gjort fra prekode
        (list '1-
              (lambda (x) (- x 1))) ;; Endringer gjort fra prekode
        ))
(set! the-global-environment (setup-environment))


;; 2b)
(define (install-primitive! name proc)
  (set! the-global-environment
        (extend-environment (list name) (list (list 'primitive proc))
                            the-global-environment)))

(install-primitive! 'square (lambda (x) (* x x))) ;; Legger inn square som primitive operasjon evaluatoren.



(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; Lagt til 3a
        ((or? exp) #t)  ;; Lagt til 3a
        ((let? exp) #t)  ;; Lagt til 3c
        (else #f)))

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env)) ;; Lagt til 3a
        ((or? exp) (eval-or exp env))   ;; Lagt til 3a
        ((let? exp) (eval-let exp env)) ;; Lagt til 3c
         
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))))

;; 3a
(define (or? exp) (tagged-list? exp 'or))
(define (and? exp) (tagged-list? exp 'and))

(define (eval-or exp env)
  (define (or-iter liste)
    (if (null? liste)
        #f
        (if (car liste)
            (car liste)
            (or-iter (cdr liste)))))

  (or-iter (cdr exp)))

(define (eval-and exp env)
  (define (and-iter liste)
    (if (null? (cdr liste))
        (car liste)
        (if (not (car liste))
            #f
            (and-iter (cdr liste)))))
  (and-iter (cdr exp)))


;;3b
(define (eval-if exp env)
  (if (eq? (car exp) 'if)
      (if (true? (cadr exp))
          (cadr exp)
          (eval-elsif (cddddr exp) env))
      'bigerror))


(define (eval-elsif exp env)
  (cond ((eq? (car exp) 'else) (cadr exp))
        ((eq? (car exp) 'elsif) (if (true? (cadr exp))
                                    (cadddr exp)
                                    (eval-elsif (cddddr exp) env)))))



  
;; 3c
(define (let? exp) (tagged-list? exp 'let))

(define (eval-let exp env)
  
  (define (get-vars vars exp)
    (if (null? exp)
        (reverse vars)
        (get-vars (cons (caar exp) vars) (cdr exp))))
        
  (define (get-exps exps exp)
    (if (null? exp)
        (reverse exps)
        (get-exps (cons (cadar exp) exps) (cdr exp))))

  (define (let-til-lambda vars exps body)
    (cons (cons 'lambda (cons vars (cons body '()))) exps))
    
  
  (mc-eval (let-til-lambda (get-vars '() (cadr exp)) (get-exps '() (cadr exp)) (caddr exp)) env))
 

(mc-eval
 '(let ((var1 1)
        (var2 5)
        (var3 10))
    (+ var1 var2 var3))
 the-global-environment)


;; 3d

;(define (eval-let exp env)
;  
;(define (get-vars2 vars exp)
;  (if (eq? 'in (cadddr exp))
;      (reverse (cons (car exp) vars ))
;      (get-vars2 (cons (car exp) vars) (cddddr exp))))
;
;(define (get-exps2 exps exp)
;  (if (eq? 'in (cadddr exp))
;      (reverse (cons (caddr exp) exps))
;      (get-exps2 (cons (caddr exp) exps) (cddddr exp))))
;
;(define (get-body2 exp)
;  (if (eq? 'in (cadddr exp))
;      (cddddr exp)
;      (get-body2 (cddddr exp))))
;
;  
;(mc-eval (cons (cons 'lambda
;                (cons (get-vars2 '() (cdr exp)) ; get-vars2 henter variabelnavnene
;                 (append (get-body2 (cdr exp)) '()))) ; get-body2 henter bodyen
;                  (get-exps2 '() (cdr exp))) ; get-exps2 henter variabelverdiene
;         env))


;; Tester for nye let.
;(mc-eval
; '(let var1 = 1 and
;       var2 = 5 and
;       var3 = 10 in
;    (display (cons var1 var2))
;    (+ var1 var2 var3))
; the-global-environment)


;(mc-eval (cons (cons 'lambda (cons vars2 (append body2 '()))) exps2) the-global-environment)



(read-eval-print-loop)