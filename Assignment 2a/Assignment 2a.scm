(load "huffman.scm")

;; 1a
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y)
          x)))

(define (p-cdr proc)
  (proc (lambda (x y)
          y)))


;(p-car (p-cons "foo" "bar"))
;(p-cdr (p-cons "foo" "bar"))
;(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar"))))

;; 1b
(define foo 42)

;; Evaluerer til different
((lambda (foo x)
   (if (= foo x)
       'same
       'different))
 5 foo)

;; Evaluerer til (towel (42 towel))
((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz) baz))
 foo 'towel)


;; 1c
(display "1c\n")
(define (infix-eval items)
  ((cadr items) (car items) (caddr items)))

; Kall eksempler
;(define foo (list 21 + 21))
;(define baz (list 21 list 21))
;(define bar (list 84 / 2))
;(infix-eval foo)
;(infix-eval baz)
;(infix-eval bar)

;; 1d
(define bah '(84 / 2))

; Kall på (infix-eval bah) git feilmelding pga. qoute.
; Quote gjør slik at / ikke blir tolket som et prosedyrkall
; Men som en "character" / symbol





;; 2a
(define (decode-i bits tree)
  (define (decode-iter bits current-branch out)
    (if (null? bits)
        (reverse out)
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-iter (cdr bits) tree (cons (symbol-leaf next-branch) out))
              (decode-iter (cdr bits) next-branch out)))))
  (decode-iter bits tree '()))


;; 2b
;; Resultat: (samurais fight ninjas by night)
(decode sample-code sample-tree)


;; 2c
(define (encode items tree)
  (define (encode-1 items current-branch out)
    (if (null? items)
        (reverse out)
        (if (leaf? current-branch)
            (if (eq? (car items) (symbol-leaf current-branch))
                (encode-1 (cdr items) tree out) ;; we found word
                '())
            (append (encode-1 items (right-branch current-branch) (cons 1 out)) ;; go to right side
                    (encode-1 items (left-branch current-branch) (cons 0 out))))))
  (encode-1 items tree '())) ;; start from left side


;; 2c Test
(encode '(ninjas fight ninjas) sample-tree)
(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)


;; 2d 
(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))

(define (grow-huffman-tree items)
  (define (grow-tree leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (grow-tree (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set)))))
  (grow-tree (make-leaf-set items)))
  

;;(grow-huffman-tree freqs)
(display "2d:\n")
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook)




;; 2e
;; Det blir brukt 43bits for å kode meldingen.
;; Gjennomsnittlig kodelengde for meldingen er 2.529bits
;; Minste antall bits for fixed-length code er 4, fordi antall symboler er 16
;; log2(16) = 4

(define alphabet '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3) (in 2)
                                 (ambush 2) (defeat 1) (the 5) (sword 4) (by 12)
                                 (assasin 1) (river 2) (forest 1) (wait 1) (poison 1)))

(define alphabet-tree (grow-huffman-tree alphabet))
;(display alpha)

(define message '(ninjas fight
                         ninjas fight ninjas
                         ninjas fight samurais
                         samurais fight
                         samurais fight ninjas
                         ninjas fight by night))

(display "\n2e\nMelding som bit:\n")
(encode message alphabet-tree)

(define (teller liste verdi) ;; Teller antall elementer i liste
  (if (null? liste)
      verdi
      (teller (cdr liste) (+ verdi 1))))

(teller (encode message alphabet-tree) 0) ;; 43 bits totalt i melding.

(/ (teller (encode message alphabet-tree) 0) (teller message 0));; 2.529 gjennomsnittlig bits per symbol


;; 4 bits
;; fordi 4 bits, gir 16 mulige meldinger
;; log2(16) = 4


;; 2f  
(define (huffman-leaves tree)
  (if (leaf? tree)
      (list (cdr tree))
      (append (huffman-leaves (left-branch tree))
              (huffman-leaves (right-branch tree)))))
  
(huffman-leaves sample-tree)

