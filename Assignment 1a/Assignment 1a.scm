;; 1. Grunnleggende syntaks og semantikk i Scheme
;; a) Følgende utrykk evaluerer til 30
;; b) Det kommer feil melding på delen der vi har (5). Fordi den liste har ikke operatoren og den andre argument.
;; c) I den tilfelle får vi feil igjen, fordi delen med (4 + 2) er ikke prosedyre, fordi operatoren står ikke i
;;    riktig posisjon i prosedyre.
;; d) Følgende utrykk skal returnere 22 uten noen feil melding. Her vi definert "bar" og kalte den etter på.
;; e) Denne prosedyre skal returnerer feil. Fordi vi har ikke definert "bar" ennå. Det skjer hvis vi kjører den
;;    prosedyre uten å definere bar. Men hvis vi bruker den prosedyre etter definering som vi hadde i
;;    deloppgave «d», så får vi 11 som evaluering.
;; f) Her det blir samme som i deloppgave «e», fordi vi har ikke definert bar. Men hvis vi definerer bare som
;;    i deloppgave «d» så får vi følgende evaluering: 12. Først ganges «bar» med 3, 4 og 1, og etter på
;;    svar deles på «bar». 

;; 2. Kontrollstrukturer og egendefinerte prosedyrer
;; a) Verdien som evalueres fra første utrykken er: «paff!». Fordi den første tingen som er "true" er "paff!"
;;    Så kode går ikke videre og returnerer "paff!" som "true" verdi.

;;    Verdien som evalueres fra andre utrykken er: #f. Fordi ikke alle 4 utrykkene var "true"

;;    Det er "poff!" som skal evalueres fra den tredje utrykk. Selv om "i-am-undefined» er ikke definert.
;;    Det er fordi hvis vi vår true fra prosedyre så kommer ikke kode til å sjekke hva som står i "false"

;; b)
(define (sign x)
  (if (negative? x)
      -1
      (if (= x 0) 0 1 )))

(define (sign x)
  (cond ((< x 0) -1)
        ((> x 0) 1)
        (else 0)))
;; c)
(define (sign x)
  (or
   (and (<= x 0) (< x 0) -1)
   (and (>= x 0) (> x 0) 1)
   (and (<= x 0) (= x 0) 0)))

;; 3. Rekursjon, iterasjon, og blokkstruktur
;; a)
(define (add1 x)
  (+ x 1))
(define (sub1 x)
  (- x 1))
;; b)
(define (plus x y)
  (if (zero? y)
      x
      (plus (add1 x) (sub1 y))))
;; Her prosedyre er rekursiv men prosess er iterativ.
;; c) Prosedyren som jeg har definert i oppgave b gir opphav til en iterativ prosess.
;;    Det er fordi x og y endres på vei. Også det er ingen prosedyrer som venter for noen
;;    argumenter som skal regnes ut av rekursiv kall.
;;    For å lage rekursive process må vi ha prosedyrer som skal venter for minst en argument
;;    som skal beregnes med rekursiv kall. Her er eksempel på rekursiv prosess:
(define (plus x y)
  (if (zero? y)
       x
       (add1 (plus x (sub1 y)))))


;; d)
(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (+ 1 e))))
  (power-iter 1))
;; Vi kan forenkle definisjonen av hjelpe-prosedyren (fjerne formele parametrene b og n i "power-iter"
;; fordi vi allerede definert «b» og «n» i power-close-to prosedyre, derfor har hjelpe prosedyren
;; tilgang til de. Så vi trenger ikke definerer dem på nytt i "power-iter" fordi "b"
;; og "n" har konstante veriderier.

;; e) Nei, det er ikke mulig å forenkle den interne definisjonen av fib-iter.
;;    Grunnen for dette er at vi bruker ikke samme n over hele prosessen.