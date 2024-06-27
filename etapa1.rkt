#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.

(define (longest-common-prefix-helper w1 w2 acc)
  (if (or (st-empty? w1) (st-empty? w2))
      (list (reverse acc) w1 w2)
      (if (equal? (car w1) (car w2))
          (longest-common-prefix-helper (cdr w1) (cdr w2) (cons (car w1) acc))
          (list (reverse acc) w1 w2))))

(define (longest-common-prefix w1 w2)
   (longest-common-prefix-helper w1 w2 empty-st))

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.

; Ia cdr-ul cuvantului pentru fiecare curvant din lista
(define (get-rests words)
  (if (st-empty? words)
      empty-st
      (cons (cdr (car words)) (get-rests (cdr words)))))

; Extrage o lista cu primele caractere din fiecare cuvant
(define (get-first-chars words)
   (if (st-empty? words)
       empty-st
       (cons (car (car words)) (get-first-chars (cdr words)))))

; Verifica daca primele caractere din fiecare cuvant sunt la fel
(define (all-first-chars-equal? first-chars)
  (if (or (st-empty? first-chars) (st-empty? (cdr first-chars)))
      #t
      (and (equal? (car first-chars) (cadr first-chars))
           (all-first-chars-equal? (cdr first-chars)))))

; Verificare pentru fiecare element din lista daca este null
(define (any-elements-are-empty? words)
  (if (st-empty? words) 
      #f ; stim din enunt ca lista de cuvinte nu este vida
      (if (st-empty? (car words))
          #t ; cuvantul din lista este o lista vida
          (any-elements-are-empty? (cdr words)))))

(define (longest-common-prefix-of-list-helper words prefix)
   (if (or (any-elements-are-empty? words) (not (all-first-chars-equal? (get-first-chars words))))
          (reverse prefix)
          (longest-common-prefix-of-list-helper (get-rests words) (cons (car (get-first-chars words)) prefix))))

(define (longest-common-prefix-of-list words)
  (longest-common-prefix-of-list-helper words empty-st)) 


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.

(define (match-pattern-with-label st pattern)
  (if (get-ch-branch st (car pattern))
      (let* ((branch (get-ch-branch st (car pattern)))
             (lcp (longest-common-prefix (get-branch-label branch) pattern))
             (first (car lcp))
             (second (cadr lcp))
             (third (caddr lcp)))
        (if (and (st-empty? second) (st-empty? third))
            #t
            (if (st-empty? third)
                (if (st-empty? (get-branch-subtree branch))
                    #t
                    (list (get-branch-label branch) '() (get-branch-subtree branch)))
                (if (st-empty? second)
                    (list (get-branch-label branch) third (get-branch-subtree branch))
                    (list #f first)))))
      (list #f '())))


; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.

(define (st-has-pattern? st pattern)
  (let* ((match (match-pattern-with-label st pattern)))
  (if (equal? match #t)
      #t
      (if (list? match)
          (if (equal? (car match) #f)
              #f
              (st-has-pattern? (caddr match) (cadr match)))
          #f))))
                  