#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")
(require racket/trace)

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).

(define (substring? text pattern)
  (let* ((suffix-tree (text->st text ast-func)))
    (st-has-pattern? suffix-tree pattern)))


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).

 (define (longest-common-substring text1 text2)
  (let ((st1 (text->cst text1)))
    (define (find-longest-match suffix)
      (let loop ((st st1) (pattern suffix) (longest '()))
        (let ((match (match-pattern-with-label st pattern)))
          (cond
            ((equal? match #t) (append longest pattern))
            ((equal? (car match) #f) (append longest (cadr match)))
            (else
              (let* ((prefix-found (car match))
                     (remaining-pattern (cadr match))
                     (subtrees (caddr match))
                     (new-longest (append longest prefix-found)))
                (loop subtrees remaining-pattern new-longest)))))))

     (let process-suffixes ((suffixes (get-suffixes text2)) (longest empty-st))
      (if (null? suffixes)
          longest
          (let* ((suffix (car suffixes))
                (rest-suffixes (cdr suffixes))
                (match (find-longest-match suffix))
                (new-longest (if (> (length match) (length longest)) match longest)))
                (process-suffixes rest-suffixes new-longest))))))
 

; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.

(define (repeated-substring-of-given-length text len)
  (define (dfs node prefix)
    (if (and (not (st-empty? node)) (>= (length prefix) len))
        (take prefix len)
        (let loop ((branches (if (st-empty? node)
                                 empty-st
                                 (cons (first-branch node) (other-branches node)))))
          (if (st-empty? branches)
              #f
              (let* ((branch (first-branch branches))
                     (rest-branch (other-branches branches))
                     (label (get-branch-label branch))
                     (subtree (get-branch-subtree branch))
                     (new-prefix (append prefix label)))
                (or (dfs subtree new-prefix)
                    (loop rest-branch)))))))
 
  (let* ((st (text->cst text)))
    (dfs st empty-st)))


