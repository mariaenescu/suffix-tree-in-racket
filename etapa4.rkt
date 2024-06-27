#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix-helper w1 w2 acc)
  (if (or (collection-empty? w1) (collection-empty? w2))
      (list (reverse acc) w1 w2)
      (if (equal? (collection-first w1) (collection-first w2))
          (longest-common-prefix-helper (collection-rest w1) (collection-rest w2) (cons (collection-first w1) acc))
          (list (reverse acc) w1 w2))))

(define (longest-common-prefix w1 w2)
  (longest-common-prefix-helper w1 w2 '()))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (get-rests words)
  (if (collection-empty? words)
      collection-empty
      (collection-cons (cdr (collection-first words)) (get-rests (collection-rest words)))))

(define (get-first-chars words)
   (if (collection-empty? words)
       collection-empty
       (collection-cons (car (collection-first words)) (get-first-chars (collection-rest words)))))

(define (all-first-chars-equal? first-chars)
  (if (or (collection-empty? first-chars) (collection-empty? (collection-rest first-chars)))
      #t
      (and (equal? (collection-first first-chars) (collection-first (collection-rest first-chars)))
           (all-first-chars-equal? (collection-rest first-chars)))))

(define (any-elements-are-empty? words)
  (if (collection-empty? words) 
      #f
      (if (null? (collection-first words))
          #t
          (any-elements-are-empty? (collection-rest words)))))

(define (longest-common-prefix-of-collection-helper words prefix)
   (if (or (any-elements-are-empty? words) (not (all-first-chars-equal? (get-first-chars words))))
          (reverse prefix)
          (longest-common-prefix-of-collection-helper (get-rests words) (cons (collection-first(get-first-chars words)) prefix))))

(define (longest-common-prefix-of-collection words)
  (longest-common-prefix-of-collection-helper words '()))



(define (match-pattern-with-label st pattern)
  (if (get-ch-branch st (car pattern))
      (let* ((branch (get-ch-branch st (car pattern)))
             (lcp (longest-common-prefix (get-branch-label branch)  pattern))
             (first (car lcp))
             (second (cadr lcp))
             (third (caddr lcp)))
        (if (and (null? second) (null? third))
            #t
            (if (null? third)
                (if (st-empty? (get-branch-subtree branch))
                    #t
                    (list (get-branch-label branch) empty-st (get-branch-subtree branch)))
                (if (null? second)
                    (list (get-branch-label branch) third (get-branch-subtree branch))
                    (list #f first)))))
      (list #f empty-st)))



(define (st-has-pattern? st pattern)
  (let* ((match (match-pattern-with-label st pattern)))
  (if (equal? match #t)
      #t
      (if (list? match)
          (if (equal? (car match) #f)
              #f
              (st-has-pattern? (caddr match) (cadr match)))
          #f))))          



(define (get-suffixes text)
  (if (collection-empty? text)
      collection-empty
      (collection-cons text (get-suffixes (collection-rest text)))))



(define (get-ch-words words ch)
  (collection-filter words (lambda (word) (equal? (collection-first word) ch))))


(define (ast-func suffixes)
  (let* ((ast-label (list (car (collection-first suffixes))))
         (new-suffixes (stream-map (lambda (suf) (cdr suf)) suffixes)))
        (cons ast-label new-suffixes)))


(define (cst-func suffixes)
  (let* ((prefix (longest-common-prefix-of-collection suffixes)))
   (cons prefix (stream-map (lambda (suffix) (drop suffix (length prefix))) suffixes))))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (label-suffixes labeling-func suffixes)
  (if (collection-empty? suffixes)
      collection-empty
      (let* ((result (labeling-func suffixes))
             (label (car result))
             (new-suffixes (cdr result)))
        (collection-cons (cons label new-suffixes) collection-empty))))

(define (group-suffixes-by-first-char labeling-func suffixes alphabet)
  (stream-filter
   (lambda (x) (not (collection-empty? x)))
   (stream-map
    (lambda (char)
      (label-suffixes labeling-func (collection-filter suffixes (lambda (suf) (and (not (collection-empty? suf)) (equal? (collection-first suf) char))))))
    alphabet)))

(define (suffixes->st labeling-func suffixes alphabet)
  (if (collection-empty? suffixes)
      collection-empty
      (let* ((grouped-suffixes (group-suffixes-by-first-char labeling-func suffixes alphabet)))
        (stream-map
          (lambda (group)
            (if (collection-empty? group)
                collection-empty
                (let* ((label (car (collection-first group)))
                       (new-suffixes (cdr (collection-first group))))
                  (cons label (suffixes->st labeling-func new-suffixes alphabet)))))
          grouped-suffixes))))


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (text labeling-func)
    (let* ((suffixes (get-suffixes (append text '(#\$))))
           (alphabet (sort (remove-duplicates (append text '(#\$))) char<?)))
    (suffixes->st labeling-func suffixes (list->stream alphabet)))))

(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

(define text->ast (lambda (text) (text->st text ast-func)))

(define text->cst (lambda (text) (text->st text cst-func)))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let* ((suffix-tree (text->st text ast-func)))
    (st-has-pattern? suffix-tree pattern)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (define (dfs node prefix)
    (if (and (not (st-empty? node)) (>= (length prefix) len))
        (take prefix len)
        (let loop ((branches (if (st-empty? node)
                                  empty-st
                                  (collection-cons (first-branch node) (other-branches node)))))
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
    (dfs st '())))
