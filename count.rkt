#lang racket

;; To use:
;; generate the `generated-author-info.csv` file,
;; but with the modifications I've made.
;; Then run this code, probably changing the `confs` list.


(require fancy-app)

(define (count-conf c)
  (for*/fold ([h (make-hash)])
             ([l (in-lines (open-input-file "generated-author-info.csv"))]
              [s (in-value (regexp-split "," l))]
              #:when (member (list-ref s 2) c))
    (hash-update! h
                  (list-ref s 0)
                  (lambda (h) (hash-update h (list-ref s 2) (+ _ (string->number (list-ref s 3))) 0))
                  (hash))
    h))

(define confs '(#;"icfp" "oopsla" #;"popl" "pldi"))
(define table (count-conf confs))


(printf "confs: ~s\n" confs)

(for ([i (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)])
  (define result
    (for/set 
        ([(k v) (in-hash table)]
         #:when (= (length (hash-keys v)) (length confs))
         #:when (andmap (>= _ i) (hash-values v))
         #:unless (andmap (> _ i) (hash-values v)))
      k))
  (printf "There are ~a people with ~a papers in all confs.\n" (set-count result) i)
  (when (> i 7)
    (pretty-print (set->list result))))
