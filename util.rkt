#lang racket

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(provide (combine-out member?
                      dfs
                      dfs-list
                      in-range?
                      filter-compare
                      list-intersection
                      list-difference
                      remove-index
                      replace-index
                      manhattan-distance
                      symbol-upcase
                      char->symbol
                      random-elem
                      random-elem-weighted))

; depth first search
(define (dfs start-state max-depth update next-states)
  (let-values ([(visited goals) (rdfs start-state 0 max-depth update next-states empty empty)])
    goals))
(define (dfs-list start-states max-depth update next-states)
  (let-values ([(visited goals) (tdfs start-states 0 max-depth update next-states empty empty)])
    goals))
; recursive subfunction of depth first search
(define (rdfs state depth max-depth update next-states visited goals)
  (if (member? state visited)
      (values visited goals)
      (let-values ([(visited goals) (update state depth max-depth visited goals)])
        (if (= depth max-depth)
            (values visited goals)
            (let ([states (next-states state depth max-depth visited goals)])
              (tdfs states (+ depth 1) max-depth update next-states visited goals))))))
; state-threading subfunction of depth first search
(define (tdfs states depth max-depth update next-states visited goals)
  (if (empty? states)
      (values visited goals)
      (let-values ([(visited goals) (rdfs (first states) depth max-depth update next-states visited goals)])
        (tdfs (rest states) depth max-depth update next-states visited goals))))

(define (member? elm lst)
  (if (member elm lst) true false))

(define (in-range? num low high)
  (and (>= num low) (<= num high)))

; filter-compare : (x y -> bool) (listof x) (listof y) -> (listof x)
; applys pred to elements with equal indices, accumulating on lst1 when pred is true
(define (filter-compare pred lst1 lst2)
  (foldl (lambda (one two accum) 
           (if (pred one two)
               (cons one accum)
               accum))
         empty lst1 lst2))

(define (list-intersection lst1 lst2)
  (filter (lambda (c) (member c lst2)) lst1))

(define-test-suite list-intersection-tests
  (check-equal? (list-intersection '(1 2 3) '(2 3 4)) '(2 3))
  (check-equal? (list-intersection '(1 2 3) '(4 5 6)) empty))

; lst1 \ lst2  
(define (list-difference lst1 lst2)
  (filter (lambda (c) (not (member c lst2))) lst1))
    
(define (remove-index lst pos)
  (if (not (in-range? pos 0 (- (length lst) 1)))
      lst
      (append (take lst pos) (rest (drop lst pos)))))

(define-test-suite remove-index-tests
  (check-equal? (remove-index '(1 2 3) 1) '(1 3))
  (check-equal? (remove-index '(1 2 3) 0) '(2 3))
  (check-equal? (remove-index '(1 2 3) 2) '(1 2))
  (check-equal? (remove-index '(1 2 3) -1) '(1 2 3))
  (check-equal? (remove-index '(1 2 3) 3) '(1 2 3))
  (check-equal? (remove-index empty 0) empty))
    
(define (replace-index lst pos elem)
  (if (not (in-range? pos 0 (- (length lst) 1)))
      lst
      (append (take lst pos) (cons elem (rest (drop lst pos))))))

; manhattan-distance : num num num num -> num
(define (manhattan-distance row1 col1 row2 col2)
  (+ (abs (- row1 row2)) (abs (- col1 col2))))

; symbol-upcase : sym -> sym
(define (symbol-upcase sym)
  (string->symbol (string-upcase (symbol->string sym))))

(define (char->symbol c)
  (string->symbol (string c)))

(define (random-elem lst)
  (list-ref lst (random (length lst))))

(define (random-elem-weighted lst weight-func)
  (let* ([tot (apply + (map weight-func lst))]
         [rand (random tot)])
    (let loop ([lst lst]
               [acc 0])
      (if (> (+ acc (weight-func (first lst))) rand)
          (first lst)
          (loop (rest lst) (+ acc (weight-func (first lst))))))))

(define-test-suite all-tests
  list-intersection-tests
  remove-index-tests)

(define num-tests-failed (run-tests all-tests))
