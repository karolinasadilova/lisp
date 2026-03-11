#| polyglot-plist: (:YEAR 2025 :COURSE "PP1" :LECTURE 5 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 05_excercises.lisp -- řešení úloh k cvičení 5
;;;

#|
Do tohoto souboru pište řešení úloh z cvičení. Soubor se bude při každém 
spuštění aplikace načítat, takže všechny definice budou k dispozici.

Pokud je v některé definici chyba, soubor se nenačte a aplikace chybu
oznámí. Pak je možné ji opravit a soubor načíst ručně z menu (Evaluate
All Expressions) nebo klávesovou zkratkou.

Nově vytvořené definice vždy hned vyhodnoťte (menu Evaluate Expression
nebo Evaluate All Expressions) a otestujte v Listeneru.

Chybné a rozpracované definice můžete uzavírat do komentářů tak, jak to
je uděláno s tímto textem (víceřádkový komentář) nebo s nadpisem souboru
nahoře (jednořádkový komentář).
|#

(defun my-point-distance (a b)
  (sqrt(+ (power2 (- (point-x a) (point-x b)))
          (power2 (- (point-y a) (point-y b))))))
(defun middle (a b)
  (point (/ (+ (point-x a) (point-x b)) 2)
         (/ (+ (point-y a) (point-y b))2)))
(defun frac-eq-my (frac1 frac2)
  (= (* (num frac1) (denom frac2))
     (* (num frac2) (denom frac1))))
(defun my-fraction (num denom)
  (let ((div (gcd num denom)))
    (cons (/ num div) (/ denom div))))
(defun my-nth (n list)
  (if (= n 0) (car list)
    (my-nth (- n 1) (cdr list))))
(defun my-nthcdr (n list)
  (if (= n 0) list
    (my-nthcdr (- n 1) (cdr list))))
(defun my-nth-2 (n list)
  (car (nthcdr n list)))
(defun right-triangle-p (a b c)
  (let ((ab (point-distance a b))
        (bc (point-distance b c))
        (ca (point-distance c a)))
    (or (= power2 ab (+ (power2 bc) (power2 ca)))
        (= power2 bc (+ (power2 ab) (power2 ca)))
        (= power2 ca (+ (power2 ab) (power2 bc))))))
(defun op-vertex (a b)
  (point  (- (* 2 (point-x b)) (point-x a))
          (- (* 2 (point-y b)) (point-y a))))
(defun op-vertex-2 (A B)
  (point (+ (point-x B) (- (point-x B) (point-x A)))
         (+ (point-y B) (- (point-y B) (point-y A)))))
(defun frac--(frac1 frac2)
  (fraction (- (* (num frac1) (denom frac2)) (* (num frac2) (denom frac1)))
            (* (denom frac1) (denom frac2))))
(defun frac-/ (frac1 frac2)
  (fraction (* (num frac1) (denom frac2)
)
            (*  (num frac2) (denom frac1))))
(defun interval (upper-bound lower-bound)
  (cons upper-bound lower-bound))
(defun lower-bound (interval)
  (car interval))
(defun upper-bound (interval)
  (cdr interval))
(defun member-in-interval-p (number interval)
  (and (>= number (lower-bound interval))
       (<= number (upper-bound interval))))
(defun interval-insertion (interval1 interval2)
  (interval (min (lower-bound interval1) (lower-bound interval2))
            (max (upper-bound interval1) (upper-bound interval2))))
(defun position (el list)
  (posi-help el list 0))
(defun posi-help (el list index)
  (cond ((null list) nil)
        ((eql el (car list)) index)
        (t (posi-help el (cdr list) (+ 1 index)))))
(defun equal-lists-p (l1 l2)
  (cond ((and (null l1) (null l2)) t)
        ((or (null l1) (null l2)) nil)
        ((eql (car l1) (car l2)) (equal-lists-p (cdr l1) (cdr l2)))
        (t nil)))
(defun mismatch (l1 l2)
  (mismatch-help l1 l2 0))
(defun mismatch-help (l1 l2 index)
  (cond ((and (null l1) (null l2)) nil)
        ((or (null l1) (null l2)) index)
        ((eql (car l1) (car l2)) (mismatch-help (cdr l1) (cdr l2) (+ 1 index)))
        (t index)))
(defun last (list n)
  (last-help list (- (length list) n)))
(defun last-help (list index)
  (if (= index 0) list
    (last-help (cdr list) (- index 1))))
(defun mismatch-rep (l1 l2)
  (mismatch-rep-help l1 l2 0))
(defun mismatch-rep-help (l1 l2 index)
  (cond ((and (null l1) (null l2)) nil)
        ((or (null l1) (null l2)) index)
        ((eql (car l1) (car l2)) (mismatch-rep-help (cdr l1) (cdr l2)))
        (t index)))
(defun my-last (list n)
  (my-last-iter list (- (length list) n)))
(defun my-last-iter (list i)
  (if (= i 0) list
    (my-last-iter (cdr list) (- i 1))))


         