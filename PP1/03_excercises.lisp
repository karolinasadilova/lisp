#| polyglot-plist: (:YEAR 2025 :COURSE "PP1" :LECTURE 3 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 03_excercises.lisp -- řešení úloh k cvičení 3
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

(defun percentage-1 (part whole)
  (* (/ part whole) 100.0))
(defun percentage-2 (part whole)
  (let ((whole (if (eql whole t) 
                   10909500
                 whole)))
        (* (/ part whole) 100.0)))
(defun percentage-3 (part whole)
  (percentage-1 part (if (eql whole t) 10909500 whole)))
(defun percentage-4 (part whole)
  (if (eql t whole) 
      (percentage-4 part 10909500) 
    (* (/ part whole) 100.0)))
(defun squarep2 (n)
  (= n (power2 (round (sqrt n)))))

(defun contains-square-p (a b)
  (if (> a b) nil
    (if (squarep a) t (contains-square-p (+ a 1) b))))


(defun contains-square-p-cond (a b)
  (cond ((> a b) nil)
        ((squarep a) t)
        (t (contains-square-p-cond (+ a 1) b))))

(defun contains-square-log (a b)
  (and (<= a b)
       (or (squarep a)
           (contains-square-log (+ a 1) b))))

(defun my-fact (n)
  (if (= n 0) 1
    (* n (fact (- n 1)))))
(defun approx-= (a b epsilon)
  (<= (abs (- a b)) epsilon))
(defun cos-fix-point-iter (x epsilon)
  (let ((y (cos x)))
    (if (approx-= x y epsilon) y
      (cos-fix-point-iter y epsilon))))
(defun cos-fixpoint (epsilon)
  (cos-fix-point-iter 0 epsilon))
(defun my-fact-iter (acc n)
  (if (= n 0) ACC
    (my-fact-iter (* n acc) (- n 1))))
(defun my-fact (n)
  (my-fact-iter 1 n))
(defun my-fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (my-fib (- n 2))
              (my-fib(- n 1))))))
(defun ellipse-area (a b)
  (if (eql b t) (* pi (power2 a))
    (* pi a b)))
(defun ellipse-area-rek (a b)
  (if (not(eql b t)) (* pi a b)
    (ellipse-area-rek a a)))
(defun ellipse-area-3 (a b)
  (let ((b (if (eql b t) a b)))
    (* pi a b)))
(defun my-squarep-help (n i)
  (cond ((= n (power2 i)) t)
        ((> (power2 i) n) nil)
        (t (my-squarep-help n (+ i 1)))))
(defun my-squarep (n)
  (my-squarep-help n 1))
(defun gcd (a b)
  (if (= b 0) a
    (gcd b (rem a b))))
(defun heron-sqrt-help (a x epsilon)
  (let ((y (/ (+ x (/ a x)) 2)))
    (if (approx-= a (power2 y) epsilon) y
      (heron-sqrt-help a y epsilon))))
(defun heron-sqrt (a epsilon)
  (heron-sqrt-help a 1 epsilon))
(defun sum-interval (a b)
  (cond ((> a b) 0)
        ((= a b) a)
        (t (+ a (sum-interval (+ a 1) b)))))
(defun power-iter-help (a n acc)
  (cond ((= n 0) acc)
        (t (power-iter-help a (- n 1) (* a acc)))))
(defun power-iter (a n)
  (power-iter-help a n 1))

(defun digit-sum-1 (n)
  (if (= n 0) 0
    (+ (rem n 10)
       (digit-sum-1 (div n 10)))))
(defun digit-sum-2-help (a n)
  (if (< n 0) 0
    (+ (digit a n)
       (digit-sum-2-help a (- n 1)))))
(defun digit-sum-2 (a)
  (digit-sum-2-help a (- (digit-count a) 1)))
(defun div9 (n)
  (= (rem (digit-sum-2 n) 9)0))
(defun liebniz (epsilon)
  (liebniz-help 1 3 -1 epsilon))
  
(defun liebniz-help (ir div sign epsilon)
  (if (< (/ 1 div) epsilon)
      (* 4 ir)
    (liebniz-help (+ ir (* sign (/ 1 div))) (+ div 2) (- sign) epsilon)))
;heronsqrt, digit sum


(defun heron-sqrt (a epsilon)
  (heron-sqrt-help-2 a 1 epsilon))
(defun heron-sqrt-help-2 (a x epsilon)
  (let ((y (/ (+ (/ x a) x) 2)))
    (if (approx-= (power2 y) a epsilon) y
      (heron-sqrt-help a y epsilon))))
    
(defun digit-sum-3 (a)
  (digit-sum-3-help a (- (digit-count a) 1)))
(defun digit-sum-3-help (a n)
  (if (< n 0) 0
  (+ (digit a n) (digit-sum-3-help a (- n 1)))))
(defun my-liebniz (epsilon)
  (my-liebniz-help 3 1 -1 epsilon))
(defun my-liebniz-help (div ir sign epsilon)
  (if (< (/ 1 div) epsilon)
      (* 4 ir)
    (my-liebnziz-help (+ 2 div) (+ ir (* sign (/ 1 div))) (- sign) epsilon)))