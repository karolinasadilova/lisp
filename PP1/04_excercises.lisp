#| polyglot-plist: (:YEAR 2025 :COURSE "PP1" :LECTURE 4 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 04_excercises.lisp -- řešení úloh k cvičení 4
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


(defun power2-speed (a n)
  (cond  ((= n 0) 1)
         ((evenp n) (power2 (power2-speed a (/ n 2))))
         (t (* a (power2-speed  a (- n 1))))))
(defun my-count-change (amount)
  (my-cc amount 6))
(defun my-cc (amount kinds)
  (cond ((= amount 0) 1)
        ((or (= kinds 0) (< amount 0)) 0)
        (t (+ (my-cc (- amount (my-kinds kinds) kinds)
              (my-cc amount (- kinds 1)))))))
(defun my-kinds (kinds)
  (cond ((= kinds 1) 1)
        ((= kinds 2) 2)
        ((= kinds 3) 5)
        ((= kinds 4) 10)
        ((= kinds 5) 20)
        ((= kinds 6) 50)))
(defun fast-power (a n)
  (fast-power-iter a n 1))
(defun fast-power-iter (a n ir)
  (cond ((= n 0) 1)
        ((evenp n) (fast-power-iter (power2 a) (/ n 2) ir))
        (t (fast-power-iter a (- n 1) (* a ir)))))
(defun dividesp (a b)
  (or (= 0 (rem a b))
      (= 0 (rem b a))))
(defun primep (n)
  (primep-help n 2))
(defun primep-help (n div)
  (cond ((= div n) t)
        ((= 0 (rem n div)) nil)
        (t (primep-help n (+ 1 div)))))
(defun perfectp (n)
  (perfectp-help n 2 1))
(defun perfectp-help (n div ir)
  (cond ((= ir n) t)
        ((> ir n) nil)
        ((= 0 (rem n div)) (perfectp-help n (+ div 1) (+ div ir)))
        (t (perfectp-help n (+ div 1) ir))))
(defun pascal (row col)
  (if (or (= row col) (= 0 col) (= 0 row)) 1
        (+ (pascal (- row 1) col)
           (pascal (- row 1) (- col 1)))))


(defun my-pascal (row col)
  (if (or (= 0 col) (= 0 row) (= row col)) 1
    (+ (my-pascal (- row 1) col)
       (my-pascal (- row 1) (- col 1)))))
(defun sum-of-squares-p (n)
  (sum-of-squares-p-help n 0 0))
(defun sum-of-squares-p-help (n i acc)
  (cond ((= n 0) t)
        ((= acc n) t)
        ((> i (sqrt n)) nil)
        ((> acc n) nil)
        (t (or (sum-of-squares-p-help n (+ i 1) (+ acc (power2 i)))
               (sum-of-squares-p-help n (+ i 1) acc)))))
(defun fast-fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (fast-fib-iter n 2 1 1))))
(defun fast-fib-iter (n index currprvek prevprvek)
  (cond ((= n index) currprvek)
        (t (fast-fib-iter n (+ 1 index) (+ currprvek prevprvek) currprvek))))
;repte:
(defun digit-sum-rep (a)
  (digit-sum-rep-help a (- (digit-count a) 1)))
(defun digit-sum-rep-help (a index)
  (if (< index 0) 0
    (+ (digit a index) (digit-sum-rep-help a (- index 1)))))

(defun liebniz-rep (epsilon)
  (liebniz-rep-help 3 1 epsilon -1))
(defun liebniz-rep-help (div ir epsilon sign)
  (if (< (/ 1 div) epsilon) (* 4 ir)
    (liebniz-rep-help a (+ div 2) (+ ir (* sign (/ 1 div))) epsilon (- sign))))
(defun fast-fib-rep (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (fast-fib-iter-rep n 2 1 1))))
(defun fast-fib-iter-rep (n crrindex crrprvek prevprvek)
  (if (= crrindex n) crrprvek
    (fast-fib-iter-rep n (+ 1 crrindex) (+ crrprvek prevprvek) crrprvek)))
(defun power-speed (a n)
  (power-speed-help a n 1))
(defun power-speed-help (a n ir)
  (cond ((= n 0) 1)
        ((evenp n) (power-speed-help (power2 a) (/ n 2) ir))
        (t (* a (power-speed-help a (- n 1) (* a ir))))))
(defun sum-of-sq-rep (n)
  (sum-of-sq-rep-hlp n 0 0))
(defun sum-of-sq-rep-hlp (n i acc)
  (cond ((= n 0) t)
        ((= acc n) t)
        ((< n acc) nil)
        ((> i (sqrt n)) nil)
        (t (or (sum-of-sq-rep-hlp n (+ 1 i) (+ acc (power2 i)))
               (sum-of-sq-rep-hlp n (+ 1 i) acc)))))
    