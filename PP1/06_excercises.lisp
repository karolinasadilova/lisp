#| polyglot-plist: (:YEAR 2025 :COURSE "PP1" :LECTURE 6 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 06_excercises.lisp -- řešení úloh k cvičení 6
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
;even,odd,scale/list,reverse,revappend,append2,sumlists2
(defun my-even-conses (list)
  (if (null list) nil
    (cons (car list)
          (my-odd-conses (cdr list)))))
(defun my-odd-conses (list)
  (if (null list) nil
    (my-even-conses (cdr list))))
(defun my-revappend (l1 l2)
  (if (null l1) l2
    (my-revappend (cdr l1)
                  (cons (car l1) l2))))
(defun my-reverse (list)
  (my-revappend list nil))
(defun my-append2 (l1 L2)
  (if (null l1) l2
    (cons (car l1)
          (my-append2 (cdr l1) l2))))
(defun my-scale-list (list factor)
  (if (null list) nil
    (cons (* factor (car list))
          (my-scale-list (cdr list) factor))))
(defun my-sum-lists-2 (l1 l2)
  (if (null l1) nil
    (cons (+ (car l1) (car l2))
          (my-sum-lists-2 (cdr l1) (cdr l2)))))
(defun my-elementp (element list)
  (cond ((null list) nil)
        ((eql element (car list)) t)
        (t (my-elementp element (cdr list)))))
(defun my-adjoin (x list)
  (if (elementp x list) list
    (cons x list)))
(defun my-subsetp (l1 l2)
  (cond ((null l1) t)
        ((elementp (car l1) l2) (my-subsetp (cdr l1) l2))
        (t nil)))
(defun my-subset-better (l1 l2)
  (or (null l1)
      (and (elementp (car l1) l2)
           (my-subset-better (cdr l1) l2))))
(defun my-intersection (l1 l2)
  (cond ((null l1) nil)
        ((elementp (car l1) l2) (cons (car l1) (my-intersection (cdr l1) l2)))
        (t (my-intersection (cdr l1) l2))))
(defun my-power-set (list)
  (if (null list) (list nil)
    (let ((cdr-subsets (my-power-set (cdr list))))
      (append-2 cdr-subsets (my-power-set-help (car list) cdr-subsets)))))
    
(defun my-power-set-help (elem list)
  (if (null list) nil
    (cons (cons elem (car list))
          (my-power-set-help elem (cdr list)))))

(defun meine-last (list n)
  (meine-last-iter list (- (length list) n)))
(defun meine-last-iter (list i)
  (if (= i 0) list
    (meine-last-iter (cdr list) (- n 1))))
(defun meine-subsets (list)
  (if (null list) (list nil)
    (let ((cdr-subsets (meine-subsets (cdr list))))
     (append-2 cdr-subsets (cons-alles (car list) cdr-subsets))))) 
      
(defun cons-alles (element list)
  (if (null list) nil
    (cons (cons element (car list))
          (cons-alles element (cdr list)))))
(defun my-merge-sort (list)
  (if (or (null list) (null (cdr list))) list
          (my-merge (my-merge-sort (even-conses list))
                    (my-merge-sort (odd-conses list)))))
(defun my-merge (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (my-merge (cdr l1) l2)))
        (t (cons (car l2) (my-merge l1 (cdr l2))))))
(defun make-ar-seq-list (a1 d n)
  (if (= n 0) nil
    (cons a1 (make-ar-seq-list (+ a1 d) d (- n 1)))))
(defun make-ar-seq-list-tail (a1 d n)
  (make-ar-seq-list-tail-help a1 d n nil))
(defun make-ar-seq-list-tail-help (a1 d n acc)
  (if (= n 0) (reverse acc)
    (make-ar-seq-list-tail-help (+ a1 d) d (- n 1) (cons a1 acc))))
(defun make-geom-list (a1 q n)
  (if (= n 0) nil
    (cons a1 (make-geom-list (* q a1) q (- n 1)))))
(defun make-geom-list-tail (a1 q n)
  (make-geom-list-tail-help a1 q n nil))
(defun make-geom-list-tail-help (a1 q n acc)
  (if (= n 0) (reverse acc)
    (make-geom-list-tail-help (* a1 q) q (- n 1) (cons a1 acc))))
(defun copy-list (list)
  (if (null list) nil
    (cons (car list) (copy-list (cdr list)))))
(defun remove (element list)
  (cond ((null list) nil)
        ((eql element (car list)) (remove element (cdr list)))
        (t (cons (car list)
                 (remove element (cdr list))))))
(defun tailp (l1 l2)
  (cond ((eql l1 l2) t)
        ((null l2) nil)
        (t (tailp l1 (cdr l2)))))
(defun ldiff (l1 l2)
  (if (tailp l2 l1) (ldiff-help l1 l2) l1))
(defun ldiff-help (l1 l2)
  (if (eql (car l1) (car l2)) nil
    (cons (car l1) (ldiff-help (cdr l1) l2))))
(defun factorials (n)
  (factorials-help n 0 1))
(defun factorials-help (n ir acc)
  (cond ((= ir n) nil)
        ((= ir 0) (cons 1 (factorials-help n (+ 1 ir) acc)))
        (t (cons (* acc ir) (factorials-help n (+ ir 1) (* ir acc))))))
(defun my-tailp (l1 l2)
  (cond ((null l2) nil)
        ((eql l1 l2) t)
        (t (my-tailp l1 (cdr l2)))))
(defun my-ldiff (l1 l2)
  (if (tailp l2 l1) (my-ldiff-help l1 l2) l1))
(defun my-ldiff-help (l1 l2)
  (if (eql (car l2) (car l1)) nil
        (cons (car l1) (my-ldiff-help (cdr l1) l2))))
        
(defun my-factorials (n)
  (my-fact-help 0 n 1))
(defun my-fact-help (i n acc)
  (cond ((= i n) nil)
        ((= i 0) (cons 1 (my-fact-help (+ i 1) n acc)))
        (t (cons (* i acc) (my-fact-help (+ 1 i) n (* i acc))))))

(defun list-tails (list)
  (if (null list) (list nil)
    (cons (copy-list list) (list-tails (cdr list)))))
(defun fib-list (n)
  (fib-list-help n 0 1 0))
(defun fib-list-help (n ir curr prev)
  (cond ((= ir n) nil)
        ((= ir 0) (cons 0 (fib-list-help n (+ 1 ir) curr prev)))
        ((= ir 1) (cons 1 (fib-list-help n (+ 1 ir) curr prev)))
        (t (cons (+ curr prev) (fib-list-help n (+ 1 ir) (+ prev curr) curr)))))
(defun list-tail-withnil (list)
  (if (null list) (list nil)
    (cons (copy-list list) (list-tail-withnil (cdr list)))))
(defun list-tails-with-o-nil (list)
  (if (null list) nil
    (cons (copy-list list) (list-tails-with-o-nil (cdr list)))))
(defun my-sum-help (list n len)
  (if (>= n len)
      0
    (+ (nth n list) (my-sum-help list (+ n 1) len))))
(defun my-sum (list)
  (my-sum-help list 0 (length list)))
(defun subtract-lists-2 (l1 l2)
  (if (null l1) nil
    (cons (- (car l1) (car l2))
          (subtract-lists-2 (cdr l1) (cdr l2)))))
(defun scalar-product (l1 l2)
  (if (null l1) 0
    (+ (* (car l1) (car l2))
       (scalar-product (cdr l1) (cdr l2)))))
(defun vector-length (list)
  (sqrt (sum-of-squares list)))
(defun sum-of-squares (list)
  (if (null list) 0
    (+ (power2 (car list)) (sum-of-squares (cdr list)))))
(defun remove-duplicates (list)
  (cond ((null list) nil)
        ((elementp (car list) (cdr list)) (remove-duplicates (cdr list)))
        (t (cons (car list) (remove-duplicates (cdr list))))))
(defun union (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        ((elementp (car l1) l2) (union (cdr l1) l2))
        (t (cons (car l1) (union (cdr l1) l2)))))
(defun equal-sets-p (l1 l2)
  (and (= (length l1) (length l2))
       (my-subset l1 l2)))
(defun my-subset (l1 l2)
  (cond ((null l1) t)
        ((elementp (car l1) l2) (my-subset (cdr l1) l2))
        (t nil)))
(defun set (list)
  (merge-sort list))
(defun mein-fact-list (n)
  (mein-fact-list-hlp n 0 1))
(defun mein-fact-list-hlp (n ir acc)
  (cond ((= n ir) nil)
        ((= ir 0) (cons 1 (mein-fact-list-hlp n (+ ir 1) acc)))
        ((= ir 1) (cons 1 (mein-fact-list-hlp n (+ ir 1) acc)))
        (t (cons (* ir acc) (mein-fact-list-hlp n (+ 1 ir) (* ir acc))))))
          
(defun mein-fib-list (n)
  (mein-fib-list-hlp n 0 1 0))
(defun mein-fib-list-hlp (n i cr prev)
  (cond ((= i n) nil)
        ((= i 0) (cons 0 (mein-fib-list-hlp n (+ 1 i) cr prev)))
        ((= i 1) (cons 1 (mein-fib-list-hlp n (+ 1 i) cr prev)))
        (t (cons (+ prev cr) (mein-fib-list-hlp n (+ 1 i) (+ cr prev) cr)))))
    