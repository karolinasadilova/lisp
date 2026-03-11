#| polyglot-plist: (:YEAR 2025 :COURSE "PP1" :LECTURE 11 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 11_excercises.lisp -- řešení úloh k cvičení 11
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

;; Testy funkcionální grafiky
;; Testy jsou v komentářích, vyhodnocujte je jeden po druhém.

#|

(im-segment)
(im-gap)

(im-rotated (im-segment) (/ pi 4))
(im-scaled (im-segment) 1/2)
(im-normalized (im-rotated (im-segment) (/ pi -6)))

(im-hflipped (im-normalized (im-rotated (im-segment) (/ pi -6))))

(im-chain (im-segment)
          (im-rotated (im-segment) (/ pi 4))
          (im-rotated (im-segment) (* pi 3/4))
          (im-rotated (im-segment) pi))

(im-split (im-segment)
          (im-scaled (im-rotated (im-segment) (/ pi 5)) 2/3)
          (im-scaled (im-rotated (im-segment) (/ pi -5)) 2/3))

(im-vflipped
 (im-split (im-segment)
           (im-scaled (im-rotated (im-segment) (/ pi 5)) 2/3)
           (im-scaled (im-rotated (im-segment) (/ pi -5)) 2/3)))

|#

(defun tree-1 (n)
  (if (= n 0)
      nil
    (im-split (im-segment) 
              (im-scaled (im-rotated (tree-1 (1- n)) (/ pi 5)) 2/3)
              (im-scaled (im-rotated (tree-1 (1- n)) (/ pi -5)) 2/3))))

;; šetrnější k paměti:
(defun tree (n)
  (if (= n 0)
      nil
    (let ((gr (tree (- n 1))))
      (im-split (im-segment) 
                (im-scaled (im-rotated gr (/ pi 5)) 2/3)
                (im-scaled (im-rotated gr (/ pi -5)) 2/3)))))

#|
(tree 1)
(tree 2)
(tree 3)
(tree 5)

(im-scaled (tree 12) 0.6)


(im-normalized (im-row (tree 2) (tree 2) (tree 2) (tree 2)))
(im-scaled (im-row (tree 12) (im-gap) (tree 12)) 1/3)
(im-scaled  (im-row (tree 12) (im-scaled (im-gap) 1/2) (tree 12)) 1/3)

(im-column (tree 2) (im-vflipped (tree 2)))

|#


#|


(im-closed (im-chain (im-segment)
                     (im-rotated (im-segment) (/ pi 4))
                     (im-rotated (im-segment) (* pi 3/4))
                     (im-rotated (im-segment) pi)))

;; pozor:
(im-filled (im-segment))

(im-filled (im-chain (im-segment)
                     (im-rotated (im-segment) (/ pi 4))
                     (im-rotated (im-segment) (* pi 3/4))
                     (im-rotated (im-segment) pi)))

|#


;; pravidelný mnohoúhelník, m je počet vrcholů
(defun reg-polygon (m)
  (labels ((rp (n)
             (if (= n 0)
                 nil
               (im-chain (im-segment) 
                         (im-rotated (rp (1- n))
                                     (/ (* 2 pi) m))))))
    (im-closed (im-rotated (rp (1- m)) (/ pi -2)))))

#|

(reg-polygon 3)
(reg-polygon 5)

|#

(defun polygonal-spiral (m n)
  (if (= n 0)
      nil
    (im-chain (im-segment) 
              (im-rotated (im-scaled (polygonal-spiral m (- n 1))
                                     9/10)
                          (/ (* 2 pi) m)))))

#|
(polygonal-spiral 3 1)
(polygonal-spiral 3 2)
(polygonal-spiral 3 3)
(polygonal-spiral 3 8)
(im-normalized (polygonal-spiral 3 30))
(polygonal-spiral 5 30)
|#

(defun square ()
  (reg-polygon 4))

#|
(square)

(im-split (square) (im-rotated (square) (/ pi 2)))

(im-split (im-split (square) (im-rotated (square) (/ pi 2)))
          (square))

(im-normalized
 (im-split (im-split (im-split (im-split (square) (im-rotated (square) (/ pi 2)))
                               (square))
                     (im-rotated (square) (/ pi -2)))
           (im-rotated (square) pi)))

(im-deck (square) (im-normalized (im-rotated (square) (/ pi 4))))

|#


(defun squares (n)
  (if (= n 0)
      nil
    (im-deck (square)
             (im-normalized (im-rotated (squares (1- n)) (/ pi 30))))))

#|    
(im-scaled (squares 50) 2)
|#


;; Pythagorův strom
(defun pythagoras (n)
  (if (= n 0)
      nil
    (let ((tree2 (im-scaled (pythagoras (1- n)) (sqrt 1/2))))
      (im-split (im-split (im-filled (square))
                          (im-rotated tree2 (/ pi 4)))
                (im-rotated (im-hflipped tree2) (/ pi -4))))))

#|
(pythagoras 1)
(pythagoras 2)
(im-scaled (pythagoras 5) 1/3)
(im-scaled (pythagoras 6) 1/3)
(im-scaled (pythagoras 7) 1/3)
(im-scaled (pythagoras 10) 1/3)
(im-scaled (pythagoras 15) 1/3)
|#

(defun triangle ()
  (reg-polygon 3))

#|
(triangle)
(im-filled (triangle))
|#

(defun triplicated (obj)
  (im-column (im-row obj obj) obj))

(defun sierp (n)
  (if (zerop n)
      (im-filled (triangle))
    (triplicated (im-scaled (sierp (1- n)) 1/2))))

#|
(im-scaled (sierp 4) 2)
(im-scaled (sierp 6) 2)
(im-scaled (sierp 8) 2)
|#

(defun koch-polygon (n)
  (if (zerop n)
      (im-rotated (im-segment) (/ pi -2))
    (let ((next (im-scaled (koch-polygon (1- n)) 1/3)))
      (im-chain next
                (im-rotated next (/ pi 3))
                (im-rotated next (/ pi -3))
                next))))

#|
(koch-polygon 2)
(koch-polygon 3)
(koch-polygon 4)

(im-scaled (koch-polygon 6) 2.5)
|#

(defun better-koch-polygon (n)
  (if (zerop n)
      (im-rotated (im-segment) (/ pi -2))
    (let ((next (im-scaled (koch-polygon (1- n)) 1/3)))
      (im-brow next
               (im-rotated next (/ pi 3))
               (im-rotated next (/ pi -3))
               next))))

;;ZK:
;;1:vyhodnotí se sám na sebe: T
;;let vs let*:
;;(let ((a 2) (b (* 2 a)))
;  (* a b))
;(let ((a 2))
 ; (let ((b (* a 2)))
 ;   (* a b)))
(let* ((a 2) (b (* 2 a)))
  (* a b))
;;
(defun zk-list-tails (list)
  (if (null (cdr list)) (cdr list)
    (cons (copy-list (cdr list)) (zk-list-tails (cdr list)))))
(defun zk-new-from-two (seq)
  (lambda (n) (* (mem seq n)
                 (mem seq (+ n 1)))))
(defun zk-all-intervals (lower upper)
  (if (> lower upper) '()
    (append (zk-all-intervals-help lower lower upper) (zk-all-intervals (+ 1 lower) upper))))
(defun zk-all-intervals-help (start curr end)
  (if (> curr end) '()
    (cons (cons start curr) (zk-all-intervals-help start (+ 1 curr) end))))
(defun all-intervals (a b)
  (if (> a b)
      '()
    (append (all-intervals-h a a b)(all-intervals (+ a 1) b))))

(defun all-intervals-h (start current end)
  (if (> current end)
      '()
    (cons (cons start current) (all-intervals-h start (+ current 1) end))))
(defun square-spiral (n)
  (if (= n 0) nil
    (im-chain (im-segment)
              (im-rotated 
               (im-scaled 
(square-spiral (- n 1)) 3/4) (/ pi 2)))))
(defun my-if (a b c)
  (if a b c))
(defun zk-interval (left right)
  (if (> (max (lower-bound left) (lower-bound right))
         (min (upper-bound left) (upper-bound right)))
      (list nil)
    (interval (max (lower-bound left) (lower-bound right))
              (min (upper-bound left) (upper-bound right)))))
(defun random-numb-p (pred)
  (let ((n (random 100)))
    (if (funcall pred n) n
      (random-numb-p pred))))
(defun zk-zero-row-p (tbl row)
  (labels ((iter (col)
             (cond ((> col 9) t)
                   ((= 0 (funcall tbl row col)) (iter (+ 1 col)))
                   (t nil))))
    (iter 0)))
(let ((p (cons 3 4)))
  (cons (cons 1 p) (cons p 2)))
(defun interleave (l1 l2)
  (If (or (null l2) (null l1)) nil 
    (cons (car l1)
          (cons (car l2)(interleave (cdr l1) (cdr l2))))))
(defun pascal-row (row)
  (if (= row 0) (list 1)
    (pascal-row-help row 0)))
(defun pascal-row-help (row col)
  (if (> col row) nil
    (cons (pascal row col) (pascal-row-help row (+ 1 col)))))
(defun first-less (seq1 seq2)
  (labels ((iter (index)
             (if (< (mem seq1 index) (mem seq2 index)) index
               (iter (+ 1 index)))))
    (iter 0)))
(defun list-to-num (list)
  (labels ((iter (list acc)
             (if (null list) acc
               (iter (cdr list) (+ (car list) (* 10 acc))))))
    (iter list 0)))
(defun count-zk (list)
  (cond ((consp list) (+ (count-zk (car list)) (count-zk (cdr list))))
        ((null list) 1)
        (t 0)))
(defun every (list pred)
  (or (null list)
      (and (funcall pred list)
           (every (cdr list) pred))))
(defun seq-count-if-zk (seq n pred)
  (labels ((iter (i)
             (cond ((= i n) 0)
                   ((funcall pred (mem seq i)) (1+ (iter (+ 1 i))))
                   (t (iter (+ i 1))))))
    (iter 0)))
(defun each-2nd (list)
  (if (null list) nil
    (cons (car list)
          (odd-conses (cdr list)))))
(defun trianglp (a b c)
  (labels ((compare (x y)
             (> x y)))
    (and (compare (+ a b) c)
         (compare (+ b c) a)
         (compare (+ a c) b))))
(defun some (pred list)
  (and (not (null list))
       (or (funcall pred (car list))
           (some pred (cdr list)))))
(defun my-list-tails (list)
  (if (null (cdr list)) (cdr list)
    (cons (copy-list (cdr list))
          (my-list-tails (cdr list)))))
(defun transpose-pair (x)
  (cond ((consp x) (cons (transpose-pair (cdr x)) (car x)))
        (t x)))
(defun zk-digit-sum (n)
  (zk-digit-sum-help n (- (digit-count n) 1)))
(defun zk-digit-sum-help (n i)
  (if (< i 0) 0
    (+ (digit n i) 
       (zk-digit-sum-help n (- i 1)))))
(defun zk-div-9 (n)
  (or (= n 9) (= n 0)
      (and (< 9 n) (zk-div-9 (zk-digit-sum n)))))
(defun zk-div-3 (n)
  (or (= n 0) (= n 3) (= n 9) (= n 6)
      (and (< 9 n) (zk-div-3 (zk-digit-sum n)))))
                    
  

(defun last-n (list n)
  (last-n-k list list n)) 

(defun last-n-k (fast slow counter)
  (cond ((null fast)
         (if (= counter 0) slow counter))
        ((> counter 0) (last-n-k (cdr fast) slow (- counter 1)))
        (t (last-n-k (cdr fast) (cdr slow) counter))))
(defun triangle (v1 v2 v3)
  (list v1 v2 v3))
(defun v1 (triangle)
  (car triangle))
(defun v2 (triangle)
  (car (cdr triangle)))
(defun v3 (triangle)
  (car (cdr (cdr triangle)))) 

(defun triangle-center (triangle)
  (point (/ (+ (point-x (v1 triangle))
               (point-x (v2 triangle))
               (point-x (v3 triangle)))
            3)
         (/ (+ (point-y (v1 triangle ))
               (point-y (v2 triangle))
               (point-y (v3 triangle)))
            3)))
(defun my-sum-zk-digit (n)
  (labels ((iter (i)
             (if (< i 0) 0
               (+ (digit n i) (iter (- i 1))))))
    (iter (- (digit-count n) 1))))
(defun diff-sum-digit (n)
  (if (< n 10) n
    (+ (rem n 10)
       (diff-sum-digit (div n 10)))))
(defun even-elements (list)
  (cond ((null list) '())
        ((funcall #'evenp (car list)) (cons (car list)
                                            (even-elements (cdr list))))
        (t (even-elements (cdr list)))))

(defun my-set-inter-2 (set1 set2)
  (cond ((or (null set1) (null set2)) nil)
        ((elementp (car set1) set2) (cons (car set1) (my-set-inter-2 (cdr set1) set2)))
        (t (my-set-inter-2 (cdr set1) set2))))
(defun my-set-inter (set1 &rest sets)
  (foldr #'my-set-inter-2 sets set1))
(defun my-seq-interleave (seq1 seq2)
  (lambda (n) (if (evenp n) (mem seq1 (/ n 2))
                                 (mem seq2 (div n 2)))))
(defun self () (function self))
(defun sum-of-sq-tree(n)
  (labels ((iter (i acc)
             (cond ((= n 0) t)
                   ((= acc n) t)
                   ((> acc n) nil)
                   ((> i (sqrt n)) nil)
                   (t (or (iter (+ 1 i) acc)
                          (iter (+ 1 i) (+ (power2 i) acc)))))))
    (iter 0 0)))
(defun column (n tbl)
  (apply #'mapcar #'nth n tbl)) 
(defun list-to-numb-3 (list)
  (labels ((iter (list acc)
           (if (null list) acc
             (iter (cdr list) (+ (car list) (* 10 acc))))))
    (iter list 0)))
(defun list-to-num-4 (list)
  (labels ((iter (list acc)
             (if (null list) acc
               (iter (cdr list) (+ (car list) (* 10 acc))))))
    (iter list 0)))
(defun every2 (pred list)
  (or (null list)
      (and (funcall pred (car list))
           (every2 pred (cdr list)))))
(defun seq-find-zk (seq pred)
  (labels ((iter (index)
             (if (funcall pred (mem seq index)) index
               (iter (+ 1 index)))))
    (iter 0)))
(defun make-counter (n)
  (labels ((count (list)
             (cond ((null list) 0)
                   ((= n (car list)) (1+ (count (cdr list))))
                   (t (count (cdr list))))))
    (lambda (list) (count list))))
                      
(defun my-reverse (list)
  (my-revappend-2 list nil))
(defun my-revappend-2 (l1 l2)
  (if (null l1) l2
    (cons (my-revappend-2 (cdr l1) l2)
          (car l1))))
(defun my-reverse-4 (list)
  (foldr (lambda (element acc) (append acc (list element)))
         list nil))
                   
(defun m-list (list)
  (if (null list) nil
    (cons (foldr #'* list 1)
          (m-list (cdr list)))))

(defun trianglpp (a b c)
(labels ((compare (x y)
           (< x y)))
 (and (compare a (+ b c))
      (compare b (+ c a))
      (compare c (+ a b)))))
(defun euler (n)
  (euler-help n 1 1 0))

(defun euler-help (n i curr acc)
  (if (> i n) acc
    (euler-help n (+ i 1) (/ curr (+ i 1)) (+ curr acc))))
(defun sums-zk (list n)
  (cond ((= n 0) 1)
        ((null list) 0)
        ((< n 0) 0)
        (t (+ (sums-zk (cdr list) n)
              (sums-zk (cdr list) (- n (car list)))))))
(defun each2nd (list)
  (if (null list) nil
    (cons (car list) (odd-conses (cdr list)))))
(defun my-odd-conses (list)
  (if (null list) nil
    (each2nd (cdr list))))
(defun mein-seq-count (seq n pred)
  (labels ((iter (i)
             (cond ((= i n) 0)
                   ((funcall pred (mem seq i)) (+ 1 (iter (+ i 1))))
             (t (iter (+ i 1))))))
    (iter 0)))
               
(defun seq-shift (seq shift)
  (lambda (n) (let ((move (- n shift)))
                (if (< move 0) 0 (mem seq move)))))
(defun seq-shift-next (seq shift)
  (lambda (n) (if (< (- n shift) 0) 0 (mem seq (- n shift)))))


(defun tails(list)
  (if (null list) '()
    (cons (copy-list list)
          (tails (cdr list)))))
(defun mein-reverse (list)
  (foldr (lambda (el acc) (if (null list) nil (append acc (list el)))) list nil))
(defun same-sum-p (lists)
  (or (null lists)
      (null (cdr lists))
      (apply #'=
             (mapcar (lambda (sub) (apply #'+ sub))
                     lists))))
(defun seq-pred-10 (pred n seq)
  (labels ((iter (i)
             (cond ((= i n) nil)
                   ((funcall pred (mem seq i)) i)
                   (t (iter (+ i 1))))))
    (iter 0)))
(defun m-list-8 (list)
  (if (null list) '()
    (cons (foldr #'* list 1)
          (m-list-8 (cdr list)))))
(defun listo-numero (list)
  (labels ((iter (list acc)
             (if (null list) acc
               (iter (cdr list) (+ (car list) (* 10 acc))))))
    (iter list 0)))
(defun star (n)
  (star-help (/ pi n) n))
(defun star-help (deg n)
  (if (= n 0) nil
    (im-deck (im-rotated (im-segment) (* deg n))
             (star-help deg (- n 1)))))
(defun my-square-spiral (n)
  (if (= n 0) nil
    (im-chain (im-segment)
               (im-rotated (im-scaled (my-square-spiral (- n 1)) 3/4) (/ pi 2)))))
(defun mein-square (n)
  (if (= n 0) nil
    (im-chain (im-segment)
              (im-rotated (im-scaled (mein-square (- n 1)) 3/4) (/ pi 2)))))
(defun mein-star (n)
(mein-star-help n (/ pi n)))
(defun mein-star-help (n deg)
  (if (= n 0) nil
    (im-deck (im-rotated (im-segment ) (* n deg))
             (mein-star-help (- n 1) deg))))
(defun tree-height (tree)
  (if (null (node-children tree)) 0
    (+ 1 (tree-list-max (node-children tree)))))
(defun digitssum (n)
  (if (< n 10) n
    (+ (rem n 10)
       (digitssum (div n 10)))))
(defun intervalinter (i1 i2)
  (let ((l (max (lower-bound i1) (lower-bound i2)))
        (u (min (upper-bound i1) (upper-bound i2))))
    (if (> l u) nil
      (interval l u))))
(defun compress (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list (cons (car list) 1)))
        ((eql (car list)
              (car (cdr list)))
         (let ((res (compress (cdr list))))
           (cons (cons (car list)
                       (1+ (cdr (car res))))
                 (cdr res))))
        (t (cons (cons (car list)
                       1)
                 (compress (cdr list))))))
(defun compress2 (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list (cons (car list) 1)))
        ((eql (car list) (car (cdr list)))
         (let ((res (compress2 (cdr list))))
           (cons (cons (car list) (1+ (cdr (car res))))
                 (cdr res))))
        (t (cons (cons (car list) 1)
                 (compress (cdr list))))))
(defun mistrospirale (n)
  (if (= n 0) nil
     (im-chain (im-segment)
              (im-rotated (im-scaled (mistrospirale (- n 1)) 3/4) (/ pi 2))) ))
(defun sumero-digits (n)
  (labels ((iter (i)
             (if (< i 0) 0
               (+ (digit n i)
                  (iter (- i 1))))))
    (iter (- (digit-count n) 1))))
(defun sumero-digitale (n)
  (if (< n 10) n
    (+ (rem n 10)
       (sumero-digitale (div n 10)))))
(defun my-seq-interleave (seq1 seq2)
  (lambda  (n) (if (evenp n) (mem seq1 (div n 2)
                               )
                 (mem seq2 (div n 2)))))
(defun self-return ()
  (function self-return))
(defun listtails1 (list)
  (if (null list) nil
    (cons (copy-list list)
          (listtails1 (cdr list)))))
(defun listtails2 (list)
  (if (null (cdr list)) (cdr list)
            (cons (copy-list (cdr list))
                  (listtails2 (cdr list)))))
(defun list-numero (list)
  (labels ((iter (list acc)
             (if (null list) acc
               (iter (cdr list) (+ (car list) (* 10 acc))))))
    (iter list 0)))
(defun mein-revappend (l1 l2)
  (if (null l1) l2
    (mein-revappend (cdr l1) 
          (cons (car l1) l2))))
(defun mein-rev (l)
  (mein-revappend l nil))
(defun mein-reverse3 (l)
  (foldr (lambda (el acc) (append acc (list el))) list nil))
(defun sum-of-squares-wo (&rest numbs)
  (foldr #'+ (mapcar #'power2 numbs) 0))
(defun mein-rem (a b)
  (- a (* (floor (/ a b)) b)))
(defun seq-to-shifted (seq shift)
  (lambda (n) (if (< (- n shift) 0) 0
                (mem seq (- n shift)))))
(defun sums (list n)
  (cond ((= n 0) 1)
        ((null list) 0)
        (t (+ (sums (cdr list) n)
              (sums (cdr list) (- n (car list)))))))
(defun sums2 (list n)
  (cond ((zerop n) 1)
        ((or (null list) (< n 0)) 0)
        (t (+ (sums2 (cdr list) n)
              (sums2 (cdr list) (- n (car list)))))))
(defun same-sumsss-p (lists)
  (or (null lists) 
      (null (cdr lists))
      (apply #'= (lambda (sup) (mapcar (apply#'+ sub))) lists))))
(defun star3 (n)
  (labels ((star-help (deg n)
             (if (= n 0) nil
               (im-deck (im-rotated (im-segment) (* n deg))
                        (star-help deg (- n 1))))))
    (star-help (/ pi n) n)))
  

(defun sum-sq-p (n)
(labels ((iter (i acc)
           (cond ((= n 0) t)
                 ((= acc n) t)
                 ((> acc n) nil)
                 ((> i (sqrt n)) nil)
                 (t (or (iter (+ 1 i) acc)
                        (iter (+ 1 i) (+ (power2 i) acc)))))))
  (iter 0 0)))
(defun sum-sq-9 (&rest numbs)
  (foldr #'+ (mapcar #'power2 numbs) 0))
(defun my-comp (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list (cons (car list) 1)))
        ((eql (car list) (car (cdr list)))
         (let ((res (my-comp (cdr list))))
           (cons (cons (car list) (1+ (cdr (car res))))
                 (cdr res))))
         (t (cons (cons (car list) 1)
                  (my-comp (cdr list))))))
(defun div-by-3 (n)
  (or (= n 3) (= n 6) (= n 9) (= n 0)
      (and (> n 9) (div-by-3 (3digit-sum n)))))
(defun 3digit-sum (n)
  (labels ((iter (i)
             (if (< i 0) 0
               (+ (digit n i) (iter (- i 1))))))
    (iter (- (digit-count n) 1))))
(defun szn (list k n)
  (cond ((null list) nil)
        ((> k 0) (szn (cdr list) (- k 1) n))
        ((> n 0) (cons (car list) (szn (cdr list) 0 (- n 1))))
        (t nil)))
(defun star-n (n)
(star-n-helper (/ pi n) n))
(defun star-n-helper (deg n)
  (if (= n 0) nil
    (im-deck (im-rotated (im-segment ) (* deg n))
             (star-n-helper deg (- n 1)))))
(defun div3 (n)
  (labels ((digit-sum (i) 
             (if (< i 0) 0
               (+ (digit n i) (digit-sum (- i 1))))))
    (or (= n 0) (= n 3) (= n 6) (= n 9)
        (and (> n 9) (div3 (digit-sum (- (digit-count n) 1)))))))
(defun trianglp2 (a b c)
  (labels ((bigger (x y)
             (> x y)))
    (and (bigger (+ a b) c)
         (bigger (+ b c) a)
         (bigger (+ c a) b))))
(defun list-tail2 (list)
  (if (null list) nil
    (cons (copy-list list)
          (list-tail2 (cdr list)))))
(defun subintervals2 (left right)
  (if  (> left right) nil
    (append (subintervals2-helper left left right)
            (subintervals2 (+ 1 left) right))))
(defun subintervals2-helper (start current end)
  (if (> current end) nil
    (cons (cons start current)
          (subintervals2-helper start (+ 1 current) end))))
(defun some (list pred)
  (cond ((null list) nil)
        ((funcall pred (car list)) t)
        (t (some (cdr list) pred))))
(defun some2 (list pred)
  (and (not (null list))
       (or (funcall pred (car list))
            (some2 (cdr list) pred))))
(defun seq-pred-2 (seq)
  (lambda (n) (* (mem seq n)
                 (mem seq (+ 1 n)))))
(defun sq-spiral (n)
  (if (= n 0) nil
    (im-chain (im-segment)
              (im-rotated (im-scaled (sq-spiral (- n 1)) 3/4) (/ pi 2)))))
(defun list-to-numero22 (list)
  (labels ((iter (list acc)
             (if (null list) acc
               (iter (cdr list) (+ (car list) (* acc 10))))))
    (iter list 0)))
(defun sumslist22 (n list)
  (cond ((= n 0) 1)
        ((null list) 0)
        (t (+ (sumslist22 n (cdr list))
           (sumslist22 (- n (car list)) (cdr list))))))
(defun every22 (list pred)
  (or (null list)
        (and (funcall pred (car list))
             (every22 (cdr list) pred))))
(defun each2nd (list)
  (if (null list) nil
    (cons (car list) (oddconses (cdr list)))))
(defun oddconses (list)
  (if (null list) nil
    (each2nd (cdr list))))
(defun seq-count-if-22 (seq pred n )
  (labels ((iter (i)
             (cond ((= i n) 0)
                   ((funcall pred (mem seq n)) (+ 1 (iter (+ 1 i))))
                   (t (iter (1+ i))))))
    (iter 0)))
  

;;final repe
(defun last-n (list n)
(last-n-helper list list n))
(defun last-n-helper (fast slow count)
  (cond ((null fast) (if (= count 0) slow count))
        ((> count 0) (last-n-helper (cdr fast) slow (- count 1)))
        (t (last-n-helper (cdr fast) (cdr slow) count))))
(defun tree-hei (node)
  (1+ (tree-hei-help (cdr node))))
(defun tree-hei-help (nodes)
  (if (null nodes) 0
    (+ (tree-hei (car nodes))
       (tree-hei-help (crd nodes)))))
(defun my-tree-hei (tree)
  (if (null (node-children tree)) 0
    (+ 1 (max-tree-list (node-children tree)))))
(defun max-tree-list (roots)
  (if (null roots) 0
    (max (my-tree-hei (car roots))
         (max-tree-list (cdr roots)))))

                      
(defun my-count-if (seq p n)
  (labels ((iter (i)
             (cond ((= i n) 0)
                   ((funcall p (mem seq i)) (+ 1 (iter (+ 1 i))))
                   (t (iter (+ i 1))))))
    (iter 0)))
(defun seq-count-if3 (seq n test)
  (labels ((counter (c)
             (cond ((> c n) 0)
                   ((funcall test (mem seq c)) (1+ (counter (1+ c))))
                   (t (counter (1+ c))))))
    (counter 0)))
(defun my-primep (n)
  (labels ((iter (n i)
             (cond ((= n 2) t)
                   ((= i n) t)
                   ((= 0 (rem n i)) nil)
                   (t (iter n (+ 1 i))))))
    (iter n 2)))
(defun summ3 (list)
  (if (null (cdr list)) (cons 0 nil)
    (cons (foldr #'+ (cdr list) 0)
          (summ3 (cdr list)))))
(defun deph (struc)
  (if (atom struc) 0
    (1+ (max (deph (car struc))
            (deph (cdr struc))))))
                  
(defun depth (struct)
 (if (atom struct)
	      0
	    (1+ (max (depth (car struct)) (depth (cdr struct))))))
(lambda (n) (funcall (lambda (self n) (if (= n 0) 1 
                                        (* n (funcall self self (- n 1)))))
                       (lambda (self n) (if (= n 0) 1 
                                          (* n (funcall self self (- n 1))))) 
    n))

(let ((f (lambda (fun n) (if (= n 0) 1
                           (* n (funcall fun fun (- n 1)))))))
         
  (funcall f f 3))

           
(defun column (n lists)
  (if (null lists) nil
    (mapcar (lambda (list) (funcall #'nth n list)) lists)))
(defun column2 (n lists)
  (if (null lists) nil
    (mapcar (lambda (list) (funcall #'nth n list)) lists)))
(defun my-make-counter (n)
  (labels ((iter (list)
             (cond ((null list) 0)
                   ((= (car list) n) (+ 1 (iter (cdr list))))
                   (t (iter (cdr list))))))
    (lambda (list) (iter list))))
(defun my-reverso (list)
  (labels ((my-rev (l1 l2)
             (if (null l1) l2
               (my-rev (cdr l1) (cons (car l1) l2)))))
    (my-rev list nil)))
(defun my-m-list (list)
  (if (null list) nil
    (cons (foldr '* list 1)
          (my-m-list (cdr list)))))
(defun same-sums22 (lists)
  (cond ((null lists) t)
        ((null (cdr lists)) t)
        (t (apply #'= 
                  (mapcar (lambda (sub) (foldr #'+ sub 0)) lists)))))
(defun same-sum-p (lists)
  (or (null lists)
      (null (cdr lists))
      (apply #'=
             (mapcar (lambda (sub) (apply #'+ sub))
                     lists))))
(defun lasttt(list n)
  (cond ((null list) n)
        ((= n 0) nil)
        ((= n 1) list)
        (t (lasttt (cdr list) (- n 1)))))
(defun lastest (list n)
  (cond ((null list) n)
        ((= n 0) nil)
        ((= n 1) list)
        (t (lastest (cdr list) (- n 1)))))
(defun make-arit (a1 n d acc)
  (if (= n 0) (reverse acc)
    (make-arit (+ a1 d) (- n 1) d (cons a1 acc))))
(defun makearit2 (a1 n d)
  (make-arit a1 n d nil))
(defun counter (n)
  (labels ((count (list)
             (cond ((null list) 0)
                   ((= n (car list)) (1+ (count (cdr list))))
                   (t (count (cdr list))))))
    (lambda (list) (count list))))
(defun m-listo (list)
  (if (null list) nil
    (cons (apply #'* list)
          (m-listo (cdr list))))) 
(defun last (n list)
  (cond ((null list) n)
        ((= n 0) nil)
        ((= n 1) list)
        (t (last (- n 1) (cdr list)))))
(defun coltbl (lists n)
  (if (null lists) nil
    (mapcar (lambda (list) (funcall #'nth n list)) lists)))
(defun tailen (list)
  (if (null (cdr list)) nil
    (cons (cdr list)
          (tailen (cdr list)))))
(defun tailen2 (list)
  (if (null  list) nil
    (cons  list
          (tailen2 (cdr list)))))
(defun summero (list)
  (if (null list) nil
    (cons (apply #'+ list)
          (summero (cdr list)))))
(defun compress3 (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list (cons (car list) 1)))
        ((eql (car list) (car (cdr list))) 
         (let ((rest (compress3 (cdr list))))
           (cons (cons (car list) (+ 1 (cdr (car rest))))
                 (cdr rest))))
        (t (cons (cons (car list) 1)
                 (compress3 (cdr list))))))
(defun counter (n)
  (labels ((count (list)
             (cond ((null list) 0)
                   ((= n (car list)) (1+ (count (cdr list))))
                   (t (count (cdr list))))))
    (lambda (list) (count list))))
(defun depth3 (struct)
  (if (atom struct) 0
    (1+ (max (depth (car struct))
             (depth (cdr struct))))))
(defun sublists5 (left ri)
  (if (> left ri) nil
    (append (sub-help left left ri)
            (sublists5 (+ 1 left) ri))))
(defun sub-help (start curr end)
 (if (> curr end) nil
  (cons (cons start curr) (sub-help start (+ 1 curr) end))))
(defun interleavelist (l1 l2)
  (if (or (null l1) (null l2))
      nil
    (cons (car l1) (cons (car l2) (interleavelist (cdr l1) (cdr l2))))))
(defun interleavel (l1 l2)
  (if (or (null l1) (null l2)) nil
      (cons (car l1) (cons (car l2) (interleavel (cdr l1) (cdr l2))))))
(defun facttolist (n)
  (labels ((iter (i acc)
             (cond ((= i n) nil)
                   ((= i 0) (cons 1 (iter (+ i 1) acc)))
                   (t (cons (* i acc) (iter (+ i 1) (* i acc)))))))
    (iter 0 1)))
(defun fiblist (n)
  (labels ((iter (i curr prev)
             (cond ((= i n) nil)
                   ((= i 0) (cons 0 (iter (+ 1 i) curr prev)))
                   ((= i 1) (cons 1 (iter (+ 1 i) curr prev)))
                   (t (cons (+ curr prev) (iter (+ i 1) (+ prev curr) curr))))))
    (iter 0 1 1)))
(defun samesublists (lists)
  (cond ((null lists) t)
        ((null (cdr lists)) t)
        (t (apply #'= (mapcar (lambda (sublist) (apply #'+ sublist)) lists)))))
(defun factorials10 (n)
  (labels ((iter (i acc)
             (cond ((= i n) nil)
                   ((= i 0) (cons 1 (iter (+ 1 i) acc)))
                   (t (cons (* i acc) (iter (+ i 1) (* acc i)))))))
    (iter 0 1)))
(defun fibtolist10 (n)
  (labels ((iter (i curr prev)
             (cond ((= i n) nil)
                   ((= i 0) (cons 0 (iter (+ 1 i) curr prev)))
                   ((= i 1) (cons 1 (iter (+ 1 i) curr prev)))
                   (t (cons (+ curr  prev) (iter (+ i 1) (+ curr prev) curr))))))
    (iter 0 1 0)))
(lambda (n)
  (funcall
   (lambda (self n)
     (if (= n 0)
         1 
       (* n (funcall self self (- n 1)))))
   (lambda (self n)
     (if (= n 0)
         1 
       (* n (funcall self self (- n 1)))))
  n))
         
(defun y (x)
  (if (consp x)
      (cons (y (cdr x))
            (y (car x)))
x))
(defun list-tails (list)
  (if (null (cdr list)) nil
    (cons (cdr list) (list-tails (cdr list))))))
(defun new-from-two (seq)
  (lambda (n) (* (mem seq n) (mem seq (+ n 1)))))
(defun intervaly (left right)
  (if (> left right) '()
      (append (intervaly-help left left right)
              (intervaly (+ left 1) right))))
(defun intervaly-help (start curr end)
  (if (> curr end) nil
      (cons (cons start curr) (intervaly-help start (+ 1 curr) end))))

(defun tree-height3 (tree)
  (if (null (node-children tree))
      0
    (+ 1 (tree-list-max (node-children tree)))))

(defun tree-list-max (roots)
  (if (null roots)
      0
    (max (tree-height3 (car roots))(tree-list-max (cdr roots)))))

(defun square-sp (n)
  (if (= n 0) nil
    (im-chain (im-segment)
              (im-rotated (im-scaled (square-sp (- n 1)) 3/4) (/ pi 2)))))
(defun my-if (a b c)
  (if a b c))
(defun interval (left right)
  (cons left right))
(defun left (interval)
  (car interval))
(defun right (interval)
  (cdr interval))
(defun interval-intersection (int1 int2)
  (let ((x (max (left int1) (left int2)))
        (y (min (right int1) (right int2))))
    (if (> x y) nil
      (interval x y))))
(defun random-number-p (pred)
  (let ((n (random 100)))
    (if (funcall pred n) n
      (random-number-p pred))))
(defun zero-row-pp (tbl row)
  (labels ((iter (column)
             (cond ((> column 9) t)
                   ((= 0 (funcall tbl row column)) (iter (+ 1 column)))
                   (t nil))))
    (iter 0)))
(defun interleave (list1 list2)
  (cond ((null list1) list2)
        ((null list2) list1)
        (t (cons (car list1)
                 (cons (car list2) (interleave (cdr list1) (cdr list2)))))))
(defun my-pascal (row column)
  (if (or (= 0 row) (= 0 column) (= row column)) 1
    (+ (my-pascal (- row 1) column)
       (my-pascal (- row 1) (- column 1)))))
(defun pascal-row (row)
  (labels ((iter (col)
             (if (> col row) nil
               (cons (my-pascal row col) (iter (+ 1 col))))))
    (iter 0)))
(defun pasc-row (row)
  (if (= row 0) '(1)
    (let ((prev (pascal-row (- row 1))))
      (cons 1 (mapcar '+ prev 
                      (append (cdr prev) '(0)))))))
(defun first-less (seq1 seq2)
  (labels ((iter (index)
             (if (< (mem seq1 index) (mem seq2 index)) index
               (iter (+ 1 index)))))
    (iter 0)))
(defun max-path (node)
  (if (null (node-children node))
      (list (node-value node))
    (cons (node-value node)(find-longest (mapcar #'max-path (node-children node))))))

(defun find-longest (paths)
  (if (null paths)
      nil
    (let ((path1 (node-value paths))
          (path2 (find-longest (node-children paths))))
      (if (< (length path1) (length path2))
          path2
        path1))))
(defun divibleby3 (n)
  (or (= n 0) (= n 3) (= n 6) (= n 9)
      (and (> n 9) (divibleby3 (digit-sum n)))))
(defun digit-sum (n)
  (labels ((iter (i)
             (if (< i 0) 0
    (+ (digit n i) (iter (- i 1))))))
  (iter (- (digit-count n) 1))))
(defun divby3 (n)
  (labels ((iter (i)
             (if (< i 0) 0
               (+ (digit n i) (iter (- i 1))))))
    (or (= n 0) (= n 3) (= n 6) (= n 9)
        (and (< 9 n) (divby3 (iter (- (digit-count n) 1)))))))
   
(defun trianglppp (a b c)
  (labels ((compare (x y)
             (> x y)))
    (and (compare (+ a b) c)
         (compare (+ a c) b)
         (compare (+ b c) a))))
(defun tails (list)
  (if (null list) nil
    (cons list
          (tails (cdr list)))))

(defun transpose-pair (x)
  (if (consp x)
	(cons (transpose-pair (cdr x))
		(transpose-pair (car x)))
    x))
(defun sub-intervals (left ri)
 (if (> left ri) nil
   (append (sub-int-help left left ri) (sub-intervals (+ 1 left) ri))))
(defun sub-int-help (start curr end)
  (if (> curr end) nil
    (cons (cons start curr) (sub-int-help start (+ 1 curr) end))))
(defun last-n (list n)
(last-n-helper list list n))
  
(defun last-n-helper (slow fast count)
  (cond ((null fast) (if (= count 0) slow count))
        ((> count 0) (last-n-helper slow (cdr fast) (- count 1)))
        (t (last-n-helper (cdr slow) (cdr fast) count))))
(defun some (list pred)
  (and (not (null list))
       (or (funcall pred (car list))
           (some (cdr list) pred))))
(defun seq-pred-2 (seq)
  (lambda (n) ( * 
                (mem seq n) 
                (mem seq (+ 1 n)))))
(defun list-to-number (list)
  (labels ((iter (list acc)
             (if (null list) acc
               (iter (cdr list) (+ (car list) (* 10 acc))))))
    (iter list 0)))
(defun sums (n list)
  (cond ((= n 0) 1)
        ((null list) 0)
       (t  (+ (sums n (cdr list))
           (sums (- n (car list)) (cdr list))))))
(defun count (list)
  (cond ((consp list) (+ (count (car list)) (count (cdr list))))
        ((null list) 1)
        (t 0)))
(defun every (list pred)
  (or (null list)
      (and (funcall pred (car list))
           (every (cdr list) pred))))
                         
(defun each-2nd (list)
  (cond ((null list)
         '())
        ((null (cdr list)) (cons (car list) '()))
        (t (cons (car list)
          (each-2nd (cddr list))))))
(defun each2nd (list)
  (cond ((null list) nil)
        ((null (cdr list)) (cons (car list) nil))
        (t (cons (car list)
                 (each2nd (cddr list))))))
(defun seq-count-if-3 (pred seq n)
  (labels ((iter (i)
             (cond ((= i n) 0)
                   ((funcall pred (mem seq i)) (1+ (iter (1+ i))))
                   (t (iter (1+ i))))))
    (iter 0)))
(defun tree-level (tree n)
  (if (= n 0) (list (node-value tree))
    (tree-list-level (node-children tree) (- n 1))))
(defun tree-list-level (trees n)
  (if (null trees) nil
    (append (tree-level (car trees) n)
            (tree-list-level (node-children trees) n))))
(defun triangllp (a b c)
  (labels ((bigger (x y)
             (> x y)))
    (and (bigger (+ a b) c)
         (bigger (+ a c) b)
         (bigger (+ b c) a))))
  
(defun my-signum (n)
  (if (or (> n 0) (< n 0)) 1
        (t 0)))

(defun euler (n)
  (euler-help n 0 1 0))
(defun euler-help (n i curr acc)
  (if (> i n) acc
    (euler-help n (+ i 1) (/ curr (+ i 1)) (+ acc curr))))

(defun same-sums-pp (lists)
  (apply #'= (mapcar (lambda (n) (apply #'+ n)) lists)))

(defun my-sum (list)
  (labels ((s (n len)
             (if (> n len) 0
    (+ (nth n list) (s (+ n 1) len)))))
    (s 0 (length list))))
(defun my-reverse (list)
  (labels ((revappend (l1 l2)
             (if (null l1) l2
               (revappend (cdr l1) (cons (car l1) l2)))))
    (revappend list nil)))
(defun seq-shift (seq shift)
  (lambda (n) (if (< (- n shift) 0) 0 (mem seq (- n shift)))))





(defun prefix-to-infix (list)
  (cond ((atom list) list) ;;není to seznam jen číslo nebo symbol 5 nebo x treba tak ten list vratim
        ((or (null (cdr list))(null (cdr (cdr list))))(mapcar #'prefix-to-infix list));;kdyz ma seznam jen 1 prvek nebo ma jen 2 prvky operator a arg treba (+ 1)...unární operátor, nema klasicky tvar (op ar1 ar2)...NEDĚLÁM infix protože nemám 2 argumenty->aplikuje funkci na kazdy prvek seznamu
        (t (let ((op (car list))
                 (args (mapcar #'prefix-to-infix (cdr list))))

             (foldl (lambda (a b) 
                      (list a op b))
                    (cdr args) (car args)))))) ;;treti vetev je normalni prefixovy vyraz-list neni atom a ma aspon 3 prvky, car=operator, cdr=argumenty kromě operátoru, mapcar je rekurzivně převede na infix-kdyby byly vnořené výrazy, lambda vytvori infix, 
;;musi tam byt foldl aby se to skladalo zleva ...u + a * by nevadil foldr ale u deleni atd by to vadilo


(defun my-tree-height (node)
  (if (null (node-children node)) 0
    (1+ (apply #'max (mapcar #'my-tree-height (node-children node))))))




;;maximalni delka cesty ve stromě:
;;max-path
(defun longer (a b)
  (if (> (length a) (length b)) a b))
(defun my-max-path-list (nodes)
  (if (null (cdr nodes)) (max-path (car nodes))
    (longer (max-path (car nodes))
            (my-max-path-list (cdr nodes)))))
(defun max-path (node)
  (if (null (node-children node) (list (node-value node)))
      (cons (node-value node)
            (my-max-path-list (node-children node)))))

;;trojuhelnik konstruktor+selektory a funkce triangl center:
(defun point (a b)
  (cons a b))
(defun point-x (point)
  (car point))
(defun point-y (point)
  (cdr point))
(defun triangle (v1 v2 v3)
  (list v1 v2 v3))
(defun v1 (triangle)
  (car triangle))
(defun v2 (triangle)
  (car (cdr triangle)))
(defun v3 (triangle)
  (car (cdr (cdr triangle))))
(defun triangle-centre (triangle)
  (point (/ (+ (point-x (v1 triangle))
               (point-x (v2 triangle))
               (point-x (v3 triangle)))
            3)
         (/ (+ (point-y (v1 triangle))
               (point-y (v2 triangle))
               (point-y (v3 triangle)))
            3)))


;;sum-digits
;;s floor a rem:
(defun sum-digits (number)
  (if (< number 10) number
    (+ (rem number 10)
       (sum-digits (div number 10)))))
;;rem 10 vrati posledni cifru, div 10 odsekne posledni cifru
;bez divu s floorem:
(defun sum-digits1 (number)
  (if (< number 10) number
    (+ (rem number 10)
       (sum-digits1 (floor (/ number 10))))))
;;bez remu, flooru a divu:
(defun sum-digits2 (number)
  (labels ((iter (i)
             (if (< i 0) 0
                   (+ (digit number i) (iter (- i 1))))))
    (iter (- (digit-count number) 1))))
;;even-elements
(defun even-elements (list)
  (cond ((null list) nil)
        ((funcall #'evenp (car list)) (cons (car list) (even-elements (cdr list))))
        (t (even-elements (cdr list)))))

;;mnozina=list...usporadane vzestupne
;;my-set-intersection2 vrati prunik
(defun my-intersection2 (set1 set2)
  (cond ((or (null set1) (null set2)) nil)
        ((elementp (car set1) set2) (cons (car set1) (my-intersection (cdr set1) set2)))
        (t (my-intersection2 (cdr set1) set2))))
;;bez elememtu:
(defun my-int2 (set1 set2)
  (cond ((or (null set1) (null set2)) '())
        ((< (car set1) (car set2)) (my-int2 (cdr set1) set2))
        ((> (car set1) (car set2)) (my-int2 set1 (cdr set2)))
        (t (cons (car set1)
                 (my-int2 (cdr set1) (cdr set2))))))
;seq-interleave vrací pslp ve ktere se stridaji cleny 1 a 2 pslp:
(defun seq-interleave (seq1 seq2)
  (lambda (n) (if (evenp n) (mem seq1 (div n 2))
                (mem seq2 (div n 2)))))
(defun intersections (set1 &rest sets)
  (foldr #'my-intersection2 sets set1))
;;find-path hledani cesty ve stromu 
(defun find-path (node el)
  (if (eql el (node-value node)) (list (node-value node))
         (let((subpath (find-path-in-children (node-children node) el)))
             (if (null subpath) nil
               (cons (node-value node) subpath)))))
(defun find-path-in-children (nodes el)
  (if (null nodes) nil
    (or (find-path (car nodes) el)
        (find-path-in-children (cdr nodes) el))))

;;node=tree


;;self funkce bez parametrů
(defun selfreturn ()
  (function selfreturn))
(defun square-spiral (n)
  (if (= n 0) nil
    (im-chain (im-segment)
              (im-rotated (im-scaled (square-spiral (- n 1)) 3/4) (/ pi 2)))))
(defun list-tails (list)
(if (null list) nil
  (cons list
        (list-tails (cdr list)))))
(defun vec-+ (vec &rest vecs)
  (foldr #'vec-+-2 vecs vec))
 
(defun antilistp (antilist)
  (or (eql antilist nil)
      (and (consp antilist)
           (antilistp (car antilist)))))
(defun last-n (list n)
(last-help list list n))
(defun last-help (fast slow count)
  (cond ((null fast)
         (if (= count 0) slow count))
        ((> count 0) (last-help (cdr fast) slow (- count 1)))
        (t (last-help (cdr fast) (cdr slow) count))))
(defun compress (list)
  (cond ((null list) nil)
        ((null (cdr list)) (cons (car list) 1))
        ((eql (car list) (car (cdr list)))
         (let ((rest (compress (cdr list))))
             (cons (cons (car list) (1+ (cdr (car rest))))
                   (cdr rest))))
        (t (cons (cons (car list) 1)
                 (compress (cdr list))))))
                   
                       
  
(defun sum-squares (&rest args)
  (foldr #'addsq args 0))
(defun addsq (x acc)
  (+ (power2 x) acc))

(defun sublist (list k n)
  (cond ((or (= 0 n)(null list)) nil)
        ((> k 0) (sublist (cdr list) (- k 1) n))
        (t (cons (car list)
                 (sublist (cdr list) 0 (- n 1))))))

(defun my-gcd (a b)
  (if (= b 0) a
    (my-gcd b (rem a b))))
(defun my-reverse (list)
  (labels ((my-revappend (l1 l2)
             (if (null l1) l2
              (my-revappend (cdr l1) (cons (car l1) l2)))))
    (my-revappend list nil)))
           
    
(defun my-set-intersection (set1 set2)
  (cond ((or (null set1) (null set2)) nil)
        ((> (car set1) (car set2)) (my-set-intersection set1 (cdr set2)))
        ((< (car set1) (car set2)) (my-set-intersection (cdr set1) set2))
        (t (cons (car set1) (my-set-intersection (cdr set1) (cdr set2))))))
(defun my-set-intersection-el (set1 set2)
  (cond ((or (null set1) (null set2)) nil)
        ((elementp (car set1) set2) (cons (car set1) (my-set-intersection-el (cdr set1) set2)))
        (t (my-set-intersection-el (cdr set1) set2))))

(defun my-rev (list)
  (labels ((my-rev (l1 l2)
             (if (null l1) l2
               (my-rev (cdr l1) (cons (car l1) l2)))))
    (my-rev list nil)))
(defun my-reverse-2 (list)
  (foldr (lambda (x acc) (append acc (list x))) list nil))
(defun my-rev2 (list)
  (foldr (lambda (x acc) (append acc (list x))) list nil)
;;append spojuje seznamy
;;foldr: (f a (f b (f c acc)))
;foldr jde z prava
(defun zero-col-p (fun col)
  (labels ((iter (row)
             (cond ((> row 9) t)
                   ((= 0 (funcall fun row col)) (iter (+ 1 row)))
                   (t nil))))
    (iter 0)))
(defun column (lists index)
    (mapcar (lambda (list) (funcall #'nth index list)) lists))
(defun count-equal-partiotions (list)
  (labels ((iter (list 