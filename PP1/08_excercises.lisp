#| polyglot-plist: (:YEAR 2025 :COURSE "PP1" :LECTURE 8 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 08_excercises.lisp -- řešení úloh k cvičení 8
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

;; příklady z přednášky
(defun log-10 (x)
  (log x 10))

(defun list-log-10 (list)
  (mapcar #'log-10 list))

(defun list-inc (list)
  (mapcar #'1+ list))

;; Alternativní definice
(defun my-append-2 (list1 list2)
  (foldr #'cons list1 list2))

;; Alternativní definice
(defun my-remove (x list)
  (labels ((rem (el accum)
             (if (eql x el) accum (cons el accum))))
    (foldr #'rem list '())))

;; Další ukázky:
(defun sum-lists (list1 &rest lists)
  (apply #'mapcar #'+ list1 lists))

(defun sum-lists (list1 &rest lists)
  (foldr #'sum-lists-2 lists list1))

(defun scalar-product (list1 list2)
  (foldr #'+ (mapcar #'* list1 list2) 0))

;; Toto by nemělo fungovat!
(defun my-scale-list (list factor)
  (labels ((prod (x) (* x factor)))
    (mapcar #'prod list)))
(defun my-mapcar-1 (fun list)
  (if (null list) nil
    (cons (funcall fun (car list)) (my-mapcar-1 fun (cdr list)))))
(defun my-findt (x list test)
  (cond ((null list) nil)
        ((funcall test x (car list)) (car list))
        (t (my-findt x (cdr list) test))))
(defun my-find-if (pred list)
  (cond ((null list) nil)
        ((funcall pred (car list)) (car list))
        (t (my-find-if pred (cdr list)))))
(defun my-removet (x test list)
  (cond ((null list) nil)
        ((funcall test x (car list)) (my-removet x test (crd list)))
        (t (cons (car list) (my-removet x test (cdr list))))))
(defun my-foldr (fun list init)
  (if (null list) init
    (funcall fun (car list) (my-foldr fun (cdr list) init))))
(defun my-sum-lits (l1 &rest lists)
  (foldr #'sum-lists-2 lists l1))
(defun mein-sum-lists (l1 &rest lists)
  (apply #'mapcar #'+ l1 lists))
(defun my-scale-list-r (factor list)
  (labels ((iter (x)
             (* x factor)))
  (mapcar #'iter list)))
(defun membert (x list test)
  (cond ((null list) nil)
        ((funcall test x (car list)) list)
        (t (membert x (cdr list) test))))
(defun member-if (pred list)
  (cond ((null list) nil)
        ((funcall pred (car list)) list)
        (t (member-if pred (cdr list)))))
(defun countt (x test list)
  (cond ((null list) 0)
        ((funcall test x (car list)) (1+ (countt x test (cdr list))))
        (t (countt x test (cdr list)))))
(defun count-if (pred list)
  (cond ((null list) 0)
        ((funcall pred (car list)) (1+ (count-if pred (cdr list))))
        (t (count-if pred (cdr list)))))
(defun merge-sort-8 (list pred)
  (cond ((or (null list) (null (cdr list))) list)
        (t (merge-8 pred (merge-sort-8 (even-conses list) pred)
                  (merge-sort-8 (odd-conses list) pred)))))
(defun merge-8 (pred l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        ((funcall pred (car l1) (car l2)) (cons (car l1) (merge-8 pred (cdr l1) l2)))
        (t (cons (car l2) (merge-8 pred l1 (cdr l2))))))
                  
(defun arithmetic-mean (num &rest numbs)
  (/ (foldr #'+ numbs num) (1+ (length numbs))))
(defun equal-lists-p (l1 &rest lists)
  (cond ((null lists) t)
        ((equal-lists-2 l1 (car lists)) (apply #'equal-lists-p l1 (cdr lists)))
        (t nil)))
(defun equal-lists-2 (l1 l2)
  (cond ((and (null l2) (null l1)) t)
        ((or (null l1) (null l2)) nil)
        ((eql (car l1) (car l2)) (equal-lists-2 (cdr l1) (cdr l2)))
        (t nil)))
(defun foldl (fun list init)
  (if (null list) init
    (foldl fun (cdr list) (funcall fun (car list) init))))
(defun mein-foldr (fun list init)
  (cond ((null list) init)
        (t (funcall fun (car list) (mein-foldr fun (cdr list) init)))))


 