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