#| polyglot-plist: (:YEAR 2025 :COURSE "PP1" :LECTURE 7 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 07_excercises.lisp -- řešení úloh k cvičení 7
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

(defun f (symbol)
  (list symbol))
(defun fact-tail (n)
  (labels ((iter (n acc)
             (if (= n 0) acc
               (iter (- n 1) (* n acc)))))
    (iter n 1)))
(defun test-rest (a b &rest letters)
  (list a b letters))
(defun my-append (&rest lists)
  (labels ((app-2 (l1 l2)
             (if (null l1)
                 l2
               (cons (car l1) (app-2 (cdr l1) l2))))
           (app (lists)
             (if (null lists) '()
               (app-2 (car lists) (app (cdr lists))))))
    (app lists)))
(defun my-append-repte (&rest lists)
  (labels ((app-2 (l1 l2)
             (if (null l1)
                 l2
               (cons (car l1) (app-2 (cdr l1) l2))))
           (app (lists)
             (if (null lists) '()
               (app-2 (car lists) (app (cdr lists))))))
    (app lists)))