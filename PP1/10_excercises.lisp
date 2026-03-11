#| polyglot-plist: (:YEAR 2025 :COURSE "PP1" :LECTURE 10 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 10_excercises.lisp -- řešení úloh k cvičení 10
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
(defun my-test-structure (struct test)
  (if (atom struct) 
      (if (funcall test struct) struct nil)
    (let ((res (my-test-structure (car struc) test)))
          (if (null res) (my-test-structure (cdr struc) test) res))))
(defun my-struct-find-if (test struc)
  (if (atom struc) (and (funcall test struc) struc)
    (or (struc-find-if test (car struc))
        (struc-find-if test (cdr struc)))))
(defun my-struc-count (struc el)
  (cond ((cosnp struc) (+ (my-struc-count (car struc))
                          (my-struc-count (cdr struc))))
        ((eql struc el) 1)
        (t 0)))
(defun my-reverse (list)
  (foldr (lambda (element acc) (append acc (list element)))
         list nil))
          

;;ZK pri