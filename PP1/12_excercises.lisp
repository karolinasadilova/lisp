#| polyglot-plist: (:YEAR 2025 :COURSE "PP1" :LECTURE 12 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 12_excercises.lisp -- řešení úloh k cvičení 12
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

;; Testy interpretu Scheme
;; Testy jsou zakomentované, vyhodnocujte je jeden po druhém.

#|

(ss-eval 1)
(ss-eval '(if 1 2 3))
(ss-eval 'zero)
(ss-eval 'pi)

(ss-eval 'x)

(ss-eval ''x)

(ss-eval '(let ((x 1)
                (y 2))
            (+ x y)))

(ss-eval '(let ((1+ (lambda (x)
                      (+ x 1))))
            (1+ 5)))

(ss-eval '(let ((fact (lambda (n)
                        (let ((fct (lambda (f n)
                                     (if (= n 0)
                                         1
                                       (* n (f f (- n 1)))))))
                          (fct fct n)))))
            (fact 5)))
                         
|#