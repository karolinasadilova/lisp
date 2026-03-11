#| polyglot-plist: (:YEAR 2025 :COURSE "PP1" :LECTURE 9 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 09_excercises.lisp -- řešení úloh k cvičení 9
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

(defun multiplier (factor)
  (labels ((prod (x) (* x factor)))
    #'prod))

#|
;; Definice pomocí anonymní funkce:

(defun multiplier (factor)
  (lambda (x) (+ x factor)))
|#


(defun balances (deposits interest-rate) 
  (lambda (month)
    (if (= month 0)
        (mem deposits 0)
      (+ (* (mem (balances deposits interest-rate) (- month 1))
            (+ interest-rate 1))
         (mem deposits month)))))
(defun multi (factor list)
  (labels ((iter (x)
             (* x factor)))
  (mapcar #'iter list)))
(defun my-multi (factor list)
  (mapcar (lambda (x) (* x factor)) list))
(defun my-seq-squares (seq)
  (lambda (n) (power2 (mem seq n))))
(defun my-sum-seq (seq from to)
  (if (> from to) 0
    (+ (mem seq from) (my-sum-seq seq (+ 1 from) to))))
(defun my-seq-to-list (seq n)
  (labels ((st (i)
             (if (>= i n) '()
               (cons (mem seq i) (st (+ i 1))))))
    (st 0)))
(defun const-seq (c)
  (lambda (n) c))
(defun my-sum-2-seq (seq1 seq2)
  (lambda (n) (+ (mem seq1 n) (mem seq2 n))))
(defun seq-power2+1 (seq)
  (lambda (n) (+ 1 (power2 n))))
(defun my-first-index (seq condition)
  (labels ((iter (i)
            (if (funcall condition (mem seq i)) i
                         (iter (+ 1 i)))))
    (iter 0)))
;(let((x a)) telo)
; (funcall (lambda (x) telo) a)
(let ((f (lambda (fun n) 
           (if (= n 0) 
               1
             (* n (funcall fun fun (- n 1)))))))
  (funcall f f 3))
(defun constant-seq-p (seq k)
  (labels ((iter (i)
             (cond ((= i (- k 1)) t)
                   ((eql (mem seq i) (mem seq (+ 1 i))) (iter (+ 1 i)))
                   (t nil))))
    (iter 0)))
(defun increasing-seq-p (seq k)
  (labels ((iter (i)
             (cond ((= i (- k 1)) t)
                   ((< (mem seq i) (mem seq (+ i 1))) (iter (+ i 1)))
                   (t nil))))
    (iter 0)))
(defun from-nth (seq i)
  (lambda (n) (+ n i)))
(defun even-numbers (seq)
  (lambda (n) (* 2 n)))
(defun zero-row-p (tbl row)
  (labels ((iter (col)
        (cond ((> col 9) t)
              ((= (funcall tbl row col) 0) (iter (+ 1 col)))
              (t nil))))
    (iter 0)))
(defun transpose-table (tbl)
  (lambda (row column) (funcall tbl column row)))