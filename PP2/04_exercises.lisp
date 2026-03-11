#| polyglot-plist: (:YEAR 2025 :COURSE "PP2" :LECTURE 4 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 04_exercises.lisp -- řešení úloh z cvičení 4
;;;
;;priklady z konzultace:
(defun power-set (s n p)
  (if (>= n (length s)) 
      (list p)
    (append (power-set s (1+ n) (append p (list (nth n s))))
            (power-set s (1+ n) p))))
(defun power-set2 (set)
  (if (null set) (list nil)
    (let ((subsets (power-set2 (cdr set))))
      (append subsets
              (add-element (car set) subsets)))))

(defun add-element (element list)
  (if (null list) nil
    (cons (cons element (car list))
          (add-element element (cdr list)))))
;př notes:

(defmacro my-setf-2 (place value)
  `(funcall (place-accessors ,place) ,value))

(defmacro my-setf (place value &rest rest-of-places-and-values)
  (if (null rest-of-places-and-values) 
      `(my-setf-2 ,place ,value)
    `(progn (my-setf-2 ,place ,value)
       (my-setf ,@rest-of-places-and-values))))


;priklady ze cvičení:
;1:
;(let ((x 1))
;(let ((x x))
;(setf x 2)
;(print x))
;x)


;2
;1


;
;x se vrací z prvního prostředí , printuje v prostředí letu

(defmacro swap (place1 place2)
  (let ((p1 (gensym "place1"))
        (p2 (gensym "place2"))
        (tmp (gensym "tmp")))
    `(let ((,p1 (place-accessors ,place1))
           (,p2 (place-accessors ,place2)))
       (bind-list (sel1 mut1) ,p1
       (bind-list (sel2 mut2) ,p2
       (let ((,tmp (funcall sel2)))
         (funcall mut1 (funcall sel2))
         (funcall mut2 ,tmp)))))))


;;swap
(defmacro swap (place1 place2)
  `(bind-list (sel1 mut1) (place-accessors ,place1)
              (bind-list (sel2 mut2) (place-accessors ,place2)
              (let ((tmp (funcall sel2)))
                (funcall mut2 (funcall sel1))
                (funcall mut1 tmp)))))
                    
                         
  






;3:
;(defmacro swap (a b)
;  (let ((place-sym (gensym "place")))
;    `(let* ((,place-sym ,a))
;       (setf ,a ,b
;             ,b ,place-sym))))

;bude místa vyhodnocovat několikrát např. dlouhý seznam a poslední prvek


(let ((x 0))
  (defun f ()
    (lambda () (incf x))))

(defun test1 (a)
  (setf a 2))
(defun test2 (a)
  (test1 a)
  a)

(defun test3 (a)
  (setf (car a) 2))
(defun test4 (a)
  (test3 a)
  a)

(defun last-element (list)
  (cond ((null list) nil)
        ((null (cdr list)) list)
        (t (last-element (cdr list)))))

(defmacro last-element-accessors (list-expr)
  (let ((last (gensym "last")))
    (let ((l (gensym "list")))
   `(let ((,l ,list-expr))
      (let ((,last (last-element ,l)))
        (list (lambda () (car ,last))
              (lambda (value) (set-car ,last value) value)))))))

(defun gen-oddp-squares ()
  (gen-map #'power2 (gen-filter #'oddp (gen-naturals))))


(defun gen-second (gen)
  (lambda ()
    (let ((value (next gen)))
      (next gen)
      value)))

(defun gen+ (gen1 gen2)
  (lambda ()
    (+ (next gen1) (next gen2))))

(defun circlist (&rest args)
    (let ((newlist (copy-list args)))
      (if (null newlist) nil
        (progn (set-cdr (last-element newlist) newlist)
          newlist))))


; (let ((pair (cons 1 nil)))
;  (set-cdr pair pair) pair)
;#1=(1 . #1#)


(defun circ-find (element list)
  (cond ((null list) nil)
        ((eql (car list) element) (car list))
        ((not (circlep list)) (find element list))
        (t (labels ((iter (slow fast)
                      (cond ((eql element (car slow)) (car slow))
                            ((eql slow fast) nil)
                            (t (iter (cdr slow) (cddr fast))))))
             (iter (cdr list) (cddr list))))))

(defun struct-find (element struct)
    (let ((visited '()))
      (labels ((iter (node)
                 (cond ((atomp node) (if (eql node element) node nil))
                       ((find node visited) nil )
                       (t (progn (push node visited) (or (iter (car node)) (iter (cdr node))))))))
        (iter struct))))

(defun struct-sum (struct)
  (let ((visited '()))
    (labels((iter  (node)
            (cond ((atomp node) (if (numberp node) node 0))
                  ((not (find node  visited)) (progn (push node visited) (+ (iter (car node)) (iter (cdr node)))))
                  (t 0))))
      (iter struct))))
                       
(defun circle-struct-p (struct)
  (let ((visited '()))
    (labels ((iter (node)
               (cond ((atomp node) nil)
                     ((find node visited) t)
                     (t (progn (push node visited)
                          (or (iter (car node)) (iter (cdr node))))))))
      (iter struct))))


(defun struct-copy (struct)
  (let ((visited '()))
    (labels ((iter (node)
               (cond ((null node) nil)
                     ((find node visited) nil)
                     (t (cons node ( (iter (car node))
                                       (iter (cdr node))))))))
      (iter struct))))

