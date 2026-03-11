#| polyglot-plist: (:YEAR 2025 :COURSE "PP2" :LECTURE 1 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 01_exercises.lisp -- řešení úloh k cvičení 1
;;;

(defun printing-sqr (x)
  (labels ((res (sqr)
             (out "vstup funkce :" (strs x) 
                  ", výstup funkce:"(strs sqr))
             sqr))
    (res (* x x))))


(defun print-3 ()
  (out 3)
  (out 4)
  (out 8))


(defun find (elem list &key (key #'identity) (test #'eql) from-end)
  (find-if
    (lambda (e) (funcall test elem e)) list :key key :from-end from-end))


(defun find-if (predicate list &key (key #'identity) (from-end nil))
  (labels ((my-find (predicate list)
             (if (null list) nil
               (if (funcall predicate (funcall key (car list))) (car list)
               (my-find predicate (cdr list))))))
           
    (if from-end (my-find predicate (reverse list))
      (my-find predicate list))))


(defun find-if2 (pred list &key (key #'identity) (from-end nil))
  (if (eql nil (member-if pred (if from-end (reverse list) list) :key key)) nil
(car (member-if pred (if from-end (reverse list) list) :key key))))

(defun find-if3 (pred list &key (key #'identity))
  (if (null list) 
      nil
    (if (funcall pred (funcall key (car list))) 
        (car list)
      (find-if3 pred (cdr list) :key key))))
                      
(defun count-if (list pred &key (key #'identity))
  (if (null list) 0
    (if (funcall pred (funcall key (car list))) (1+ (count-if (cdr list) pred :key key))
      (count-if (cdr list) pred :key key))))


(defun count (elem list &key (key #'identity) (test #'eql))
  (if (null list) 0
    (if (funcall test elem (funcall key (car list))) (1+ (count elem (cdr list) :key key :test test))
      (count elem (cdr list) :key key :test test))))


(defun mismatch (list1 list2 &key (key #'identity) (test #'eql) from-end)
  (labels ((miss (index l1 l2)
             (if (null l1) 
                 (if (null l2) nil index)
               (if (null l2) index
                 (if (funcall test (funcall key (car l1)) (funcall key (car l2)))
                     (miss (1+ index) (cdr l1) (cdr l2))
                   index))))
           (sub-unless-nil (a b)
             (if (null b)
                 nil
               (- a b))))
  (if from-end (1- (sub-unless-nil (length list1)
                                   (miss 0 (reverse list1) (reverse list2))))
    (miss 0 list1 list2))))

(defun mismatch-2 (l1 l2 &key (key #'identity) (test #'eql) from-end)
  (labels ((iter (l1 l2 index)
             (if (eql l1 l2) nil
               (if (or (null l1) (null l2)) index
                 (if (funcall test (funcall key (car l1))
                          (funcall key (car l2)))
                     (iter (cdr l1) (cdr l2) (1+ index))
                   index))))
           (sub-unless-nil (a b)
             (if (null b) nil
               (1- (- a b)))))
    (if from-end (sub-unless-nil (length l1) (iter (reverse l1) l2 0))
      (iter l1 l2 0))))
(defun same-lists-p (list1 list2 &key (key #'identity) (test #'eql))
  (if (eql nil (mismatch list1 list2 :key key :test test)) t nil))


(defun intersection (list1 list2 &key (key #'identity) (test #'eql))
  (labels ((main ()
             (if (null list1) 
                 nil
               (if (null list2) 
                   nil 
                 (if (find (car list1) list2 :key key :test test)
                     (cons (car list1) (intersection (cdr list1) list2))
                   (intersection (cdr list1) list2 :key key :test test)
                   )))))
    (remove-duplicates (main))))

  ;neošetřuje duplicity:     
(defun intersection2 (l1 l2 &key (key #'identity) (test #'eql))
 (labels ((main ()
            (if (or (null l1) (null l2)) nil
              (if (find (car l1) l2 :key key :test test)
                  (cons (car l1) (intersection2 (cdr l1) l2 :key key :test test))
                (intersection2 (cdr l1) l2 :key key :test test)))))
   (remove-duplicates2 (main))))
                  
(defun remove-duplicates (list &key (key #'identity) (test #'eql) from-end)
  (labels ((my-remove (list1 list2)
            (if (null list1) (reverse list2)
              (if (eql nil (find (car list1) (cdr list1) :key key :test test :from-end from-end))
                  (my-remove (cdr list1) (cons (car list1) list2))
                (my-remove (cdr list1) list2))))

          (my-remove-end (list1 list2)
            (if (null list1) (reverse list2)
              (if (find (car list1) (cdr list1) :key key :test test :from-end from-end)
                  (my-remove-end (remove (car list1) list1 :key key :test test)  (cons (car list1) list2))
                (my-remove-end (cdr list1) (cons (car list1) list2))))))

   (if from-end
       (my-remove-end list nil)
     (my-remove list nil)))) 


(defun remove-duplicates2 (list &key (key #'identity) (test #'eql))
  (labels ((iter (list newlist)
             (if (null list) (reverse newlist)
               (if (find (car list) (cdr list) :key key :test test)
                   (iter (cdr list) newlist)
                 (iter (cdr list) (cons (car list) newlist))))))
  (iter list nil)))


 