#| polyglot-plist: (:YEAR 2025 :COURSE "PP2" :LECTURE 5 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 05_exercises.lisp -- řešení úloh z cvičení 5
;;;
(let ((count 0))
  (defun counting-fib (n)
  (setf count (1+ count))
  (if (<= n 1) 1
    (+ (counting-fib (1- n))
       (counting-fib (- n 2)))))
    

(defun fib-count ()
  count))

(defmacro defcfun (function l-list &body body)
  (let ((count (gensym "count")))
    `(let ((,count 0))
       (defun ,function ,l-list  
         (setf ,count (1+ ,count))
         ,@body)
       (defun ,(symbol (stra function "-cc")) ()
         ,count))))

(defun 0and1 ()
  (cons-stream 0
               (cons-stream 1
                            (0and1))))
      
;verze1:
(defun stream-ref (stream index)
  (cond ((null stream) nil)
        ((zerop index) (stream-car stream))
        (t (stream-ref (stream-cdr stream) (1- index)))))
;verze2:
(defun stream-ref (stream index)
  (when stream
        (if (zerop index)
            (stream-car stream)
          (stream-ref (stream-cdr stream) (1- index)))))


(defun stream-heads (stream)
  (let ((car (stream-car stream)))   
    (labels ((iter (car acc stream)
              (when stream
                (setf acc (+ acc car))
                (cons-stream acc (iter (stream-car (stream-cdr stream)) acc (stream-cdr stream))))))
      (iter car 0 stream))))

(defun stream-append2 (stream1 stream2)
  (if (null stream1) 
      stream2
    (cons-stream (stream-car stream1)
                    (stream-append2 (stream-cdr stream1) stream2))))


#|
(defun stream-append (stream &rest streams)
  (foldr #'stream-append2 streams stream))
|#


(defun stream-append-1 (stream &rest streams)
  (if (null streams) stream
    (stream-append2 stream 
                    (apply #'stream-append-1 streams))))
                      

(defmacro stream-append (stream &rest streams)
  `(if (null ,streams) ,stream
     (stream-append2 ,stream
                     (stream-append ,@streams))))

(defun prime-twins ()
  (labels ((iter (stream)
             (let ((car (stream-car stream))
                   (cdr (stream-cdr stream)))
               (cond ((= (- (car cdr) car) 2) 
                      (cons-stream (cons car (car cdr)) (iter cdr)))
                     (t (iter cdr))))))
    (iter (eratosthenes))))
(defun power3 (element)
  (* element element element))

(defun newstream (stream)
  (let ((stream1 (stream-each-other stream)))
    (let ((stream2 (stream-mapcar-1 #'power3 stream1)))
      (stream-heads stream2))))

(defun naturals-frm-1 ()
  (stream-cdr (naturals)))
(defun fib-list (n)
  (labels ((iter  (stream index)
             (let ((current (stream-car stream)))
             (if (= n index) nil
                   (cons current (iter (stream-cdr stream) (1+ index)))))))
    (iter (fib2) 0)))

(defun fib-gen ()
  (let ((stream (fib2)))
    (lambda () 
      (let ((current (stream-car stream)))
        (setf stream (stream-cdr stream))
        current))))


          


#|(defun fib2 () 
  (let ((result (cons-stream
                 0
                 (cons-stream
                  1
                  (stream-mapcar-2 #'+
                                   16
                                   result
                                   (stream-cdr result))))))))

;syntakticky špatně:chybí tělo letu
;se setf to funguje protože si let vytvoří result 
;resultu pak přiřadí stream který už existuje
;v letu se nejřív vyhodnotí všechno napravo a pak se to naváže nalevo ale v ten moment ještě result neexistuje 

|#
(defun next-row-for-pascal (row)
  (labels ((iter (row)
             (cond ((null (stream-cdr row)) (cons-stream 1 nil))
                   (t (cons-stream (+ (stream-car row) (stream-car (stream-cdr row)))
                                (iter (stream-cdr row))))))))
  (cons-stream 1 (iter row)))
(defun pascal-stream ()
  (labels ((iter (row)
             (cons-stream row
                          (iter (nex-row-for-pascal row)))))
    (iter (cons-stream 1 nil))))