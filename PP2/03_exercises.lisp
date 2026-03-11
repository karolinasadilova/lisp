#| polyglot-plist: (:YEAR 2025 :COURSE "PP2" :LECTURE 3 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 03_exercises.lisp -- řešení úloh z cvičení 3
;;;

(defmacro test-number (number minus zero plus)
  `(if (minusp ,number)
       ,minus
     (if (zerop ,number)
         ,zero
       ,plus)))
(defmacro while (test &body body)
  `(do-while (lambda () ,test)
             (lambda () ,@body)))
(defun do-while (test-fun body-fun)
  (when (funcall test-fun)
    (funcall body-fun)
    (do-while test-fun body-fun)))
;špatně/problem zabrání symbolu:
(defmacro whenv1 (test &body body)
  `(let ((result ,test))
     (when result ,@body result)))

;dobře:
(defmacro whenv (test &body body)
  (let ((res-symbol (gensym)))
    `(let ((,res-symbol ,test))
       (when ,res-symbol
          ,@body
          ,res-symbol))))
;;ukoly:
;1.awhen
(defmacro awhen (condition &body body)
    `(let ((it ,condition))
       (when it
         ,@body
      it )))
(defmacro awhen2 (condition &body body)
  `(awhen-help ,condition 
               (lambda () ,@body)))


(defun awhen-help (condition body-fun)
  (let ((it condition))
    (when it
      (funcall body-fun)
      it)))

(defmacro my-awhen (condition &body body)
    `(let ((it ,condition))
       (when it ,@body))))
;2.setq-seq-2
(defmacro setq-seq-2 (expr sym1 sym2)
  (let ((value (gensym "value")))
    `(let ((,value ,expr))
       (setq ,sym1 ,value)
       (setq ,sym2 (+ 1 ,value))
       ,value)))
(defmacro setq-seq-2-2 (expr sym1 sym2)
  `(setq-seq-2-2-help ,expr ',sym1 ',sym2))
(defun setq-seq-2-2-help (val sym1 sym2)
  (setq sym1 val)
  (setq sym2 (+ val 1))
        val)
    
(defmacro set-seq-2-my (arg symbol1 symbol2)
  (let ((tmp (gensym "tmp")))
    `(let ((,tmp ,arg))
       (progn (setf ,symbol1 ,tmp)
         (setf ,symbol2 (1+ ,tmp))))))
        
;error-if-nil
(defmacro error-if-nil (expr)
  `(error-help ,expr ',expr))
(defun error-help (value expr)
  (if (eql value nil)
      (error "Error value of expression " expr " is NIL")
    value))


(defmacro my-error-if-nil (expr)
  (let ((value (gensym "value")))
    `(let ((,value ,expr))
       (if (null ,value)
           (error "Error expression: " ',expr "is NIL")
         ,value))))

;4.do-interval
(defmacro do-interval (binding &body body)
  `(do-interval-help ,(car (cdr binding))  ,(car (cdr (cdr binding))) (lambda (,(car binding)) ,@body)))

(defun do-interval-help (x upper body-fun)
  (if (> x upper)
      nil
    (progn 
      (funcall body-fun x)
      (do-interval-help (1+ x)  upper body-fun))))

(defmacro my-do-interval (binding &body body)
  `(my-do-interval-help ,(cadr binding)
                        ,(car (cdr (cdr binding)))
                        (lambda (,(car binding)) ,@body)))

(defun my-do-interval-help (current upper func)
  (if (> current upper) nil
    (progn (funcall func current)
      (my-do-interval-help (1+ current) upper func))))
;whenv:
(defun whenv-help (cond-val body-fun)
  (when cond-val
    (funcall body-fun)
    cond-val))
(defmacro whenv (test &body body)
  `(whenv-help ,test (lambda () ,@body)))

;whenv obsahuje test a telo, makro expanduje a zavola se expanzni funkce tj pomocna funkce whenv-help ktera dostane testovaci funkci a telo, pokud pojde test zavola pomoci funcall body-fun. jinak vrati cond-val coz bude nil v druhem pripade. 

(defmacro test-number (number minus zero plus)
  `(test-number-help ,number ,minus ,zero ,plus))
(defun test-number-help (number minus zero plus)
  (if (minusp number) minus
    (if (zerop number) zero
      plus)))
;(defmacro while (cond &body body)
;  `(when ,cond ,@body 
;     (while cond &body body)))
(defun test ()
  (let ((n 10))
    (while (> n 0)
      (print n)
      (setf n (- n 1)))))
;;while je napsané špatně, při expanzi se bude pořád expandovat a zacyklí se


(defmacro my-test-number (number minus zero plus)
  `(my-test-number-help ,number ,minus ,zero ,plus))
(defun my-test-number-help (number minus zero plus)
  (if (minusp number) minus
    (if (zerop number) zero
      plus)))
;7:
(defmacro prog1-1 (first &body body)
  `(let ((result ,first)
         (fun (lambda () ,@body)))
     (funcall fun)
     result))

(defmacro prog1-2 (first &body body)
  `(let ((result ,first)
         (bresult (progn ,@body)))
     result))

(defmacro prog1-3 (first &body body)
  `(let ((result ,first))
     ,@body
     result))

;(let ((result 100))
;  (let ((result 1))
;   (setq result 999)
;  result))

;by měl vratit 1:
;(let ((result 100))
; (prog1-3 1
;  (setq result 999)))
;999


;(let ((result 100))
;(prog1-3 1 2 3
;  (print result )))
;1
;1


;vnější result je 100 a result z makra je 1, správně by mělo makro vrátit 1 ale vrátí 999

(defmacro prog1 (first &body body)
  (let ((x (gensym "first")))
    `(let ((,x ,first))
       ,@body 
       ,x)))
(defmacro and (&rest forms)
  (cond 
   ((null forms) t)
   ((null (cdr forms)) (car forms))
   (t `(when ,(car forms) 
        (and ,@(cdr forms))))))

(defmacro my-prog1 (first &body body)
  (let ((tmp (gensym "tmp")))
   `(let ((,tmp ,first))
      ,@body
      ,tmp)))
;(defun and-helper (&optional first &rest forms)
;  (if (null first) 
;      (if (eql first nil) nil t)
;    (if first 
;        (if (null forms) first
 ;         (if (car forms) (apply and (cdr forms)))
  ;    nil))))
(defmacro my-and (&rest args)
  `(cond ((null',args) t)
       (,(car args) (my-and ,@(cdr args)))
       (t nil)))
          
          
         
(defmacro and-3 (&rest args)
 (if (null args) t
   (if (null (cdr args)) (car args)
    `(if ,(car args) (and-3 ,@(cdr args))
           nil))))
;;pri expanzi se vyhodnocuji vyrazy s carkou ale u (my-and) se vyhodnoti car nilu coz nejde a cdr nilu taky nejde takze pokud je args nil tak to vzdy spadne

;(and) vrací T"všechny výrazy jsou T"...nic to neporušuje
;(or) vrací NIL"stačí jeden T"...není nic co by bylo T
(defmacro my-or (&rest args)
  (if (null args) nil
    (if (null (cdr args)) (car args)
      `(if ,(car args) ,(car args)
        (my-or ,@(cdr args))))))

;;problem vicenasobneho vyhodnoceni

(defmacro my-or-correct (&rest args)
(let ((tmp (gensym "tmp")))
  (if (null args) nil
    (if (null (cdr args)) (car args)
      `(let ((,tmp ,(car args)))
         (if ,tmp ,tmp (my-or-correct ,@(cdr args))))))))
        
 
(defmacro or (&rest args)
  (cond ((null args) nil)
        ((null (cdr args)) (car args))
        (t (let ((s (gensym "or")))
            `(let ((,s ,(car args)))
                 (if ,s ,s (or ,@(cdr args))))))))

(defmacro or-not-full (&rest args)
  (cond ((null args) nil)
        ((null (cdr args)) `(if ,(car args) t nil))
        (t (let ((g (gensym)))
             `(let ((,g ,(car args))) 
                (if ,g t (or-not-full ,@(cdr args))))))))

(defmacro or-not-full-2 (&rest args)
  (cond ((null args) nil)
        ((null (cdr args)) `(if ,(car args) t nil))
        (t `(if ,(car args) 
                t 
              (or-not-full-2 ,@(cdr args))))))
;defun-doc:
;co ma delat:
;1. definovat funkci defunem
;2. vypsat info
;3. vratit funkci

(defmacro defun-doc (name lambda-list documentation &body body)
  `(progn (defun ,name ,lambda-list ,documentation ,@body)
     (print (list "name: " ',name))
     (print (list "lambda-list: " ',lambda-list))
     (print (list "documentation: " ',documentation))
     ',name))
(defmacro setq-seq (expr &rest symbols)
  (if (null symbols) expr
    (let ((value (gensym)))
      `(let ((,value ,expr))
         ,@(setq-seq-expander value symbols 0)
         ,value))))

            
(defun setq-seq-expander (value symbols i)
  (if (null symbols) nil
    (cons 
     `(setq ,(car symbols) (+ ,value ,i))
          (setq-seq-expander value (cdr symbols) (1+ i)))))
    
(defmacro my-setq-seq (expr &rest symbols)
(if (null symbols) nil
  (let ((value (gensym "value")))
    `(let ((,value ,expr))
      ,@(my-setq-seq-expander value symbols 0)
      ,value))))

(defun my-setq-seq-expander (value symbols index)
  (if (null symbols) nil
   (cons `(setf ,(car symbols) (+ ,index ,value))
     (my-setq-seq-expander value (cdr symbols) (1+ index)))))
;let*
;navazuje hodnotu na symbol
;;let: funcall (lambda (a) body) expr
;let* == několik vnořených letu

(defmacro let* (bindings &body body)
  (if (null bindings) `(progn ,@body)
    `(let (,(car bindings))
       (let* ,(cdr bindings) ,@body))))

;conds ((< 1 2) (print 1)) ((> 1 2) (print 2))
(defmacro all-cond (&rest conds)
  (if (null conds) nil
    `(progn (when ,(car (car conds)) ,@(cdr (car conds)))
      (all-cond ,@(cdr conds)))))


(defmacro all-cond-mapcar (&rest conds)
 `(progn
    ,@(mapcar (lambda (branch) 
                `(if ,(car branch) (progn ,@(cdr branch)) nil)) conds)))

(defmacro mylet* (bindings &body body)
  (if (null bindngs) `(progn ,@body)
    `(let ((,(car bindings)))
       (mylet* ,@(cdr bindings) ,@body))))

;;musi byt zabackquotovane progn body !!


