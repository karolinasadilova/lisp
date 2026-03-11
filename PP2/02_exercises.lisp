#| polyglot-plist: (:YEAR 2025 :COURSE "PP2" :LECTURE 2 :LOADP T) |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 02_exercises.lisp -- řešení úloh z cvičení 2
;;;


;;1. co je to makro?
;specialni operator definovany uzivatelem

;Jak se vyhodnocuje makro?
;makro expanduje na nějaký výraz-pomocí expanzní funkce a ten se vyhodnotí běžným vyhodnocovacím procesem
(defun or-2-expander (a b)
  (list 'if a a b))
(defmac or-2 #'or-2-expander)
(defmacro or-2 (a b)
          (list 'if a a b))
(defmacro or-2-2 (a b)
  `(if ,a a b))

(defmacro or-2-3 (a b)
  (let  ((cond (gensym "cond")))
    `(let ((,cond ,a))
       (if ,cond ,cond ,b))))
        

(defun unless-expander (condition &rest expressions)
  (list 'if condition 
        nil 
        (cons 'progn expressions)))
(defmac unless #'unless-expander)

(defun unless-expander-2 (condition &rest expressions)
  `(when (not ,condition) ,@expressions))

(defmac unless-2 #'unless-expander-2)

(defmacro unless-2 (condition &body body)
  (let ((test (gensym "condition")))
    `(let ((,test ,condition))
    (if ,test nil (progn ,@body)))))

(defmacro unless-3 (condition &body body)
  `(when (not ,condition) (progn ,@body)))
(defun if-zero-expander (a b c)
  `(if ,(zerop a) ,b ,c))
(defmac if-zero #'if-zero-expander)

(defmacro if-zero-2 (number a b)
 (list 'if (list 'zerop number) a b))

(defmacro if-zero-3 (number x y)
  (let ((test (gensym "number")))
    `(let ((,test ,number))
       (if (zerop ,test) ,x ,y))))


;;binding (x condition)
;;
(defmacro whenb (binding &rest expressions)
    `(let ((,(car binding) ,@(cdr binding)))
         (when ,(car binding) ,@expressions)))
(defmacro reverse-progn (&rest expressions)
         `(progn ,@(reverse expressions)))
(defmacro bind-list (symbols vals &rest expressions)
          `(apply (lambda ,symbols ,@expressions) ,vals))

(defmacro whenb-2 (binding &body body)
    `(let ((,(car binding) ,(cadr binding)))
      (when ,(car binding) ,@body)))
(defmacro reverse-progn (&body body)
  `(progn ,@(reverse body)))
(defmacro my-bind-list (symbols values &body body)
  `(apply (lambda ,symbols ,@body) ,values))

;;let (() funcall (lambda (x) body) a)
  
      
        
;8:Jak víme z obecného popisu vyhodnocovacího procesu, každý atom, který
;není symbolem, se vyhodnocuje sám na sebe. Ověřte tuto skutečnost co nej-
;více způsoby pro funkce. 

;cisla a specialni symboly T NIL 