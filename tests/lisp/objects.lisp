;;; objects.lisp — tests for M5: &key, classes, generic functions, mixins.

;;; &key
(defun kf (a &key (b (* a 2)) c) (list a b c))
(check (kf 1) '(1 2 nil))
(check (kf 1 :c 3) '(1 2 3))
(check (kf 1 :c 3 :b 5) '(1 5 3))
(defun kr (&rest all &key x) (list x all))
(check (kr :x 1 :y 2) '(1 (:x 1 :y 2)))
(check (funcall (lambda (&optional (o 4) &key k) (list o k)) 1 :k 2) '(1 2))

;;; classes and instances
(defclass point ()
  ((x :initarg :x :initform 0 :accessor point-x)
   (y :initarg :y :initform 0 :accessor point-y)))
(defvar p1 (make-instance 'point :x 3))
(check (point-x p1) 3)
(check (point-y p1) 0)
(setf (point-y p1) 7)
(check (point-y p1) 7)
(check (slot-value p1 'x) 3)
(setf (slot-value p1 'x) 4)
(check (point-x p1) 4)
(check (prin1-to-string p1) "#<POINT>")
(check (prin1-to-string (find-class 'point)) "#<Class POINT>")
(check (typep p1 'point) t)
(check (typep p1 'object) t)
(check (typep 3 'point) nil)
(check (class-name (class-of 3)) 'integer)
(check (class-name (class-of nil)) 'null)
(check (class-name (class-of "s")) 'string)

;;; initforms are evaluated for each instance
(defvar *made* 0)
(defclass counted () ((serial :initform (setq *made* (1+ *made*)) :reader serial)))
(check (list (serial (make-instance 'counted)) (serial (make-instance 'counted))) '(1 2))

;;; inheritance, mixins, the class precedence list
(defclass colored-mixin () ((color :initarg :color :initform 'black :accessor color)))
(defclass colored-point (colored-mixin point) ())
(defvar cp (make-instance 'colored-point :x 1 :y 2 :color 'red))
(check (list (point-x cp) (color cp)) '(1 red))
(check (mapcar #'class-name (class-precedence-list (find-class 'colored-point)))
       '(colored-point colored-mixin point object t))
(defclass a () ()) (defclass b (a) ()) (defclass c (a) ()) (defclass d (b c) ())
(check (mapcar #'class-name (class-precedence-list (find-class 'd))) '(d b c a object t))

;;; generic functions and method combination
(defvar *trace* nil)
(defgeneric show (thing))
(defmethod show ((p point)) (push 'point *trace*) (list (point-x p) (point-y p)))
(defmethod show ((p colored-point)) (push 'colored-point *trace*) (cons (color p) (call-next-method)))
(defmethod show :before ((p colored-mixin)) (push 'before-mixin *trace*))
(defmethod show :before ((p colored-point)) (push 'before-point *trace*))
(defmethod show :after ((p point)) (push 'after-point *trace*))
(defmethod show :after ((p colored-point)) (push 'after-colored *trace*))
(defmethod show :around ((p colored-point)) (push 'around *trace*) (list :wrapped (call-next-method)))
(setq *trace* nil)
(check (show cp) '(:wrapped (red 1 2)))
(check (reverse *trace*)
       '(around before-point before-mixin colored-point point after-point after-colored))
(setq *trace* nil)
(check (show p1) '(4 7))
(check (reverse *trace*) '(point after-point))

(defmethod show ((n integer)) (list :number n))
(defmethod show ((s string)) (list :string s))
(defmethod show ((x t)) (list :anything x))
(check (show 42) '(:number 42))
(check (show "hi") '(:string "hi"))
(check (show 'sym) '(:anything sym))
(defmethod next-p ((x t)) (next-method-p))
(check (next-p 1) nil)

;;; redefining a method replaces it
(defmethod show ((n integer)) (list :int n))
(check (show 1) '(:int 1))

;;; initialize-instance
(defclass greeter () ((greeting :initform nil :accessor greeting)))
(defmethod initialize-instance :after ((g greeter) &key) (setf (greeting g) "hello"))
(check (greeting (make-instance 'greeter)) "hello")

;;; errors
(check (handler-case (slot-value p1 'nope) (error (c) :no-slot)) :no-slot)
(check (handler-case (make-instance 'no-such-class) (error (c) :no-class)) :no-class)
(defgeneric only-points (x))
(defmethod only-points ((p point)) t)
(check (handler-case (only-points 5) (error (c) :no-method)) :no-method)

;;; locks work outside processes too
(defvar l1 (make-lock "test"))
(check (with-lock (l1) (lock-owner l1)) :no-process)
(check (lock-owner l1) nil)

(report "objects")
