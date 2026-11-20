;;; -*- Mode: Lisp -*-
;;; objects.lisp — the object system: CLOS as Common Lisp has it, built in the
;;; spirit of Flavors (M5; multiple dispatch and the rest in M17).
;;;
;;;   (defclass name (superclass...) (slot...) option...)
;;;       slot: name, or (name :initform form :initarg :key :accessor f
;;;                       :reader f :writer f :allocation :class :documentation s)
;;;       options: (:default-initargs :key form ...), (:documentation s)
;;;   (make-instance 'name :key value ...)
;;;   (defgeneric name (args...) (:documentation s) (:method ...)...)
;;;   (defmethod name [:before | :after | :around] ((arg class) (y (eql 3)) z) body)
;;;   (call-next-method), (next-method-p), slot-value, with-slots, change-class
;;;
;;; Classes have multiple superclasses: a mixin is just a class meant to be
;;; combined with others. The class precedence list is depth-first, left to
;;; right, with a class shared by several superclasses placed after all of
;;; them (as in Flavors).
;;;
;;; A method has a specializer for each required argument: a class, or
;;; (EQL value). The applicable methods are ordered by their specializers,
;;; the first argument's first, as in CLOS. Standard method combination:
;;; :around methods (most specific first), then :before methods (most
;;; specific first), the primary methods (most specific first, chained with
;;; CALL-NEXT-METHOD), then :after methods (most specific last).

;;; ---- classes ------------------------------------------------------------

;; #(:class name direct-superclasses direct-slots cpl slot-names slot-specs
;;   local-slot-names class-slots)
;;   slot-specs: ((name initform-function initarg allocation) ...), in slot order
;;   local-slot-names: the slots an instance holds, the layout of its slot vector
;;   class-slots: ((name . cell) ...) for :ALLOCATION :CLASS, the value in (car cell)
(defvar *classes* nil)

(defun class-name (c) (aref c 1))
(defun class-direct-superclasses (c) (aref c 2))
(defun class-precedence-list (c) (aref c 4))
(defun class-slot-names (c) (aref c 5))
(defun classp (x) (and (vectorp x) (= (length x) 9) (eq (aref x 0) :class)))

(defvar *unbound-slot* (make-symbol "UNBOUND"))   ; what an unbound slot holds

(defun find-class (name &optional (errorp t))
  (let ((c (cdr (assq name *classes*))))
    (if (or c (not errorp)) c (error "There is no class named" name))))

;; depth-first, left to right; keep the LAST occurrence of a repeated class,
;; so that a shared superclass comes after everything that inherits from it
(defun compute-cpl (class supers)
  (let ((all (cons class nil)))
    (dolist (s supers) (setq all (append all (class-precedence-list s))))
    (let ((seen nil) (result nil))
      (dolist (c (reverse all))
        (unless (memq c seen)
          (push c seen)
          (push c result)))
      result)))

;; the precedence list and the slots of class C, from its direct parts
(defun %finalize-class (c)
  (setf (aref c 4) (compute-cpl c (aref c 2)))
  ;; effective slots: from the least specific class to the most specific;
  ;; a more specific class may give a slot a new initform, initarg or allocation
  (let ((effective nil))
    (dolist (k (reverse (class-precedence-list c)))
      (dolist (s (aref k 3))
        (let ((old (assq (car s) effective)))
          (if old
              (progn (when (cadr s) (setf (cadr old) (cadr s)))
                     (when (caddr s) (setf (car (cddr old)) (caddr s)))
                     (setf (car (cdddr old)) (cadddr s)))
              (setq effective (append effective (list (list (car s) (cadr s) (caddr s) (cadddr s)))))))))
    (setf (aref c 6) effective)
    (let ((names (mapcar #'car effective))
          (local nil) (class-slots nil))
      (dolist (s effective)
        (if (eq (cadddr s) :class)
            ;; the cell belongs to the most specific class that defines the slot
            (let* ((definer (find-if (lambda (k) (assq (car s) (aref k 3))) (class-precedence-list c)))
                   (cell (if (eq definer c)
                             (or (cdr (assq (car s) (aref c 8)))
                                 (list (if (cadr s) (funcall (cadr s)) *unbound-slot*)))
                             (cdr (assq (car s) (aref definer 8))))))
              (push (cons (car s) cell) class-slots))
            (push (car s) local)))
      ;; the same names keep the same list, so that instances stay current
      (unless (equal names (aref c 5)) (setf (aref c 5) names))
      (setq local (nreverse local))
      (unless (equal local (aref c 7)) (setf (aref c 7) local))
      (setf (aref c 8) (nreverse class-slots))))
  c)

;; Defining a class again changes the class object in place, so that its
;; methods and instances stay with it; its subclasses are worked out again,
;; and each instance takes on the new slots the next time it is used.
(defun %define-class (name super-names slot-specs)
  (let* ((supers (mapcar #'find-class
                         (or super-names (if (memq name '(t)) nil (if (eq name 'object) '(t) '(object))))))
         (old (find-class name nil))
         (class (or old (vector :class name nil nil nil nil nil nil nil))))
    (setf (aref class 2) supers)
    (setf (aref class 3) (mapcar (lambda (s) (list (car s) (cadr s) (caddr s) (cadddr s))) slot-specs))
    (%finalize-class class)
    (if old
        (dolist (e (reverse *classes*))                 ; oldest first: superclasses before subclasses
          (let ((k (cdr e)))
            (when (and (not (eq k class)) (memq class (class-precedence-list k)))
              (%finalize-class k))))
        (push (cons name class) *classes*))
    (setq *dispatch-generation* (1+ *dispatch-generation*))   ; empty every method cache
    class))

(defvar *dispatch-generation* 0)

;; the built-in classes, for methods on numbers, strings, lists...
(%define-class 't nil nil)
(%define-class 'object '(t) nil)
(%define-class 'number '(t) nil)
(%define-class 'real '(number) nil)
(%define-class 'rational '(real) nil)
(%define-class 'integer '(rational) nil)
(%define-class 'ratio '(rational) nil)
(%define-class 'float '(real) nil)
(%define-class 'symbol '(t) nil)
(%define-class 'list '(t) nil)
(%define-class 'null '(symbol list) nil)
(%define-class 'cons '(list) nil)
(%define-class 'vector '(t) nil)
(%define-class 'string '(vector) nil)
(%define-class 'character '(t) nil)
(%define-class 'function '(t) nil)
(%define-class 'condition '(t) nil)
;; system objects that are tagged vectors, like #(:class ...) and #(:process ...)
(%define-class 'class '(t) nil)
(%define-class 'generic-function '(function) nil)
(%define-class 'method '(t) nil)
(%define-class 'process '(t) nil)
(%define-class 'stream '(t) nil)
(%define-class 'bitmap '(t) nil)
(%define-class 'font '(t) nil)
(%define-class 'lock '(t) nil)

(defvar *tag-classes*
  '((:class . class) (:generic . generic-function) (:method . method) (:process . process)
    (:stream . stream) (:bitmap . bitmap) (:font . font) (:lock . lock)))

;;; ---- instances ------------------------------------------------------------

;; #(:instance class slot-vector layout): LAYOUT is the class's local slot
;; names when the slot vector was made. When the class is defined again, the
;; layout no longer matches and the instance is brought up to date: its slot
;; vector is made again, the values carried over by name.
(defun instancep (x)
  (and (vectorp x) (= (length x) 4) (eq (aref x 0) :instance)))

(defun instance-slots (x) (aref x 2))

(defun class-of (x)
  (cond ((instancep x) (aref x 1))
        ((null x) (find-class 'null))
        ((integerp x) (find-class 'integer))
        ((rationalp x) (find-class 'ratio))
        ((floatp x) (find-class 'float))
        ((symbolp x) (find-class 'symbol))
        ((consp x) (find-class 'cons))
        ((stringp x) (find-class 'string))
        ((characterp x) (find-class 'character))
        ((functionp x) (find-class 'function))
        ((conditionp x) (%kernel-condition-class x))
        ((and (vectorp x) (> (length x) 0) (assq (aref x 0) *tag-classes*))
         (find-class (cdr (assq (aref x 0) *tag-classes*))))
        ((vectorp x) (find-class 'vector))
        (t (find-class 't))))

;; the class of a kernel condition; signals.lisp knows better
(defun %kernel-condition-class (c) (find-class 'condition))

(defun typep (x class-name)
  (if (memq (find-class class-name) (class-precedence-list (class-of x))) t nil))

;; X with the slots its class has now
(defun %instance-current (x)
  (let ((c (aref x 1)))
    (unless (eq (aref x 3) (aref c 7))
      (let* ((old-names (aref x 3)) (old (aref x 2)) (names (aref c 7))
             (new (make-vector (length names) *unbound-slot*)) (added nil) (discarded nil) (i 0))
        (dolist (n names)
          (let ((p (%position n old-names)))
            (if p (setf (aref new i) (aref old p)) (push n added)))
          (setq i (1+ i)))
        (let ((k 0))
          (dolist (n old-names)
            (unless (memq n names) (push n discarded) (push (aref old k) discarded))
            (setq k (1+ k))))
        (setf (aref x 2) new)
        (setf (aref x 3) names)
        (update-instance-for-redefined-class x (nreverse added) nil (nreverse discarded))))
    x))

;; where slot NAME of INSTANCE is: an index into its slot vector, the cell of a
;; class slot, or NIL
(defun %slot-location (instance name)
  (%instance-current instance)
  (or (%position name (aref instance 3))
      (cdr (assq name (aref (aref instance 1) 8)))))

(defun slot-exists-p (instance name)
  (and (instancep instance) (%slot-location instance name) t))

;; the value, or *UNBOUND-SLOT*
(defun %slot-value (instance name)
  (let ((loc (and (instancep instance) (%slot-location instance name))))
    (cond ((integerp loc) (aref (aref instance 2) loc))
          ((consp loc) (car loc))
          (t (slot-missing (if (instancep instance) (class-of instance) nil) instance name 'slot-value)))))

(defun slot-value (instance name)
  (let ((v (%slot-value instance name)))
    (if (eq v *unbound-slot*) (slot-unbound (class-of instance) instance name) v)))

(defun set-slot-value (instance name value)
  (let ((loc (and (instancep instance) (%slot-location instance name))))
    (cond ((integerp loc) (setf (aref (aref instance 2) loc) value))
          ((consp loc) (setf (car loc) value))
          (t (slot-missing (if (instancep instance) (class-of instance) nil) instance name 'setf value)))))

;; NIL also when there is no such slot (Common Lisp calls SLOT-MISSING)
(defun slot-boundp (instance name)
  (and (instancep instance) (%slot-location instance name)
       (not (eq (%slot-value instance name) *unbound-slot*))))

(defun slot-makunbound (instance name)
  (set-slot-value instance name *unbound-slot*)
  instance)

;; the index of slot NAME in INSTANCE's slot vector (for STORE-CONDITIONAL)
(defun slot-index (instance name)
  (let ((loc (and (instancep instance) (%slot-location instance name))))
    (if (integerp loc) loc (error "No slot named" name "in" instance))))

(defun %copy-instance (x)
  (%instance-current x)
  (vector :instance (aref x 1) (copy-seq (aref x 2)) (aref x 3)))

(defun allocate-instance (class &rest initargs)
  (vector :instance class (make-vector (length (aref class 7)) *unbound-slot*) (aref class 7)))

;; the initargs given, then the class's :DEFAULT-INITARGS for any not given
(defun %default-initargs (class initargs)
  (let ((more nil))
    (dolist (k (class-precedence-list class))
      (let ((d (get (class-name k) 'default-initargs)))
        (while d
          (when (and (eq (%getf-key initargs (car d)) '%none) (eq (%getf-key more (car d)) '%none))
            (setq more (append more (list (car d) (funcall (cadr d))))))
          (setq d (cddr d)))))
    (if more (append initargs more) initargs)))

(defun make-instance (class &rest initargs)
  (let* ((c (if (symbolp class) (find-class class) class))
         (initargs (%default-initargs c initargs))
         (instance (allocate-instance c)))
    (apply #'initialize-instance instance initargs)
    instance))

;; give the slots their values: from the initargs, else, for the slots in
;; SLOT-NAMES (T: all) that are unbound, from their initforms
(defun %shared-initialize (instance slot-names initargs)
  (%instance-current instance)
  (dolist (s (aref (aref instance 1) 6))
    (let* ((name (car s)) (initarg (caddr s))
           (given (if initarg (%getf-key initargs initarg) '%none)))
      (cond ((not (eq given '%none)) (set-slot-value instance name given))
            ((and (cadr s) (not (eq (cadddr s) :class))
                  (or (eq slot-names t) (memq name slot-names))
                  (eq (%slot-value instance name) *unbound-slot*))
             (set-slot-value instance name (funcall (cadr s)))))))
  instance)

;;; ---- SETF for accessors and SLOT-VALUE -------------------------------------
;;;
;;; (putprop 'reader 'writer 'setf-function) makes (setf (reader x) v) mean
;;; (writer x v). Everything else goes to the SETF of boot.lisp.

(defvar *basic-setf* (macro-function 'setf))

(defmacro setf (place value)
  (cond ((and (consp place) (symbolp (car place)) (get (car place) 'setf-function))
         `(,(get (car place) 'setf-function) ,@(cdr place) ,value))
        ((and (consp place) (symbolp (car place)) (macro-function (car place)))
         `(setf ,(apply (macro-function (car place)) (cdr place)) ,value))   ; expand the place
        (t (funcall *basic-setf* place value))))

(putprop 'slot-value 'set-slot-value 'setf-function)

;;; ---- generic functions -------------------------------------------------------

;; #(:generic name methods cache cache-generation arity eql-p)
;;   method: #(:method qualifier class function simple source specializers)
;;   ARITY: how many leading arguments some method specializes; EQL-P: whether
;;   any method has an (EQL x) specializer (then nothing is cached)
;; CLASS is the first argument's specializer (T for an EQL one). The generic
;; function and each method function carry names, (METHOD name qualifier
;; class), so that the debugger can say what failed.
(defun ensure-generic (name)
  (or (get name 'generic)
      (let ((gf (vector :generic name nil nil 0 0 nil)))
        (putprop name gf 'generic)
        (let ((f (lambda (&rest args) (%dispatch gf args))))
          (%set-function-name f name)
          (fset name f))
        gf)))

(defun ensure-generic-function (name &rest options)
  (ensure-generic name)
  (symbol-function name))

;; the generic function object for a name, the function itself, or the object
(defun %generic (x)
  (cond ((and (vectorp x) (> (length x) 0) (eq (aref x 0) :generic)) x)
        ((symbolp x) (get x 'generic))
        ((functionp x) (let ((n (%function-name x))) (and (symbolp n) n (get n 'generic))))
        (t nil)))

(defun find-method-function (name qualifier class-name)
  (let ((gf (get name 'generic)) (class (find-class class-name nil)))
    (when gf
      (dolist (m (aref gf 2))
        (when (and (eq (aref m 1) qualifier) (eq (aref m 2) class)) (return (aref m 3)))))))

(defun find-method-source (name qualifier class-name)
  (let ((gf (get name 'generic)) (class (find-class class-name nil)))
    (when gf
      (dolist (m (aref gf 2))
        (when (and (eq (aref m 1) qualifier) (eq (aref m 2) class)) (return (aref m 5)))))))

(defun generic-methods (name)
  (let ((gf (get name 'generic))) (and gf (aref gf 2))))

(defun method-qualifiers (m) (if (aref m 1) (list (aref m 1)) nil))
(defun method-specializers (m) (aref m 6))
(defun method-function (m) (aref m 3))

(defun %eql-specializer-p (s) (and (consp s) (eq (car s) 'eql)))

(defun %same-specializers (a b)
  (and (= (length a) (length b))
       (let ((same t))
         (while (and same a)
           (let ((x (car a)) (y (car b)))
             (setq same (if (%eql-specializer-p x) (and (%eql-specializer-p y) (eql (cadr x) (cadr y))) (eq x y))))
           (setq a (cdr a) b (cdr b)))
         same)))

(defun %all-eq (a b)
  (while (and a (eq (car a) (car b))) (setq a (cdr a) b (cdr b)))
  (null a))

;; arity and eql-p of GF, from its methods
(defun %generic-shape (gf)
  (let ((arity 0) (eql-p nil))
    (dolist (m (aref gf 2))
      (let ((k 0) (last 0))
        (dolist (s (aref m 6))
          (setq k (1+ k))
          (when (%eql-specializer-p s) (setq eql-p t))
          (unless (eq s (find-class 't)) (setq last k)))
        (setq arity (max arity last))))
    (setf (aref gf 5) arity)
    (setf (aref gf 6) eql-p)
    (setf (aref gf 3) nil)))

;; SIMPLE: the method never calls CALL-NEXT-METHOD or NEXT-METHOD-P.
;; SPECIALIZERS: a class or (EQL value) for each required argument; without
;; them the method specializes its first argument on CLASS-NAME.
(defun %add-method (name qualifier class-name function &optional simple source specializers)
  (let* ((gf (ensure-generic name))
         (specs (or specializers (list (find-class class-name))))
         (first (car specs))
         (class (if (%eql-specializer-p first) (find-class 't) first))
         (methods nil))
    (when (compiled-function-p function)
      (%set-function-name function (list 'method name qualifier (class-name class))))
    (dolist (m (aref gf 2))                 ; replace a method with the same role
      (unless (and (eq (aref m 1) qualifier) (%same-specializers (aref m 6) specs))
        (push m methods)))
    (push (vector :method qualifier class function simple source specs) methods)
    (setf (aref gf 2) methods)
    (%generic-shape gf)
    name))

(defun find-method (generic qualifiers specializers &optional (errorp t))
  (let* ((gf (%generic generic))
         (specs (mapcar (lambda (s) (if (symbolp s) (find-class s) s)) specializers))
         (q (car qualifiers))
         (found (and gf (find-if (lambda (m)
                                   (and (eq (aref m 1) q)
                                        (%same-specializers (%pad-specializers (aref m 6) (length specs))
                                                            (%pad-specializers specs (length (aref m 6))))))
                                 (aref gf 2)))))
    (or found (and errorp (error "No such method of" generic qualifiers specializers)))))

(defun %pad-specializers (specs n)
  (let ((r (reverse specs)) (k (length specs)))
    (while (< k n) (push (find-class 't) r) (setq k (1+ k)))
    (nreverse r)))

(defun remove-method (generic method)
  (let ((gf (%generic generic)))
    (when gf
      (setf (aref gf 2) (remove-if (lambda (m) (eq m method)) (aref gf 2)))
      (%generic-shape gf))
    generic))

(defun sort-by-car (pairs)              ; insertion sort, stable
  (let ((sorted nil))
    (dolist (p pairs)
      (let ((before nil) (after sorted))
        (while (and after (%rank<= (car (car after)) (car p)))
          (push (car after) before)
          (setq after (cdr after)))
        (setq sorted (append (reverse before) (cons p after)))))
    sorted))

;; ranks are lists of numbers, compared from the first argument's on
(defun %rank<= (a b)
  (loop
    (cond ((null a) (return t))
          ((< (car a) (car b)) (return t))
          ((> (car a) (car b)) (return nil))
          (t (setq a (cdr a) b (cdr b))))))

;; how specific method M is for ARGS (their cpls in CPLS), or NIL if it does not apply
(defun %method-rank (m args cpls arity)
  (let ((rank nil) (specs (aref m 6)) (k 0) (ok t))
    (while (and ok (< k arity))
      (let ((s (if specs (car specs) (find-class 't))))
        (cond ((%eql-specializer-p s)
               (if (eql (car args) (cadr s)) (push -1 rank) (setq ok nil)))
              (t (let ((p (%position s (car cpls))))
                   (if p (push p rank) (setq ok nil))))))
      (setq specs (cdr specs) args (cdr args) cpls (cdr cpls) k (1+ k)))
    (and ok (or (nreverse rank) (list 0)))))   ; no argument specialized: every method equal

;; => #(arounds befores primaries afters simple-method), each a list of functions
(defun compute-effective-method (gf args)
  (let* ((arity (aref gf 5))
         (cpls (let ((r nil) (a args) (k 0))
                 (while (< k arity) (push (class-precedence-list (class-of (car a))) r) (setq a (cdr a) k (1+ k)))
                 (nreverse r)))
         (ranked nil))
    (dolist (m (aref gf 2))
      (let ((rank (%method-rank m args cpls arity)))
        (when rank (push (cons rank m) ranked))))
    (let ((arounds nil) (befores nil) (primaries nil) (afters nil) (first-simple nil))
      (dolist (r (reverse (sort-by-car (reverse ranked))))     ; least specific first
        (let ((m (cdr r)))
          (case (aref m 1)
            (:around (push (aref m 3) arounds))
            (:before (push (aref m 3) befores))
            (:after (setq afters (append afters (list (aref m 3)))))
            (t (push (aref m 3) primaries) (setq first-simple (aref m 4))))))
      ;; the common case, one primary method that calls no next method: call it
      ;; directly, without the machinery of method combination
      (vector arounds befores primaries afters
              (if (and (null arounds) (null befores) (null afters) primaries first-simple)
                  (car primaries)
                  nil)))))

;; the effective method for ARGS, from the cache when it can be
(defun %effective-method (gf args)
  (unless (= (aref gf 4) *dispatch-generation*)
    (setf (aref gf 3) nil)
    (setf (aref gf 4) *dispatch-generation*))
  (let ((arity (aref gf 5)))
    (when (< (length args) arity)
      (error "Too few arguments to the generic function" (aref gf 1)))
    (cond ((aref gf 6) (compute-effective-method gf args))     ; EQL specializers: no cache
          ((<= arity 1)
           (let* ((key (if (= arity 1) (class-of (car args)) t))
                  (hit (assq key (aref gf 3))))
             (if hit
                 (cdr hit)
                 (let ((e (compute-effective-method gf args)))
                   (push (cons key e) (aref gf 3))
                   e))))
          (t
           (let* ((key (let ((r nil) (a args) (k 0))
                         (while (< k arity) (push (class-of (car a)) r) (setq a (cdr a) k (1+ k)))
                         (nreverse r)))
                  (hit (find-if (lambda (e) (%all-eq (car e) key)) (aref gf 3))))
             (if hit
                 (cdr hit)
                 (let ((e (compute-effective-method gf args)))
                   (push (cons key e) (aref gf 3))
                   e)))))))

(defun %dispatch (gf args)
  (when (and (null args) (> (aref gf 5) 0)) (error "A generic function needs an argument:" (aref gf 1)))
  (let ((em (%effective-method gf args)))
    (cond ((aref em 4) (apply (aref em 4) args))
          ((or (aref em 0) (aref em 1) (aref em 2) (aref em 3)) (run-effective-method em args))
          (t (apply #'no-applicable-method (symbol-function (aref gf 1)) args)))))

(defun no-applicable-method (generic &rest args)
  (error "No method of" (%function-name generic) "applies to" (if (cdr args) args (car args))))

;; CALL-NEXT-METHOD finds what to call next in these, bound around each method
(defvar *next-methods* nil)
(defvar *method-args* nil)
(defvar *inner-combination* nil)    ; set inside :around methods

(defun call-methods (methods args inner)
  (let ((*next-methods* (cdr methods)) (*method-args* args) (*inner-combination* inner))
    (apply (car methods) args)))

(defun run-inner (em args)
  (dolist (m (aref em 1)) (apply m args))
  (let ((result (if (aref em 2) (call-methods (aref em 2) args nil) nil)))
    (dolist (m (aref em 3)) (apply m args))
    result))

(defun run-effective-method (em args)
  (if (aref em 0)
      (call-methods (aref em 0) args em)
      (run-inner em args)))

(defun call-next-method (&rest args)
  (let ((args (or args *method-args*)))
    (cond (*next-methods* (call-methods *next-methods* args *inner-combination*))
          (*inner-combination*
           (let ((em *inner-combination*))
             (let ((*inner-combination* nil)) (run-inner em args))))
          (t (error "There is no next method")))))

(defun next-method-p ()
  (if (or *next-methods* *inner-combination*) t nil))

;;; ---- the defining macros --------------------------------------------------------

(defvar *lambda-list-keywords* '(&optional &rest &key &aux &allow-other-keys &body &whole &environment))

;; DOC strings: (documentation name type) finds them (cl3.lisp)
(defun %doc-key (type)
  (case type
    ((function compiler-macro setf) 'function-documentation)
    (variable 'variable-documentation)
    (t 'type-documentation)))

(defun %set-documentation (name type doc)
  (when (stringp doc) (putprop name doc (%doc-key type)))
  doc)

(defmacro defgeneric (name lambda-list &rest options)
  `(progn (ensure-generic ',name)
          ,@(let ((doc (cadr (assq :documentation options))))
              (and doc `((%set-documentation ',name 'function ,doc))))
          ,@(mapcar (lambda (o) `(defmethod ,name ,@(cdr o)))
                    (remove-if-not (lambda (o) (and (consp o) (eq (car o) :method))) options))
          ',name))

;; (defmethod name [qualifier] ((arg class) (arg (eql form)) more-args...) body...)
(defmacro defmethod (name &rest rest)
  (let* ((source (cons 'defmethod (cons name rest)))
         (qualifier (if (keywordp (car rest)) (car rest) nil))
         (rest (if qualifier (cdr rest) rest))
         (ll (car rest))
         (body (cdr rest))
         (required nil) (tail ll))
    (while (and tail (not (memq (car tail) *lambda-list-keywords*)))
      (push (car tail) required)
      (setq tail (cdr tail)))
    (setq required (nreverse required))
    (let* ((first (car required))
           (class (cond ((and (consp first) (symbolp (cadr first))) (cadr first)) (t 't)))
           (plain (append (mapcar (lambda (p) (if (consp p) (car p) p)) required) tail))
           (spec-forms (mapcar (lambda (p)
                                 (cond ((atom p) '(find-class 't))
                                       ((symbolp (cadr p)) `(find-class ',(cadr p)))
                                       (t `(list 'eql ,(cadr (cadr p))))))     ; (eql form)
                               required)))
      (when (returns-from-p name body)
        (setq body (list (cons 'block (cons name body)))))
      `(%add-method ',name ',qualifier ',class (lambda ,plain ,@body)
                    ,(not (or (tree-contains 'call-next-method body) (tree-contains 'next-method-p body)))
                    ',source
                    (list ,@spec-forms)))))

(defun tree-contains (atom tree)
  (cond ((eq atom tree) t)
        ((consp tree) (or (tree-contains atom (car tree)) (tree-contains atom (cdr tree))))
        (t nil)))

(defun plist-value (plist key)
  (let ((v (%getf-key plist key))) (if (eq v '%none) nil v)))

(defun slot-spec-form (slot)
  (let* ((name (if (consp slot) (car slot) slot))
         (options (if (consp slot) (cdr slot) nil))
         (initform (%getf-key options :initform)))
    `(list ',name
           ,(if (eq initform '%none) nil `(lambda () ,initform))
           ',(plist-value options :initarg)
           ',(and (eq (plist-value options :allocation) :class) :class))))

;; the readers, writers and accessors of a slot: each option may be given more than once
(defun accessor-forms (class-name slot)
  (when (consp slot)
    (let ((name (car slot)) (options (cdr slot)) (forms nil))
      (while options
        (let ((key (car options)) (f (cadr options)))
          (when (memq key '(:reader :accessor))
            (push `(defmethod ,f ((object ,class-name)) (slot-value object ',name)) forms))
          (when (eq key :accessor)
            (let ((writer (intern (string-append "SET-" (symbol-name f)))))
              (push `(defmethod ,writer ((object ,class-name) value) (set-slot-value object ',name value)) forms)
              (push `(putprop ',f ',writer 'setf-function) forms)))
          (when (eq key :writer)
            (if (and (consp f) (eq (car f) 'setf))
                (let ((writer (intern (string-append "SET-" (symbol-name (cadr f))))))
                  (push `(defmethod ,writer ((object ,class-name) value) (set-slot-value object ',name value)) forms)
                  (push `(putprop ',(cadr f) ',writer 'setf-function) forms))
                (push `(defmethod ,f (value (object ,class-name)) (set-slot-value object ',name value)) forms))))
        (setq options (cddr options)))
      (nreverse forms))))

(defmacro defclass (name superclasses slots &rest options)
  `(progn
     (%define-class ',name ',superclasses (list ,@(mapcar #'slot-spec-form slots)))
     (putprop ',name
              (list ,@(let ((d (cdr (assq :default-initargs options))) (r nil))
                        (while d (push `',(car d) r) (push `(lambda () ,(cadr d)) r) (setq d (cddr d)))
                        (nreverse r)))
              'default-initargs)
     ,@(let ((doc (cadr (assq :documentation options))))
         (and doc `((%set-documentation ',name 'type ,doc))))
     ,@(mapcan (lambda (s) (accessor-forms name s)) slots)
     ',name))

;;; ---- standard generic functions --------------------------------------------

(defgeneric initialize-instance (instance &rest initargs))
(defmethod initialize-instance ((instance t) &rest initargs)
  (%shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &rest initargs))
(defmethod reinitialize-instance ((instance t) &rest initargs)
  (%shared-initialize instance nil initargs))

(defgeneric shared-initialize (instance slot-names &rest initargs))
(defmethod shared-initialize ((instance t) slot-names &rest initargs)
  (%shared-initialize instance slot-names initargs))

(defgeneric update-instance-for-redefined-class (instance added discarded plist &rest initargs))
(defmethod update-instance-for-redefined-class ((instance t) added discarded plist &rest initargs)
  (%shared-initialize instance added initargs))

;; CHANGE-CLASS: the instance becomes one of NEW-CLASS, keeping the values of
;; the slots both classes have; the others get their initforms
(defgeneric update-instance-for-different-class (previous current &rest initargs))
(defmethod update-instance-for-different-class ((previous t) current &rest initargs)
  (let ((added (remove-if (lambda (n) (slot-exists-p previous n)) (aref (aref current 1) 7))))
    (%shared-initialize current added initargs)))

(defun change-class (instance new-class &rest initargs)
  (let* ((new (if (symbolp new-class) (find-class new-class) new-class))
         (previous (%copy-instance instance))
         (names (aref new 7))
         (slots (make-vector (length names) *unbound-slot*))
         (i 0))
    (dolist (n names)
      (when (and (%position n (aref previous 3)) (not (eq (%slot-value previous n) *unbound-slot*)))
        (setf (aref slots i) (%slot-value previous n)))
      (setq i (1+ i)))
    (setf (aref instance 1) new)
    (setf (aref instance 2) slots)
    (setf (aref instance 3) names)
    (apply #'update-instance-for-different-class previous instance initargs)
    instance))

(defgeneric slot-unbound (class instance name))
(defmethod slot-unbound ((class t) instance name)
  (if (find-class 'unbound-slot nil)
      (error 'unbound-slot :name name :instance instance)
      (error "Unbound slot" name)))

(defgeneric slot-missing (class instance name operation &optional new-value))
(defmethod slot-missing ((class t) instance name operation &optional new-value)
  (error "No slot named" name "in" instance))

(unless (fboundp 'present) (fset 'present #'prin1))   ; until presentations.lisp

;; DESCRIBE writes what DESCRIBE-OBJECT says; a program can add methods to it
(defgeneric describe-object (object stream))
(defmethod describe-object ((x t) stream)
  (let ((*standard-output* stream))
    (present x)
    (princ " is ")
    (if (instancep x)
        (progn
          (princ "an instance of ") (prin1 (class-name (class-of x))) (terpri)
          (dolist (name (class-slot-names (class-of x)))
            (princ "  ") (prin1 name) (princ ": ")
            (let ((v (%slot-value x name)))
              (if (eq v *unbound-slot*) (princ "unbound") (present v)))
            (terpri)))
        (progn (princ "a ") (prin1 (class-name (class-of x))) (terpri)))))

(defun describe (x &optional stream)
  (let ((s (resolve-stream stream)))
    (fresh-line s)
    (describe-object x s))
  (values))
