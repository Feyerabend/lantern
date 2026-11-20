;;; -*- Mode: Lisp -*-
;;; compiler.lisp — compiles Lisp to bytecode for the VM in kernel/vm.c.
;;;
;;; Loaded right after boot.lisp, by the interpreter. At the end of this file
;;; the compiler compiles itself and everything defined so far, then becomes
;;; the evaluator: from then on every form typed or loaded is compiled first.
;;;
;;; Passes:
;;;   1. MX expands macros and rewrites derived forms into a small core
;;;      language: QUOTE FUNCTION IF PROGN SETQ LET WHILE AND OR CATCH THROW
;;;      UNWIND-PROTECT %HANDLER FUNCALL %NAMED-LAMBDA %NAMED-MACRO and calls.
;;;      BLOCK/RETURN-FROM become CATCH/THROW on a fresh tag; COND becomes IF.
;;;   2. COMP walks the core form and emits symbolic instructions.
;;;   3. ASSEMBLE resolves labels and produces bytes for %MAKE-CODE.
;;;
;;; Variables live in stack slots, except those a nested LAMBDA refers to:
;;; they live in a heap "environment vector" (slot 0 = parent environment)
;;; that closures capture. Variables proclaimed special (DEFVAR) are bound
;;; dynamically with BIND/UNBIND.
;;;
;;; This file must run under the interpreter, so it avoids BLOCK, CATCH and
;;; non-constant &optional defaults itself.

;;; ---- opcodes (the table lives in the kernel) ------------------------------

(defvar *opcodes* (%opcodes))

(let ((i 0))
  (dolist (o *opcodes*)
    (putprop (car o) i 'opcode)
    (putprop (car o) (cadr o) 'operands)
    (setq i (1+ i))))

;; calls with these names and argument counts become single instructions
(dolist (e '((car 1 . car) (cdr 1 . cdr) (cons 2 . cons) (eq 2 . eq)
             (not 1 . not) (null 1 . not) (consp 1 . consp)
             (+ 2 . add) (- 2 . sub) (< 2 . lt) (> 2 . gt) (= 2 . numeq)
             (<= 2 . le) (>= 2 . ge) (1+ 1 . add1) (1- 1 . sub1)))
  (when (cddr e)
    (putprop (car e) (cons (cadr e) (cddr e)) 'inline-op)))

;;; ---- small helpers ---------------------------------------------------------

(defun %position (x list)
  (let ((i 0) (r nil))
    (while (and list (null r))
      (if (eq (car list) x) (setq r i) (setq i (1+ i) list (cdr list))))
    r))

;; the kernel keeps a keyword's colon in its name (%SYMBOL-NAME); SYMBOL-NAME
;; leaves it out, as Common Lisp says (M17)
(defun keywordp (x)
  (and (symbolp x) x (let ((n (%symbol-name x))) (and (> (length n) 0) (eq (aref n 0) #\:)))))

(defun iota-list (n)
  (let ((r nil))
    (while (> n 0) (setq n (1- n)) (push n r))
    r))

(defun list->vector (list) (apply #'vector list))

;;; ---- pass 1: macro expansion into the core language -------------------------

(defun mx-list (forms blocks)
  (mapcar (lambda (f) (mx f blocks)) forms))

(defun mx-ll (ll blocks)
  (mapcar (lambda (p) (if (consp p) (cons (car p) (cons (mx (cadr p) blocks) (cddr p))) p)) ll))

(defun mx-lambda-rest (ll-and-body blocks)
  (cons (mx-ll (car ll-and-body) blocks)
        (mx-list (cdr ll-and-body) (shadow-symbol-macros (ll-var-names (car ll-and-body)) blocks))))

;;; ---- symbol macros (M17) ----------------------------------------------------------
;;;
;;; SYMBOL-MACROLET's names travel in BLOCKS as ((:sym . name) expansion); a
;;; variable of the same name bound inside shadows one, as
;;; ((:sym . name) %shadowed). DEFINE-SYMBOL-MACRO puts the expansion on the
;;; symbol's SYMBOL-MACRO property. A symbol macro used as a variable is
;;; replaced by its expansion; SETQ of one is a SETF of its expansion.

(defun local-symbol-macro (name blocks)
  (let ((r nil))
    (while (and blocks (null r))
      (let ((e (car blocks)))
        (when (and (consp (car e)) (eq (car (car e)) :sym) (eq (cdr (car e)) name)) (setq r e)))
      (setq blocks (cdr blocks)))
    r))

;; => (expansion) when the symbol X is a symbol macro here, else NIL
(defun symbol-macro-expansion (x blocks)
  (let ((e (local-symbol-macro x blocks)))
    (cond (e (if (eq (cadr e) '%shadowed) nil (list (cadr e))))
          ((get x 'symbol-macro) (list (get x 'symbol-macro)))
          (t nil))))

(defun shadow-symbol-macros (names blocks)
  (dolist (n names)
    (when (and (symbolp n) n (symbol-macro-expansion n blocks))
      (push (list (cons :sym n) '%shadowed) blocks)))
  blocks)

;; the variables a lambda list binds
(defun ll-var-names (ll)
  (let ((r nil))
    (while (consp ll)
      (let ((p (car ll)))
        (cond ((memq p '(&optional &rest &key &aux &body &allow-other-keys &whole &environment)) nil)
              ((symbolp p) (push p r))
              ((consp p)
               (push (if (consp (car p)) (cadr (car p)) (car p)) r)
               (when (and (consp (cdr p)) (consp (cddr p)) (symbolp (caddr p))) (push (caddr p) r)))))
      (setq ll (cdr ll)))
    (when (and ll (symbolp ll)) (push ll r))         ; a dotted rest variable
    r))

(defun mx-setq (args blocks)
  (let ((forms nil) (plain t))
    (while args
      (let ((e (and (symbolp (car args)) (symbol-macro-expansion (car args) blocks))))
        (when e (setq plain nil))
        (push (if e (list 'setf (car e) (cadr args)) (list 'setq (car args) (cadr args))) forms))
      (setq args (cddr args)))
    (setq forms (nreverse forms))
    (if (and plain (= (length forms) 1))
        (list 'setq (cadr (car forms)) (mx (caddr (car forms)) blocks))
        (if plain
            (cons 'setq (apply #'append (mapcar (lambda (f) (list (cadr f) (mx (caddr f) blocks))) forms)))
            (mx (cons 'progn forms) blocks)))))

;; Reject malformed special forms early, with the form in the message.
(defun check-syntax (op x)
  (let ((n (list-length-or-nil x)))
    (cond ((null n) (error "Malformed form (dotted list):" x))
          ((and (eq op 'if) (< n 3)) (error "IF needs a test and a then form:" x))
          ((and (eq op 'quote) (/= n 2)) (error "QUOTE takes one argument:" x))
          ((and (eq op 'setq) (oddp n)) nil)
          ((eq op 'setq) (error "SETQ needs variable/value pairs:" x))
          ((and (memq op '(let let*)) (or (< n 2) (not (listp (cadr x)))))
           (error "Malformed binding list:" x))
          ((and (memq op '(defun defmacro)) (or (< n 3) (not (or (symbolp (cadr x))
                                                                  (and (eq op 'defun) (consp (cadr x))
                                                                       (eq (car (cadr x)) 'setf))))))
           (error "Malformed definition:" x)))))

;; the length of a proper list; NIL for a dotted or circular one
(defun list-length-or-nil (x)
  (let ((n 0) (slow x) (circular nil))
    (while (and (consp x) (not circular))
      (setq n (1+ n) x (cdr x))
      (when (= (rem n 2) 0)                     ; the slow one moves at half speed
        (setq slow (cdr slow))
        (when (eq slow x) (setq circular t))))
    (if (and (null x) (not circular)) n nil)))

(defun /= (a b) (not (= a b)))

;; Does lambda list LL need DESTRUCTURING-BIND: a nested pattern among the
;; required parameters, a dotted tail, &whole or &environment?
(defun destructuring-ll-p (ll)
  (let ((r nil) (required t))
    (while (and (consp ll) (null r))
      (let ((p (car ll)))
        (cond ((memq p '(&whole &environment)) (setq r t))
              ((memq p '(&optional &rest &body &key &aux &allow-other-keys)) (setq required nil))
              ((and required (consp p)) (setq r t))))
      (setq ll (cdr ll)))
    (or r (not (null ll)))))

;; Does X contain (return-from NAME ...) anywhere?
(defun returns-from-p (name x)
  (cond ((atom x) nil)
        ((and (eq (car x) 'return-from) (consp (cdr x)) (eq (cadr x) name)) t)
        (t (or (returns-from-p name (car x)) (returns-from-p name (cdr x))))))

(defun simple-application-p (ll args)
  (and (listp ll)
       (every #'symbolp ll)
       (not (memq '&optional ll)) (not (memq '&rest ll)) (not (memq '&body ll))
       (= (length ll) (length args))))

(defun pair-up (vars vals)
  (let ((r nil))
    (while vars
      (push (list (car vars) (car vals)) r)
      (setq vars (cdr vars) vals (cdr vals)))
    (nreverse r)))

(defun let*->let (bindings body)
  (if (null bindings)
      (cons 'let (cons nil body))
      (list 'let (list (car bindings)) (let*->let (cdr bindings) body))))

(defun cond->if (clauses)
  (if (null clauses)
      nil
      (let ((c (car clauses)))
        (cond ((null (cdr c)) (list 'or (car c) (cond->if (cdr clauses))))
              ((eq (car c) t) (cons 'progn (cdr c)))
              (t (list 'if (car c) (cons 'progn (cdr c)) (cond->if (cdr clauses))))))))

;; HANDLER-CASE (M13) is a catch around the form, with a handler for each
;; clause on the handler stack, *HANDLER-CLUSTERS*, that throws the condition
;; to the catch. Handlers run when the condition is signalled, before
;; anything unwinds (the kernel calls %SIGNAL-HOOK for its own errors), so
;; HANDLER-BIND and HANDLER-CASE nest as Common Lisp says.
;;   (handler-case form (type (var) body...)... [(:no-error (vars...) body...)])
;;   => (let* ((tag (list nil))
;;             (r (catch tag (let ((*handler-clusters*
;;                                  (cons (list (cons 'type (lambda (c) (throw tag (list 0 c))))...)
;;                                        *handler-clusters*)))
;;                             (cons tag (multiple-value-list form))))))
;;        (if (eq (car r) tag) (apply #'values (cdr r))
;;            (let ((c (cadr r))) (if (eql (car r) 0) (let ((var c)) body...) ...))))
(defun expand-handler-case (x)
  (let* ((tag (gensym)) (r (gensym)) (c (gensym)) (i -1)
         (clauses (remove-if (lambda (cl) (eq (car cl) :no-error)) (cddr x)))
         (no-error (car (remove-if-not (lambda (cl) (eq (car cl) :no-error)) (cddr x))))
         (handlers (mapcar (lambda (cl)
                             (setq i (1+ i))
                             (list 'cons (list 'quote (car cl))
                                   (list 'function (list 'lambda (list c)
                                                         (list 'throw tag (list 'list i c))))))
                           clauses))
         (bodies nil))
    (setq i -1)
    (dolist (cl clauses)
      (setq i (1+ i))
      (push (list (list 'eql (list 'car r) i)
                  (cons 'let (cons (list (list (if (cadr cl) (car (cadr cl)) (gensym)) (list 'cadr r)))
                                   (or (cddr cl) (list nil)))))
            bodies))
    (list 'let* (list (list tag (list 'list nil))
                      (list r (list 'catch tag
                                    (list 'let (list (list '*handler-clusters*
                                                           (list 'cons (cons 'list handlers) '*handler-clusters*)))
                                          (list 'cons tag (list 'multiple-value-list (cadr x)))))))
          (list 'if (list 'eq (list 'car r) tag)
                (if no-error
                    (list 'apply (list 'function (cons 'lambda (cdr no-error))) (list 'cdr r))
                    (list 'apply '(function values) (list 'cdr r)))
                (cons 'cond (nreverse bodies))))))

(defun mx (x blocks)
  (if (atom x)
      (let ((e (and (symbolp x) x (symbol-macro-expansion x blocks))))   ; a symbol macro (M17)
        (if e (mx (car e) blocks) x))
      (let ((op (car x)))
        (check-syntax op x)
        (cond
          ((eq op 'quote) x)
          ((eq op 'function)
           (cond ((and (consp (cadr x)) (eq (car (cadr x)) 'lambda))
                  (list 'function (cons 'lambda (mx-lambda-rest (cdr (cadr x)) blocks))))
                 ((local-function (cadr x) blocks :function) (caddr (local-function (cadr x) blocks :function)))
                 ((and (consp (cadr x)) (eq (car (cadr x)) 'setf))      ; #'(setf name)
                  (list 'function (setf-function-symbol (cadr (cadr x)))))
                 (t x)))
          ((eq op 'flet) (mx-flet x blocks nil))
          ((eq op 'labels) (mx-flet x blocks t))
          ((eq op 'macrolet) (mx-macrolet x blocks))
          ((eq op 'lambda) (list 'function (cons 'lambda (mx-lambda-rest (cdr x) blocks))))
          ((memq op '(%named-lambda %named-macro))
           (cons op (cons (cadr x) (mx-lambda-rest (cddr x) blocks))))
          ((and (eq op 'defun) (consp (cadr x)) (eq (car (cadr x)) 'setf))
           ;; (defun (setf name) (new args...) ...): a function SETF calls (M13)
           (let ((f (setf-function-symbol (cadr (cadr x)))))
             (mx (list 'progn
                       (cons 'defun (cons f (cddr x)))
                       (list 'putprop (list 'quote (cadr (cadr x))) (list 'quote f) ''setf-cl-function)
                       (list 'quote (cadr x)))
                 blocks)))
          ((memq op '(defun defmacro))
           ;; the body is an implicit (block name ...), set up only when used
           (let ((name (cadr x)) (ll (caddr x)) (body (cdddr x)))
             ;; a macro's lambda list may destructure (M13): take the arguments
             ;; as a list and let DESTRUCTURING-BIND (cl2.lisp) take them apart
             (when (and (eq op 'defmacro) (destructuring-ll-p ll) (macro-function 'destructuring-bind))
               (let ((g (gensym)))
                 (setq body (list (cons 'destructuring-bind (cons ll (cons g body))))
                       ll (list '&rest g))))
             (when (returns-from-p name body)
               (setq body (list (cons 'block (cons name body)))))
             (mx (list 'progn
                       (list 'fset (list 'quote name)
                             (cons (if (eq op 'defun) '%named-lambda '%named-macro) (cons name (cons ll body))))
                       (list 'putprop (list 'quote name) (list 'quote x) ''source)   ; for Show Definition
                       ;; a doc string, for DOCUMENTATION (M17)
                       (list 'putprop (list 'quote name)
                             (let ((b (cdddr x))) (and (stringp (car b)) (cdr b) (car b)))
                             ''function-documentation)
                       (list 'quote name))
                 blocks)))
          ((eq op 'setq) (mx-setq (cdr x) blocks))
          ((eq op 'symbol-macrolet)
           (let ((inner blocks))
             (dolist (b (cadr x)) (push (list (cons :sym (car b)) (cadr b)) inner))
             (mx (cons 'progn (cddr x)) inner)))
          ((eq op 'let)
           (cons 'let
                 (cons (mapcar (lambda (b) (if (consp b) (list (car b) (mx (cadr b) blocks)) (list b nil)))
                               (cadr x))
                       (mx-list (cddr x) (shadow-symbol-macros (mapcar (lambda (b) (if (consp b) (car b) b)) (cadr x))
                                                               blocks)))))
          ((eq op 'let*) (mx (let*->let (cadr x) (cddr x)) blocks))
          ((eq op 'cond) (mx (cond->if (cdr x)) blocks))
          ;; a BLOCK is a CATCH of a fresh tag, but only if some RETURN-FROM
          ;; uses it: DOLIST and DOTIMES make blocks that most loops never leave
          ;; early, and those cost nothing now (no tag allocated, no catch)
          ((eq op 'block)
           (let* ((g (gensym)) (used (list nil))
                  (body (mx-list (cddr x) (cons (list (cadr x) g used) blocks))))
             (if (car used)
                 (list 'let (list (list g '(cons nil nil))) (cons 'catch (cons g body)))
                 (cons 'progn body))))
          ((eq op 'return-from)
           (let ((b (assq (cadr x) blocks)))
             (if b
                 (progn (setf (car (caddr b)) t)
                        (list 'throw (cadr b) (mx (caddr x) blocks)))
                 (error "RETURN-FROM: no block named" (cadr x)))))
          ((eq op 'handler-case) (mx (expand-handler-case x) blocks))
          ((eq op 'tagbody) (mx-tagbody x blocks))
          ((eq op 'go)
           (let ((e (local-tag (cadr x) blocks)))
             (unless e (error "GO: no tag named" (cadr x)))
             (list 'throw (cadr e) (caddr e))))
          ((and (symbolp op) (local-function op blocks nil))       ; FLET, LABELS, MACROLET
           (let ((e (local-function op blocks nil)))
             (if (eq (cadr e) :macro)
                 (mx (apply (caddr e) (cdr x)) blocks)
                 (cons 'funcall (cons (caddr e) (mx-list (cdr x) blocks))))))
          ((and (symbolp op) (macro-function op))
           (mx (apply (macro-function op) (cdr x)) blocks))
          ((and (consp op) (eq (car op) 'lambda))
           (if (simple-application-p (cadr op) (cdr x))
               ;; ((lambda (a b) body) x y) => (let ((a x) (b y)) body); the interpreter
               ;; leaves every LET it has run displaced into this shape
               (mx (cons 'let (cons (pair-up (cadr op) (cdr x)) (cddr op))) blocks)
               (cons (cons 'lambda (mx-lambda-rest (cdr op) blocks)) (mx-list (cdr x) blocks))))
          (t (cons op (mx-list (cdr x) blocks)))))))

;;; ---- TAGBODY and GO (M14) ----------------------------------------------------------
;;;
;;; The body is cut at its tags into segments 0, 1, 2 ... and run as a loop
;;; around a CATCH of a fresh object; GO throws the number of the segment to
;;; start at, so it unwinds whatever is in between, as a throw does:
;;;   (tagbody s0... tag1 s1... tag2 s2...)
;;;   => (let ((#:tb (cons nil nil)) (#:at 0))
;;;        (while (not (eq #:at :done))
;;;          (setq #:at (catch #:tb (if (<= #:at 0) (progn s0...)) (if (<= #:at 1) (progn s1...)) ... :done)))
;;;        nil)
;;; Running from segment k on falls through the later ones, as TAGBODY does.
;;; The tags travel in BLOCKS as ((:tag . name) tb-variable number).

(defun local-tag (name blocks)
  (let ((r nil))
    (while (and blocks (null r))
      (let ((e (car blocks)))
        (when (and (consp (car e)) (eq (car (car e)) :tag) (eql (cdr (car e)) name)) (setq r e)))
      (setq blocks (cdr blocks)))
    r))

(defun tagbody-tag-p (x) (or (symbolp x) (integerp x)))

(defun mx-tagbody (x blocks)
  (let ((tb (gensym)) (at (gensym)) (segments (list nil)) (inner blocks) (n 0))
    (dolist (item (cdr x))
      (if (tagbody-tag-p item)
          (progn (setq n (1+ n))
                 (push (list (cons :tag item) tb n) inner)
                 (push nil segments))
          (push item (car segments))))
    (if (= n 0)
        (mx (cons 'progn (append (reverse (car segments)) (list nil))) blocks)
        (let ((k -1) (body nil))
          (dolist (seg (reverse segments))
            (setq k (1+ k))
            (push (list 'if (list '<= at k) (cons 'progn (append (mx-list (reverse seg) inner) (list nil))))
                  body))
          (list 'let (list (list tb '(cons nil nil)) (list at 0))
                (list 'while (list 'not (list 'eq at :done))
                      (list 'setq at (cons 'catch (cons tb (append (nreverse body) (list :done))))))
                nil)))))

;;; ---- local functions: FLET, LABELS, MACROLET ------------------------------------
;;;
;;; The lexical function names travel in BLOCKS too, as entries
;;; ((:fn . name) :function var) and ((:fn . name) :macro expander); the
;;; (:fn . name) cons can never be a block name. A local function is a
;;; variable holding a closure, and a call to it is a FUNCALL of that variable:
;;;   (flet ((f (x) ...)) body)    => (let ((#:f (%named-lambda (flet f) (x) ...))) body)
;;;   (labels ((f (x) ...)) body)  => (let ((#:f nil)) (setq #:f (%named-lambda ...)) body)
;;; In LABELS the definitions see each other; in FLET only the body sees them.

;; the innermost local function or macro named NAME (of KIND, or either)
(defun local-function (name blocks kind)
  (let ((r nil))
    (while (and blocks (null r))
      (let ((e (car blocks)))
        (when (and (consp (car e)) (eq (car (car e)) :fn) (eq (cdr (car e)) name) (or (null kind) (eq (cadr e) kind)))
          (setq r e)))
      ;; a local macro shadows a local function of the same name
      (when (and r kind (not (eq (cadr r) kind))) (setq r nil))
      (setq blocks (cdr blocks)))
    r))

(defun local-lambda (kind def blocks)
  (mx (cons '%named-lambda
            (cons (list kind (car def))
                  (cons (cadr def) (list (cons 'block (cons (car def) (cddr def)))))))
      blocks))

(defun mx-flet (x blocks recursive)
  (let ((inner blocks) (vars nil))
    (dolist (d (cadr x))
      (when (or (atom d) (not (symbolp (car d))) (atom (cdr d)))
        (error "Malformed local function definition:" d))
      (let ((g (make-symbol (symbol-name (car d)))))
        (push g vars)
        (setq inner (cons (list (cons :fn (car d)) :function g) inner))))
    (setq vars (nreverse vars))
    (let ((lambdas (mapcar (lambda (d) (local-lambda (if recursive 'labels 'flet) d (if recursive inner blocks)))
                           (cadr x)))
          (body (mx-list (cddr x) inner)))
      (if recursive
          (cons 'let (cons (mapcar (lambda (v) (list v nil)) vars)
                           (append (mapcar (lambda (b) (cons 'setq b)) (pair-up vars lambdas)) body)))
          (cons 'let (cons (pair-up vars lambdas) body))))))

;; the expanders are made when the form is compiled, outside any lexical scope
(defun mx-macrolet (x blocks)
  (let ((inner blocks))
    (dolist (d (cadr x))
      (setq inner (cons (list (cons :fn (car d)) :macro
                              (eval (list 'function (cons 'lambda (cons (cadr d) (cddr d))))))
                        inner)))
    (cons 'progn (mx-list (cddr x) inner))))

;;; ---- free-variable analysis -----------------------------------------------------

;; Collects every symbol used as a variable inside a nested LAMBDA. A variable
;; bound by an enclosing construct and found here must live in an environment
;; vector. Shadowing is ignored, so this errs on the safe side.
(defvar *captured* nil)

(defun walk-list (forms inside)
  (dolist (f forms) (walk-captured f inside)))

(defun walk-lambda (ll body)
  (dolist (p ll) (when (consp p) (walk-captured (cadr p) t)))
  (walk-list body t))

(defun walk-captured (x inside)
  (cond ((symbolp x) (when (and inside x (not (eq x t))) (push x *captured*)))
        ((atom x) nil)
        (t (let ((op (car x)))
             (cond ((eq op 'quote) nil)
                   ((eq op 'function)
                    (when (consp (cadr x)) (walk-lambda (cadr (cadr x)) (cddr (cadr x)))))
                   ((memq op '(%named-lambda %named-macro)) (walk-lambda (caddr x) (cdddr x)))
                   ((eq op 'let)
                    (dolist (b (cadr x)) (walk-captured (cadr b) inside))
                    (walk-list (cddr x) inside))
                   ((consp op) (walk-lambda (cadr op) (cddr op)) (walk-list (cdr x) inside))
                   (t (walk-list (cdr x) inside)))))))

(defun captured-vars (vars forms)
  (setq *captured* nil)
  (walk-list forms nil)
  (remove-if-not (lambda (v) (memq v *captured*)) vars))

;;; ---- compilation state ---------------------------------------------------------

;; function context: #(code consts nconsts nlocals maxlocals name)
(defun make-fc (name) (vector nil nil 0 0 0 name))

(defun emit (fc &rest instr) (push instr (aref fc 0)))
(defun make-label () (list :label nil))
(defun emit-label (fc label) (push label (aref fc 0)))

(defun add-const (fc x)
  (let ((l (aref fc 1)) (i (1- (aref fc 2))) (found nil))
    (while (and l (null found))
      (if (eq (car l) x) (setq found i) (setq l (cdr l) i (1- i))))
    (if found
        found
        (progn (push x (aref fc 1))
               (setf (aref fc 2) (1+ (aref fc 2)))
               (1- (aref fc 2))))))

(defun alloc-slot (fc)
  (let ((n (aref fc 3)))
    (setf (aref fc 3) (1+ n))
    (when (> (1+ n) (aref fc 4)) (setf (aref fc 4) (1+ n)))
    (when (> n 250) (error "Too many local variables in" (aref fc 5)))
    n))

;; scope: #(lenv eloc chain)
;;   lenv   alist of (name :local slot) / (name :env id index) / (name :special)
;;   eloc   where the current environment vector is: :cenv (the closure's) or a slot
;;   chain  ids of the environment vectors reachable from it, innermost first
(defun make-scope (lenv eloc chain) (vector lenv eloc chain))
(defun copy-scope (s) (vector (aref s 0) (aref s 1) (aref s 2)))
(defun scope-add (s entry) (push entry (aref s 0)))

(defun emit-load-env (fc s)
  (if (eq (aref s 1) :cenv) (emit fc 'cenv) (emit fc 'lref (aref s 1))))

;;; ---- pass 2: code generation -------------------------------------------------------

(defun comp-const (x fc)
  (cond ((null x) (emit fc 'pushnil))
        ((eq x t) (emit fc 'pusht))
        ((and (integerp x) (>= x -128) (< x 128)) (emit fc 'fix x))
        (t (emit fc 'const (add-const fc x)))))

(defun comp-ref (x fc s)
  (if (or (null x) (eq x t) (keywordp x))
      (comp-const x fc)
      (let ((e (assq x (aref s 0))))
        (cond ((null e) (emit fc 'gref (add-const fc x)))
              ((eq (cadr e) :local) (emit fc 'lref (caddr e)))
              ((eq (cadr e) :env)
               (emit-load-env fc s)
               (emit fc 'envref (%position (caddr e) (aref s 2)) (cadddr e)))
              (t (emit fc 'gref (add-const fc x)))))))

(defun comp-store (x fc s)       ; value is on the stack
  (when (or (null x) (eq x t) (keywordp x) (not (symbolp x)))
    (error "Cannot assign to" x))
  (let ((e (assq x (aref s 0))))
    (cond ((null e) (emit fc 'gset (add-const fc x)))
          ((eq (cadr e) :local) (emit fc 'lset (caddr e)))
          ((eq (cadr e) :env)
           (emit-load-env fc s)
           (emit fc 'envset (%position (caddr e) (aref s 2)) (cadddr e)))
          (t (emit fc 'gset (add-const fc x))))))

(defun comp (x fc s tail)
  (cond ((symbolp x) (comp-ref x fc s) (when tail (emit fc 'ret)))
        ((atom x) (comp-const x fc) (when tail (emit fc 'ret)))
        (t (comp-form x fc s tail))))

(defun comp-body (forms fc s tail)
  (if (null forms)
      (progn (emit fc 'pushnil) (when tail (emit fc 'ret)))
      (progn
        (while (cdr forms)
          (comp (car forms) fc s nil)
          (emit fc 'pop)
          (setq forms (cdr forms)))
        (comp (car forms) fc s tail))))

(defun comp-form (x fc s tail)
  (let ((op (car x)))
    (cond
      ((eq op 'quote) (comp-const (cadr x) fc) (when tail (emit fc 'ret)))
      ((eq op 'function) (comp-function (cadr x) fc s) (when tail (emit fc 'ret)))
      ((eq op '%named-lambda)
       (comp-lambda (cadr x) (caddr x) (cdddr x) nil fc s) (when tail (emit fc 'ret)))
      ((eq op '%named-macro)
       (comp-lambda (cadr x) (caddr x) (cdddr x) t fc s) (when tail (emit fc 'ret)))
      ((eq op 'if) (comp-if x fc s tail))
      ((eq op 'progn) (comp-body (cdr x) fc s tail))
      ((eq op 'setq) (comp-setq (cdr x) fc s tail))
      ((eq op 'let)
       (dolist (b (cadr x)) (comp (cadr b) fc s nil))
       (comp-bind (mapcar #'car (cadr x)) (cddr x) fc s tail))
      ((eq op 'and) (comp-andor (cdr x) fc s tail 'jmpnil-keep t))
      ((eq op 'or) (comp-andor (cdr x) fc s tail 'jmpt-keep nil))
      ((eq op 'while) (comp-while x fc s tail))
      ((eq op 'catch) (comp-catch x fc s tail))
      ((eq op 'multiple-value-list)            ; the values of one form, as a list (M11)
       (comp (cadr x) fc s nil)
       (emit fc 'mvlist)
       (when tail (emit fc 'ret)))
      ((eq op 'throw)
       (comp (cadr x) fc s nil)
       (comp (caddr x) fc s nil)
       (emit fc 'throw))
      ((eq op 'unwind-protect) (comp-unwind-protect x fc s tail))
      ((eq op '%handler) (comp-handler x fc s tail))
      ((eq op 'funcall)
       (comp (cadr x) fc s nil)
       (comp-args (cddr x) fc s tail))
      ((consp op)                            ; ((lambda ...) args)
       (comp-function op fc s)
       (comp-args (cdr x) fc s tail))
      ((not (symbolp op)) (error "Illegal function" op))
      (t (comp-call op (cdr x) fc s tail)))))

(defun comp-args (args fc s tail)       ; the function is already on the stack
  (let ((n (length args)))
    (when (> n 255) (error "Too many arguments in a call" n))
    (dolist (a args) (comp a fc s nil))
    (emit fc (if tail 'tailcall 'call) n)))

(defun comp-call (f args fc s tail)
  (let ((inl (get f 'inline-op)))
    (if (and inl (= (length args) (car inl)))
        (progn (dolist (a args) (comp a fc s nil))
               (emit fc (cdr inl))
               (when tail (emit fc 'ret)))
        (progn (emit fc 'gfunc (add-const fc f))
               (comp-args args fc s tail)))))

(defun comp-if (x fc s tail)
  (let ((else-label (make-label)) (end (make-label)))
    (comp (cadr x) fc s nil)
    (emit fc 'jmpnil else-label)
    (comp (caddr x) fc s tail)
    (unless tail (emit fc 'jmp end))
    (emit-label fc else-label)
    (comp-body (cdddr x) fc s tail)
    (emit-label fc end)))

(defun comp-andor (args fc s tail jump empty)
  (if (null args)
      (progn (comp-const empty fc) (when tail (emit fc 'ret)))
      (let ((end (make-label)))
        (while (cdr args)
          (comp (car args) fc s nil)
          (emit fc jump end)
          (setq args (cdr args)))
        (comp (car args) fc s tail)
        (emit-label fc end)
        (when tail (emit fc 'ret)))))

(defun comp-setq (pairs fc s tail)
  (if (null pairs)
      (emit fc 'pushnil)
      (while pairs
        (comp (cadr pairs) fc s nil)
        (when (null (cddr pairs)) (emit fc 'dup))
        (comp-store (car pairs) fc s)
        (setq pairs (cddr pairs))))
  (when tail (emit fc 'ret)))

(defun comp-while (x fc s tail)
  (let ((top (make-label)) (end (make-label)))
    (emit-label fc top)
    (comp (cadr x) fc s nil)
    (emit fc 'jmpnil end)
    (dolist (f (cddr x)) (comp f fc s nil) (emit fc 'pop))
    (emit fc 'jmp top)
    (emit-label fc end)
    (emit fc 'pushnil)
    (when tail (emit fc 'ret))))

(defun comp-catch (x fc s tail)
  (let ((l (make-label)))
    (comp (cadr x) fc s nil)
    (emit fc 'catch l)
    (comp-body (cddr x) fc s nil)
    (emit fc 'uncatch)
    (emit-label fc l)
    (when tail (emit fc 'ret))))

(defun comp-unwind-protect (x fc s tail)
  (let ((l (make-label)))
    (emit fc 'uwp l)
    (comp (cadr x) fc s nil)
    (emit fc 'unprotect)
    (emit-label fc l)
    (dolist (c (cddr x)) (comp c fc s nil) (emit fc 'pop))
    (emit fc 'end-uwp)
    (when tail (emit fc 'ret))))

;; (%handler form var handler-body)
(defun comp-handler (x fc s tail)
  (let ((h (make-label)) (end (make-label)))
    (emit fc 'handler h)
    (comp (cadr x) fc s nil)
    (emit fc 'uncatch)
    (if tail (emit fc 'ret) (emit fc 'jmp end))
    (emit-label fc h)                   ; the condition is on the stack
    (comp-bind (list (caddr x)) (list (cadddr x)) fc s tail)
    (emit-label fc end)))

;; Bind VARS to the values on top of the stack (first var deepest) and
;; compile BODY in the new scope.
(defun comp-bind (vars body fc s tail)
  (let ((saved (aref fc 3))
        (slots (mapcar (lambda (v) (alloc-slot fc)) vars)))
    (dolist (sl (reverse slots)) (emit fc 'lset sl))
    (let* ((r (bind-vars vars slots body fc s))
           (s2 (car r))
           (nspecial (cdr r)))
      (if (= nspecial 0)
          (comp-body body fc s2 tail)
          (progn (comp-body body fc s2 nil)
                 (emit fc 'unbind nspecial)
                 (when tail (emit fc 'ret)))))
    (setf (aref fc 3) saved)))

;; VARS currently sit in local SLOTS. Move captured ones into a new
;; environment vector and bind special ones dynamically. Returns
;; (new-scope . number-of-special-bindings).
(defun bind-vars (vars slots forms fc s)
  (let* ((specials (remove-if-not #'special-variable-p vars))
         (capt (remove-if (lambda (v) (memq v specials)) (captured-vars vars forms)))
         (s2 (copy-scope s))
         (nspecial 0))
    (when capt
      (let ((id (list :env)) (envslot (alloc-slot fc)) (idx 1))
        (emit-load-env fc s)
        (emit fc 'mkenv (length capt))
        (emit fc 'lset envslot)
        (setf (aref s2 1) envslot)
        (setf (aref s2 2) (cons id (aref s 2)))
        (dolist (v capt)
          (emit fc 'lref (nth (%position v vars) slots))
          (emit fc 'lref envslot)
          (emit fc 'envset 0 idx)
          (scope-add s2 (list v :env id idx))
          (setq idx (1+ idx)))))
    (let ((vs vars) (ss slots))
      (while vs
        (cond ((memq (car vs) specials)
               (emit fc 'lref (car ss))
               (emit fc 'bind (add-const fc (car vs)))
               (scope-add s2 (list (car vs) :special))
               (setq nspecial (1+ nspecial)))
              ((memq (car vs) capt) nil)
              (t (scope-add s2 (list (car vs) :local (car ss)))))
        (setq vs (cdr vs) ss (cdr ss))))
    (cons s2 nspecial)))

(defun comp-function (f fc s)
  (cond ((symbolp f) (emit fc 'gfunc (add-const fc f)))
        ((and (consp f) (eq (car f) 'lambda)) (comp-lambda nil (cadr f) (cddr f) nil fc s))
        (t (error "Bad function" f))))

;; Emit code that creates a function object for (lambda ll . body).
(defun comp-lambda (name ll body macrop fc s)
  (let* ((outer (remove-if (lambda (e) (eq (cadr e) :local)) (aref s 0)))
         (code (compile-function name ll body outer (aref s 2) macrop)))
    (if (null (aref s 2))
        (emit fc 'const (add-const fc code))
        (progn (emit-load-env fc s)
               (emit fc 'mkclosure (add-const fc code))))))

;; => (required optional rest keys); optional and keys as (var default)
;; => (required optional rest keys aux); optional as (var default supplied-p),
;; keys as (var default keyword supplied-p), aux as (var init)
(defun parse-ll (ll)
  (let ((req nil) (opt nil) (rest nil) (keys nil) (aux nil) (state 0))
    (dolist (p ll)
      (cond ((eq p '&optional) (setq state 1))
            ((memq p '(&rest &body)) (setq state 2))
            ((eq p '&key) (setq state 3))
            ((eq p '&aux) (setq state 4))
            ((eq p '&allow-other-keys) nil)
            ((= state 0) (push p req))
            ((= state 1) (push (if (consp p) (list (car p) (cadr p) (caddr p)) (list p nil nil)) opt))
            ((= state 2) (setq rest p))
            ((= state 4) (push (if (consp p) (list (car p) (cadr p)) (list p nil)) aux))
            (t (let* ((spec (if (consp p) (car p) p))
                      (var (if (consp spec) (cadr spec) spec))
                      (kw (if (consp spec) (car spec) (intern (string-append ":" (symbol-name spec))))))
                 (push (list var (if (consp p) (cadr p) nil) kw (if (consp p) (caddr p) nil)) keys)))))
    (list (nreverse req) (nreverse opt) rest (nreverse keys) (nreverse aux))))

;; Keyword arguments come in as a &rest list. Wrap BODY in bindings that look
;; each key up, evaluating its default only when the key is missing:
;;   (let ((#:v (%getf-key rest :key))) (let ((key (if (eq #:v '%none) default #:v))) ...))
;; The forms made here are already in the core language.
(defun key-bindings (rest-var keys body)
  (if (null keys)
      body
      (let* ((var (car (car keys)))
             (default (cadr (car keys)))
             (kw (caddr (car keys)))
             (svar (cadddr (car keys)))
             (tmp (gensym))
             (inner (cons 'let (cons (list (list var (list 'if (list 'eq tmp ''%none) default tmp)))
                                     (key-bindings rest-var (cdr keys) body)))))
        (list (list 'let (list (list tmp (list '%getf-key rest-var (list 'quote kw))))
                    (if svar
                        (list 'let (list (list svar (list 'not (list 'eq tmp ''%none)))) inner)
                        inner))))))

;; optional parameters with a supplied-p variable: the parameters' defaults are
;; a marker, and the body is wrapped in bindings that test for it (M13)
(defun some-supplied-p (opt)
  (let ((r nil)) (dolist (o opt) (when (caddr o) (setq r t))) r))

(defun optional-supplied-bindings (opt body)
  (if (null opt)
      body
      (let* ((o (car opt)) (inner (optional-supplied-bindings (cdr opt) body))
             (missing (list 'eq (car o) ''%unsupplied))
             (set (cons 'let (cons (list (list (car o) (list 'if missing (cadr o) (car o)))) inner))))
        (list (if (caddr o)
                  (list 'let (list (list (caddr o) (list 'not missing))) set)
                  set)))))

(defun aux-bindings (aux body)
  (if (null aux) body (list (cons 'let (cons (list (car aux)) (aux-bindings (cdr aux) body))))))

(defun %getf-key (plist key)
  (while (and plist (not (eq (car plist) key))) (setq plist (cddr plist)))
  (if plist (cadr plist) '%none))

;; Compile a function. LENV holds the outer variables it can see (all in
;; environment vectors or special); CHAIN describes the environment it will
;; be closed over.
(defun compile-function (name ll body lenv chain macrop)
  (let* ((fc (make-fc name))
         (parsed (parse-ll ll))
         (req (car parsed))
         (opt-given (cadr parsed))
         ;; with a supplied-p variable anywhere, all defaults are worked out in the
         ;; body, in order, so each sees the parameters before it
         (late (some-supplied-p opt-given))
         (opt (if late (mapcar (lambda (o) (list (car o) ''%unsupplied)) opt-given) opt-given))
         (keys (cadddr parsed))
         (rest (or (caddr parsed) (and keys (gensym))))
         (body (aux-bindings (car (cddddr parsed)) body))
         (body (if keys (key-bindings rest keys body) body))
         (body (if late (optional-supplied-bindings opt-given body) body))
         (params (append req (mapcar #'car opt) (if rest (list rest) nil)))
         (np (length params))
         (s (make-scope lenv :cenv chain)))
    (when (> (+ (length req) (length opt)) 255) (error "Too many parameters in" name))
    (setf (aref fc 3) np)
    (setf (aref fc 4) np)
    (let ((i 0))
      (dolist (p params) (scope-add s (list p :local i)) (setq i (1+ i))))
    ;; defaults for missing optional arguments
    (let ((i (length req)))
      (dolist (o opt)
        (let ((skip (make-label)))
          (emit fc 'optarg i skip)
          (comp (cadr o) fc s nil)
          (emit fc 'lset i)
          (emit-label fc skip))
        (setq i (1+ i))))
    (let* ((r (bind-vars params (iota-list np) (append (mapcar #'cadr opt) body) fc
                         (make-scope lenv :cenv chain)))
           (s2 (car r))
           (nspecial (cdr r)))
      (if (= nspecial 0)
          (comp-body body fc s2 t)
          (progn (comp-body body fc s2 nil)
                 (emit fc 'unbind nspecial)
                 (emit fc 'ret))))
    (%make-code (assemble fc)
                (list->vector (reverse (aref fc 1)))
                name
                (+ (length req)
                   (* 256 (length opt))
                   (if rest 65536 0)
                   (if macrop 131072 0)
                   (* 262144 (aref fc 4))))))

;;; ---- pass 3: assembly ----------------------------------------------------------------

(defun operand-size (fmt)
  (let ((n 0) (i 0))
    (while (< i (length fmt))
      (setq n (+ n (if (eq (aref fmt i) #\b) 1 2)) i (1+ i)))
    n))

(defun assemble (fc)
  (let ((instrs (reverse (aref fc 0))) (addr 0) (bytes nil))
    (dolist (in instrs)                 ; pass 1: label addresses
      (if (eq (car in) :label)
          (setf (cadr in) addr)
          (setq addr (+ addr 1 (operand-size (get (car in) 'operands))))))
    (setq addr 0)
    (dolist (in instrs)                 ; pass 2: bytes
      (unless (eq (car in) :label)
        (let* ((fmt (get (car in) 'operands))
               (end (+ addr 1 (operand-size fmt)))
               (ops (cdr in))
               (i 0))
          (push (get (car in) 'opcode) bytes)
          (while (< i (length fmt))
            (let ((c (aref fmt i)) (v (car ops)))
              (cond ((eq c #\b) (push (mod v 256) bytes))
                    ((eq c #\k)
                     (when (> v 65535) (error "Too many constants in" (aref fc 5)))
                     (push (mod v 256) bytes)
                     (push (%quotient v 256) bytes))
                    (t (let ((off (- (cadr v) end)))
                         (when (or (< off -32768) (> off 32767)) (error "Function too large:" (aref fc 5)))
                         (when (< off 0) (setq off (+ off 65536)))
                         (push (mod off 256) bytes)
                         (push (%quotient off 256) bytes)))))
            (setq ops (cdr ops) i (1+ i)))
          (setq addr end))))
    (nreverse bytes)))

;;; ---- entry points -------------------------------------------------------------------

;; Compile FORM as the body of a function of no arguments.
(defun compile-toplevel (form)
  (compile-function '%top-level-form nil (list (mx form nil)) nil nil nil))

(defun %toplevel-eval (form)
  (funcall (compile-toplevel form)))

;; Compile the interpreted definition of SYM in place.
(defun compile (sym)
  (let ((f (symbol-function sym)))
    (when (%interpreted-function-p f)
      (let ((parts (%closure-parts f)))   ; (lambda-list body env name macrop)
        (if (caddr parts)
            (error "Cannot compile a closure over an interpreted environment:" sym)
            (fset sym (compile-function sym (mx-ll (car parts) nil) (mx-list (cadr parts) nil)
                                        nil nil (car (cddddr parts)))))))
    sym))

(defun cddddr (x) (cdr (cdddr x)))

(defun compile-all ()
  (let ((n 0))
    (dolist (s (%all-symbols))
      (when (and (fboundp s) (%interpreted-function-p (symbol-function s))
                 (null (caddr (%closure-parts (symbol-function s)))))
        (compile s)
        (setq n (1+ n))))
    n))

;;; ---- disassembler --------------------------------------------------------------------

(defun disassemble (f)
  (when (symbolp f) (setq f (symbol-function f)))
  (let* ((parts (%code-parts f))
         (bytes (list->vector (car parts)))
         (consts (cadr parts))
         (pc 0))
    (princ "; ") (prin1 f) (terpri)
    (while (< pc (length bytes))
      (let* ((entry (nth (aref bytes pc) *opcodes*))
             (fmt (cadr entry))
             (end (+ pc 1 (operand-size fmt)))
             (k (1+ pc))
             (i 0))
        (princ "  ") (princ pc) (princ (if (< pc 10) "   " (if (< pc 100) "  " " ")))
        (princ (car entry))
        (while (< i (length fmt))
          (let ((c (aref fmt i)))
            (princ " ")
            (cond ((eq c #\b) (princ (aref bytes k)) (setq k (1+ k)))
                  ((eq c #\k)
                   (let ((v (+ (aref bytes k) (* 256 (aref bytes (1+ k))))))
                     (princ v) (princ "  ; ") (prin1 (aref consts v)))
                   (setq k (+ k 2)))
                  (t (let ((v (+ (aref bytes k) (* 256 (aref bytes (1+ k))))))
                       (when (> v 32767) (setq v (- v 65536)))
                       (princ "-> ") (princ (+ end v)))
                     (setq k (+ k 2)))))
          (setq i (1+ i)))
        (terpri)
        (setq pc end))))
  nil)

;;; ---- bootstrap -------------------------------------------------------------------------

;; Compile everything defined so far, including this compiler (it is running
;; interpreted right now, and switches to its compiled self as it goes).
(defvar *bootstrap-compiled* (compile-all))

;; From here on the compiler evaluates every top-level form.
(defun eval (form) (funcall (compile-toplevel form)))
(%set-toplevel-evaluator #'%toplevel-eval)
(compile 'eval)      ; defined while the interpreter still evaluated: compile it too

;;; ---- control structure that needs the compiler -------------------------------------------

(defun throw (tag value) (throw tag value))

(defmacro return (&optional value) `(return-from nil ,value))

(defmacro dolist (spec &body body)
  (let ((tail (gensym)))
    `(block nil
       (let ((,tail ,(cadr spec)) (,(car spec) nil))
         (while ,tail
           (setq ,(car spec) (car ,tail))
           ,@body
           (setq ,tail (cdr ,tail)))
         ,(caddr spec)))))

(defmacro dotimes (spec &body body)
  (let ((limit (gensym)))
    `(block nil
       (let ((,limit ,(cadr spec)) (,(car spec) 0))
         (while (< ,(car spec) ,limit)
           ,@body
           (setq ,(car spec) (1+ ,(car spec))))
         ,(caddr spec)))))

(defmacro loop (&body body)
  `(block nil (while t ,@body)))

(defmacro ignore-errors (&body body)
  `(handler-case (progn ,@body) (error (c) (values nil c))))

;;; ---- the handler stack (M13; signals.lisp has the condition classes) -------------

(defvar *handler-clusters* nil)   ; innermost first: each a list of (type . function)

(defmacro handler-bind (bindings &body body)
  `(let ((*handler-clusters*
           (cons (list ,@(mapcar (lambda (b) `(cons ',(car b) ,(cadr b))) bindings))
                 *handler-clusters*)))
     ,@body))

;; the object handlers see: what the program signalled, or the kernel's condition
(defun %condition-object (c) (or (%condition-data c) c))

;; replaced in signals.lisp, once there are condition classes
(defun %condition-matches-p (object type)
  (or (memq type '(t condition error serious-condition))
      (and (conditionp object) (eq (condition-type object) type))))

;; Call the handlers for OBJECT, innermost first. A handler runs with only
;; the handlers outside its own cluster established; one that returns
;; declines. While the types are being matched no handlers are established,
;; so an error in here goes straight to the debugger.
(defun %run-handlers (object)
  (let ((clusters *handler-clusters*))
    (let ((*handler-clusters* nil))
      (while clusters
        (let ((cluster (car clusters)))
          (setq clusters (cdr clusters))
          (dolist (h cluster)
            (when (%condition-matches-p object (car h))
              (let ((*handler-clusters* clusters))
                (funcall (cdr h) object)))))))
    nil))

;; the kernel calls this for an error while handlers are established
(defun %signal-hook (c)
  (%run-handlers (%condition-object c))
  (%unhandled c))

(defmacro prog2 (first second &body body)
  `(progn ,first (prog1 ,second ,@body)))
