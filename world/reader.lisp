;;; -*- Mode: Lisp -*-
;;; reader.lisp -- what the reader leaves to Lisp (M16): #S(...) structures,
;;; #n= and #n# labels for shared and circular structure, and #. evaluation.
;;;
;;; The kernel's reader (kernel/read.c) cannot call Lisp, so it reads these
;;; as marker vectors, #(%READ-STRUCT (name slot value ...) NIL),
;;; #(%READ-LABEL n object), #(%READ-REF n NIL) and #(%READ-EVAL form NIL),
;;; and says so (%READ-FIXUPS-P). %READ-FIXUP then makes the real objects:
;;; first, bottom up, the structures and #. values, noting each label's
;;; object; then it puts each labelled object where a #n# stands for it.
;;; %READ-FROM-STRING (under READ, READ-FROM-STRING, the Listener and the
;;; editor) and the kernel's REPL (through %READ-FIXUP-EVAL) both use it.

(defvar *read-eval* t)          ; NIL: #. is an error
(defvar *%fixup-pending* nil)   ; pass 1 met a #n# before its #n=

(defun %read-marker-p (x)
  (and (simple-vector-p x) (= (length x) 3)
       (memq (aref x 0) '(%read-struct %read-label %read-ref %read-eval %read-pathname))))

;; a structure from #S(NAME :SLOT VALUE ...), by its constructor
(defun %read-structure (spec)
  (let* ((name (car spec))
         (initargs (let ((r nil) (l (cdr spec)))
                     (while l
                       (push (intern (string (car l)) (find-package "KEYWORD")) r)   ; any symbol names the slot
                       (push (cadr l) r)
                       (setq l (cddr l)))
                     (nreverse r)))
         (constructor (get name 'struct-constructor)))
    (cond (constructor (apply constructor initargs))
          ((get name 'struct-slots) (apply #'make-instance name initargs))
          (t (error "#S: not a structure type:" name)))))

;; pass 1: the structures and #. values, bottom up; labels into LABELS
(defun %fixup-build (x labels)
  (cond ((%read-marker-p x)
         (let ((a (aref x 1)))
           (case (aref x 0)
             (%read-label (let ((v (%fixup-build (aref x 2) labels)))
                            (setf (gethash a labels) v)
                            v))
             (%read-ref (multiple-value-bind (v found) (gethash a labels)
                          (if found v (progn (setq *%fixup-pending* t) x))))   ; not yet known: pass 2
             (%read-struct (%read-structure (%fixup-build a labels)))
             (%read-pathname (parse-namestring a))                  ; #P"..." (M18, cl4.lisp)
             (%read-eval (if *read-eval*
                             (eval (%fixup-build a labels))
                             (error "#. is not allowed: *READ-EVAL* is NIL"))))))
        ((consp x)
         (let ((c x))
           (loop
             (setf (car c) (%fixup-build (car c) labels))
             (cond ((%read-marker-p (cdr c)) (setf (cdr c) (%fixup-build (cdr c) labels)) (return nil))
                   ((consp (cdr c)) (setq c (cdr c)))
                   (t (return nil)))))
         x)
        ((simple-vector-p x)
         (dotimes (i (length x)) (setf (aref x i) (%fixup-build (aref x i) labels)))
         x)
        ((and (%array-header-p x) (simple-vector-p (%array-field x ar-data)))
         (%fixup-build (%array-field x ar-data) labels)
         x)
        (t x)))

;; pass 2: #n# that stood before its label was known
(defun %fixup-target (x labels)
  (multiple-value-bind (v found) (gethash (aref x 1) labels)
    (unless found (error "#n# with no #n= for it:" (aref x 1)))
    v))

(defun %fixup-refs (x labels seen)
  (cond ((and (%read-marker-p x) (eq (aref x 0) '%read-ref)) (%fixup-target x labels))
        ((gethash x seen) x)
        ((consp x)
         (let ((c x))
           (loop
             (setf (gethash c seen) t)
             (let ((a (car c)))
               (cond ((and (%read-marker-p a) (eq (aref a 0) '%read-ref)) (setf (car c) (%fixup-target a labels)))
                     ((or (consp a) (simple-vector-p a)) (%fixup-refs a labels seen))))
             (let ((d (cdr c)))
               (cond ((and (%read-marker-p d) (eq (aref d 0) '%read-ref))
                      (setf (cdr c) (%fixup-target d labels)) (return nil))
                     ((and (consp d) (not (gethash d seen))) (setq c d))
                     ((simple-vector-p d) (%fixup-refs d labels seen) (return nil))
                     (t (return nil))))))
         x)
        ((instancep x)                            ; its slots, not its class
         (setf (gethash x seen) t)
         (%fixup-refs (instance-slots x) labels seen)
         x)
        ((simple-vector-p x)
         (setf (gethash x seen) t)
         (let ((i 0))
           (while (< i (length x))
             (setf (aref x i) (%fixup-refs (aref x i) labels seen))
             (setq i (1+ i))))
         x)
        (t x)))

(defun %read-fixup (x)
  (let* ((labels (make-hash-table))
         (*%fixup-pending* nil)
         (r (%fixup-build x labels)))
    (if *%fixup-pending*
        (%fixup-refs r labels (make-hash-table :test #'eq))
        r)))

;; the kernel's REPL evaluates a form with markers in it this way
(defun %read-fixup-eval (form) (eval (%read-fixup form)))

(fset '%raw-read-from-string (symbol-function '%read-from-string))

(defun %read-from-string (string &optional (start 0))
  (let ((r (%raw-read-from-string string start)))
    (if (and (consp r) (%read-fixups-p))
        (cons (%read-fixup (car r)) (cdr r))
        r)))
