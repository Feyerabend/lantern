;;; -*- Mode: Lisp -*-
;;; packages.lisp — the package system (M12), on the primitives in
;;; kernel/package.c.
;;;
;;; A package is #(:package name nicknames table use-list). COMMON-LISP (also
;;; CL, LISP, SYSTEM) is the world itself: every symbol of the system lives
;;; there and counts as external, so any package that uses it sees them all.
;;; KEYWORD holds the keywords, and COMMON-LISP-USER (CL-USER, USER), which
;;; uses COMMON-LISP, is where the Listener starts.
;;;
;;; The reader interns in *PACKAGE* and reads PKG:NAME and PKG::NAME; the
;;; printer writes the prefix when *PACKAGE* does not see the symbol.

(%define-class 'package '(t) nil)
(push '(:package . package) *tag-classes*)

(defun packagep (x)
  (and (vectorp x) (= (length x) 6) (eq (aref x 0) :package)))

;; a string designator's name; a keyword's name without its colon
(defun %designator-name (x)
  (cond ((stringp x) x)
        ((characterp x) (string x))
        ((symbolp x) (symbol-name x))           ; a keyword's name has no colon (M17)
        (t (error "Not a name (a string or a symbol):" x))))

(defun find-package (name)
  (if (packagep name) name (%find-package (%designator-name name))))

(defun %package (designator)
  (or (find-package designator) (error "No package named" (%designator-name designator))))

(defun package-name (p) (aref (%package p) 1))
(defun package-nicknames (p) (aref (%package p) 2))
(defun package-use-list (p) (aref (%package p) 4))
(defun list-all-packages () (copy-list (%package-list)))

(defun package-used-by-list (p)
  (let ((p (%package p)))
    (remove-if-not (lambda (q) (memq p (aref q 4))) (%package-list))))

(defun make-package (name &key nicknames (use '("COMMON-LISP")))
  (%make-package (%designator-name name)
                 (mapcar #'%designator-name nicknames)
                 (mapcar #'%package use)))

(defun rename-package (p new-name &optional new-nicknames)
  (let ((p (%package p)))
    (setf (aref p 1) (%designator-name new-name))
    (setf (aref p 2) (mapcar #'%designator-name new-nicknames))
    p))

(defun %symbols (x) (if (listp x) x (list x)))

;; => the symbol and how it is there: :internal, :external or :inherited
(defun find-symbol (name &optional (package *package*))
  (let ((r (%find-symbol (string name) (%package package))))
    (if r (values (first r) (second r)) (values nil nil))))

(defun intern (name &optional (package *package*))
  (let ((p (%package package)))
    (multiple-value-bind (s status) (find-symbol name p)
      (if status
          (values s status)
          (values (%intern (string name) p) nil)))))


(defun export (symbols &optional (package *package*))
  (let ((p (%package package)))
    (dolist (s (%symbols symbols))
      (multiple-value-bind (found status) (find-symbol (symbol-name s) p)
        (cond ((and status (not (eq found s)))
               (error "EXPORT: another symbol of this name is accessible in the package:" found))
              ((eq p (find-package "COMMON-LISP")) nil)      ; all of its symbols are external
              (t (unless (memq status '(:internal :external)) (%package-add s p))
                 (if (eq (symbol-package s) p)
                     (%set-external s t)
                     ;; a symbol from elsewhere: external here, without touching its home (M17)
                     (unless (memq s (aref p 5)) (setf (aref p 5) (cons s (aref p 5)))))))))
    t))

(defun unexport (symbols &optional (package *package*))
  (%package package)
  (let ((p (%package package)))
    (dolist (s (%symbols symbols))
      (if (eq (symbol-package s) p)
          (%set-external s nil)
          (setf (aref p 5) (remove s (aref p 5) :test #'eq)))))
  t)

(defun import (symbols &optional (package *package*))
  (let ((p (%package package)))
    (dolist (s (%symbols symbols))
      (multiple-value-bind (found status) (find-symbol (symbol-name s) p)
        (cond ((eq found s) (unless (memq status '(:internal :external)) (%package-add s p)))
              ((memq status '(:internal :external))
               (error "IMPORT: the package has another symbol of this name:" found))
              (t (%package-add s p)))))
    t))

;; a symbol of PACKAGE's own, hiding any inherited one of the same name
(defun shadow (names &optional (package *package*))
  (let ((p (%package package)))
    (dolist (n (%symbols names))
      (multiple-value-bind (found status) (find-symbol (%designator-name n) p)
        (unless (memq status '(:internal :external))
          (%package-add (make-symbol (%designator-name n)) p))))
    t))

(defun shadowing-import (symbols &optional (package *package*))
  (let ((p (%package package)))
    (dolist (s (%symbols symbols))
      (multiple-value-bind (found status) (find-symbol (symbol-name s) p)
        (when (and (memq status '(:internal :external)) (not (eq found s)))
          (%package-remove found p))
        (unless (and (eq found s) (memq status '(:internal :external)))
          (%package-add s p))))
    t))

(defun unintern (symbol &optional (package *package*))
  (%package-remove symbol (%package package)))

(defun use-package (packages &optional (package *package*))
  (let ((p (%package package)))
    (dolist (u (%symbols packages))
      (let ((u (%package u)))
        (unless (or (eq u p) (memq u (aref p 4)))
          (setf (aref p 4) (append (aref p 4) (list u))))))
    t))

(defun unuse-package (packages &optional (package *package*))
  (let ((p (%package package)))
    (dolist (u (%symbols packages))
      (setf (aref p 4) (remove (%package u) (aref p 4) :test #'eq)))
    t))

(defun delete-package (package)
  (let ((p (find-package package)))
    (when p
      (when (package-used-by-list p)
        (error "DELETE-PACKAGE: other packages use it:" p))
      (%delete-package p)
      t)))

;; the symbols present in P (at home there, or imported), and, if INHERITED,
;; those it sees through its use list; each once
(defun %package-symbols (p external-only inherited)
  (let ((r nil) (cl (find-package "COMMON-LISP")))
    (if (eq p cl)
        (dolist (s (%all-symbols))
          (when (and (eq (symbol-package s) cl) (not (keywordp s))) (push s r)))
        (let ((table (aref p 3)))
          (when (vectorp table)
            (dotimes (i (length table))
              (dolist (s (aref table i))
                (when (or (not external-only) (and (eq (symbol-package s) p) (%externalp s))
                          (memq s (aref p 5)))
                  (push s r)))))
          (when inherited
            (dolist (u (aref p 4))
              (dolist (s (%package-symbols u t nil))
                (when (eq (find-symbol (symbol-name s) p) s) (push s r)))))))
    r))

(defmacro do-symbols (spec &body body)
  (let ((var (first spec)) (package (or (second spec) '*package*)))
    `(progn (dolist (,var (%package-symbols (%package ,package) nil t)) ,@body)
            ,(third spec))))

(defmacro do-external-symbols (spec &body body)
  (let ((var (first spec)) (package (or (second spec) '*package*)))
    `(progn (dolist (,var (%package-symbols (%package ,package) t nil)) ,@body)
            ,(third spec))))

(defmacro do-all-symbols (spec &body body)
  `(progn (dolist (,(first spec) (%all-symbols)) ,@body) ,(second spec)))

;;; ---- DEFPACKAGE and IN-PACKAGE ------------------------------------------------

(defun %defpackage (name nicknames use exports shadows imports interns)
  (let ((p (or (find-package name)
               (make-package name :nicknames nicknames :use nil))))
    (setf (aref p 2) (mapcar #'%designator-name nicknames))
    (dolist (s shadows) (shadow s p))
    (dolist (spec imports)                    ; (from-package name...)
      (let ((from (%package (car spec))))
        (dolist (n (cdr spec))
          (multiple-value-bind (s status) (find-symbol (%designator-name n) from)
            (unless status (error "DEFPACKAGE :IMPORT-FROM: no symbol" (%designator-name n)))
            (import s p)))))
    (use-package use p)
    (dolist (n interns) (intern (%designator-name n) p))
    (dolist (n exports) (export (intern (%designator-name n) p) p))
    p))

(defmacro defpackage (name &rest options)
  (let ((nicknames nil) (use nil) (use-given nil) (exports nil) (shadows nil) (imports nil) (interns nil))
    (dolist (o options)
      (case (car o)
        (:nicknames (setq nicknames (append nicknames (cdr o))))
        (:use (setq use (append use (cdr o)) use-given t))
        (:export (setq exports (append exports (cdr o))))
        (:shadow (setq shadows (append shadows (cdr o))))
        (:import-from (push (cdr o) imports))
        (:intern (setq interns (append interns (cdr o))))
        ((:documentation :size :local-nicknames) nil)
        (t (error "DEFPACKAGE: unknown option" (car o)))))
    `(%defpackage ',(%designator-name name)
                  ',(mapcar #'%designator-name nicknames)
                  ',(if use-given (mapcar #'%designator-name use) '("COMMON-LISP"))
                  ',(mapcar #'%designator-name exports)
                  ',(mapcar #'%designator-name shadows)
                  ',(mapcar (lambda (i) (mapcar #'%designator-name i)) (reverse imports))
                  ',(mapcar #'%designator-name interns))))

(defmacro in-package (name)
  `(setq *package* (%package ',(%designator-name name))))

;; for the Listener's prompt and the who-line
(defun package-prompt-name (p)
  (let ((names (cons (package-name p) (package-nicknames p))))
    (reduce (lambda (a b) (if (< (length b) (length a)) b a)) names)))
