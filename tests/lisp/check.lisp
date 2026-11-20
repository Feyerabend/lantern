;;; check.lisp — a tiny test harness. Load it before the test files.
;;; (check FORM EXPECTED) signals an error when FORM's value is not EQUAL to
;;; EXPECTED; the native runner exits with status 1 if any error happened.

(defvar *checks* 0)

(defmacro check (form expected)
  `(let ((got ,form) (want ,expected))
     (setq *checks* (1+ *checks*))
     (if (equal got want)
         t
         (error "Check failed:" ',form "gave" got "expected" want))))

(defun report (name)
  (princ name) (princ ": ") (princ *checks*) (princ " checks run") (terpri)
  (setq *checks* 0))
