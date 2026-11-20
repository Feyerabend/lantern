;;; errors.lisp — each form below must signal an error and leave the system
;;; usable. The runner counts errors; make test expects exactly ERRORS-EXPECTED.

(car 5)
(undefined-function-xyz 1)
unbound-variable-xyz
(+ 1 'a)
(/ 1 0)
(setq t 5)
((lambda (x) x))
((lambda (x) x) 1 2)
(funcall 42)
(defun runaway (n) (+ 1 (runaway n)))
(runaway 0)
(parse-integer "12x")
(error "Custom error" 1 "two" 'three)
(make-vector -1)
(aref #(1 2) 5)
(if)
(let ((x)) . 5)
