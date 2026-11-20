;;; bench.lisp — the same functions run interpreted and compiled.
;;; ./build/lisp tests/lisp/bench.lisp

(defun time-it (name thunk)
  (let ((start (get-internal-real-time)))
    (funcall thunk)
    (let ((ms (- (get-internal-real-time) start)))
      (princ name) (princ ": ") (princ ms) (princ " ms") (terpri)
      ms)))

(defvar *sources*
  '((defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
    (defun tak (x y z) (if (not (< y x)) z (tak (tak (1- x) y z) (tak (1- y) z x) (tak (1- z) x y))))
    ;; WHILE rather than DOLIST: DOLIST now expands into BLOCK, which only the
    ;; compiler implements
    (defun build-list (n) (let ((l nil) (i 0)) (while (< i n) (setq l (cons i l) i (1+ i))) l))
    (defun sum-list (l) (let ((s 0)) (while l (setq s (+ s (car l)) l (cdr l))) s))
    (defun double-all (l) (let ((r nil)) (while l (setq r (cons (* 2 (car l)) r) l (cdr l))) r))
    (defun list-work () (sum-list (double-all (build-list 200000))))))

(defun run-all ()
  (+ (time-it "  fib 25      " (lambda () (fib 25)))
     (time-it "  tak 18 12 6 " (lambda () (tak 18 12 6)))
     (time-it "  list work   " (lambda () (list-work)))))

(princ "Interpreted:") (terpri)
(dolist (s *sources*) (%interpret s))
(defvar *interpreted* (run-all))

(princ "Compiled:") (terpri)
(dolist (s *sources*) (eval s))
(defvar *compiled* (run-all))

(let ((tenths (/ (* 10 *interpreted*) (max 1 *compiled*))))
  (princ "Speed-up: ") (princ (/ tenths 10)) (princ ".") (princ (mod tenths 10)) (princ "x") (terpri))
