;;; gc.lisp — allocate a lot and check that nothing live is lost or corrupted.
;;; Run it with a small heap (make test does) so collections happen often.

(defun iota (n)
  (let ((result nil))
    (dotimes (i n) (push i result))
    (nreverse result)))

(defun sum (list) (reduce #'+ list))

;; a long-lived structure that must survive every collection
(defvar *keep* (mapcar (lambda (i) (list i (prin1-to-string i) (vector i (* i i)))) (iota 2000)))

(defun keep-ok ()
  (let ((ok t) (i 0))
    (dolist (e *keep*)
      (unless (and (= (car e) i)
                   (string= (cadr e) (prin1-to-string i))
                   (= (aref (caddr e) 1) (* i i)))
        (setq ok nil))
      (setq i (1+ i)))
    ok))

;; lots of garbage
(dotimes (round 20)
  (check (sum (iota 5000)) 12497500)
  (check (keep-ok) t))

;; many closures kept alive in a list
(defvar *adders* (mapcar (lambda (n) (lambda (x) (+ x n))) (iota 1000)))
(gc)
(check (sum (mapcar (lambda (f) (funcall f 1)) *adders*)) 500500)

;; strings
(defun many-strings (n)
  (let ((s ""))
    (dotimes (i n) (setq s (string-append s "x")))
    s))
(check (length (many-strings 2000)) 2000)

;; deep structure
(defun tree (d) (if (= d 0) 'leaf (cons (tree (1- d)) (tree (1- d)))))
(defun count-leaves (x) (if (consp x) (+ (count-leaves (car x)) (count-leaves (cdr x))) 1))
(check (count-leaves (tree 14)) 16384)

(check (keep-ok) t)
(report "gc")
