;;; -*- Mode: Lisp -*-
;;; streams.lisp — output streams. All printing goes through here, so output
;;; can go to the host (the terminal, or the browser console), to the Listener
;;; on the screen, or into a string.
;;;
;;; Until there is an object system (M5) a stream is a vector:
;;;   #(:stream write-function fresh-line-function data)
;;; WRITE-FUNCTION is called with the stream and a string, FRESH-LINE-FUNCTION
;;; with the stream.

(defun make-stream (write-fn fresh-line-fn &optional data)
  (vector :stream write-fn fresh-line-fn data))

(defun streamp (x)
  (and (vectorp x) (= (length x) 4) (eq (aref x 0) :stream)))

(defun stream-data (s) (aref s 3))

(defvar *host-stream*
  (make-stream (lambda (s string) (%host-write string))
               (lambda (s) (%host-fresh-line))))

(defvar *standard-output* *host-stream*)

(defun resolve-stream (s)
  (if (or (null s) (eq s t)) *standard-output* s))

;;; ---- the printing functions, now in terms of streams ------------------

(defun write-string (string &optional stream)
  (let ((s (resolve-stream stream)))
    (funcall (aref s 1) s string))
  string)

(defun write-char (char &optional stream)
  (write-string (string char) stream)
  char)

(defvar *newline-string* (string (code-char 10)))

(defun terpri (&optional stream)
  (write-string *newline-string* stream)
  nil)

(defun fresh-line (&optional stream)
  (let ((s (resolve-stream stream)))
    (funcall (aref s 2) s))
  nil)

(defun prin1 (object &optional stream)
  (write-string (prin1-to-string object) stream)
  object)

(defun princ (object &optional stream)
  (write-string (princ-to-string object) stream)
  object)

(defun print (object &optional stream)
  (terpri stream)
  (prin1 object stream)
  (write-string " " stream)
  object)

;;; ---- string streams ------------------------------------------------------

(defun ends-with-newline (string)
  (let ((n (length string)))
    (and (> n 0) (eq (aref string (1- n)) (code-char 10)))))

;; data: (chunks), newest first
(defun make-string-output-stream ()
  (make-stream (lambda (s string) (push string (car (stream-data s))))
               (lambda (s)
                 (let ((chunks (car (stream-data s))))
                   (while (and chunks (= (length (car chunks)) 0)) (setq chunks (cdr chunks)))
                   (when (and chunks (not (ends-with-newline (car chunks))))
                     (push *newline-string* (car (stream-data s))))))
               (list nil)))

(defun get-output-stream-string (s)
  (let ((text (apply #'string-append (reverse (car (stream-data s))))))
    (setf (car (stream-data s)) nil)
    text))

(defmacro with-output-to-string (spec &body body)
  (let ((var (car spec)))
    `(let ((,var (make-string-output-stream)))
       ,@body
       (get-output-stream-string ,var))))

;;; ---- system information ----------------------------------------------------

(defun room ()
  (let ((s (%heap-stats)))
    (fresh-line)
    (princ "Heap: ") (princ (first s)) (princ " of ") (princ (second s))
    (princ " words in use (") (princ (%quotient (* 100 (first s)) (second s))) (princ "%), ")
    (princ (third s)) (princ " collections, ") (princ (fourth s)) (princ " evaluator steps.")
    (terpri))
  nil)
