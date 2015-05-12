(defpackage :toad
  (:use :cl))

(in-package :toad)

(defvar *empty-stream* '())

(defun lazy-empty? (str)
  (eq str *empty-stream*))

(defun memo-proc (proc)
  (let ((already-run? nil)
        (result nil))
    (lambda ()
      (if (not already-run?)
          (progn (setf result (funcall proc))
                 (setf already-run? T)
                 result)
          result))))

(defmacro delay (exp)
  `(memo-proc (lambda () ,exp)))

(defun force (delayed)
  (funcall delayed))

(defmacro lazy-cons (head tail)
  `(cons ,head (delay ,tail)))

(defun lazy-car (stream)
  (car stream))

(defun lazy-cdr (stream)
  (force (cdr stream)))

(defun lazy-range (low high)
  (if (> low high)
      *empty-stream*
      (lazy-cons low (lazy-range (1+ low) high))))

(defun lazy-nth (str n)
  (if (= n 0)
      (lazy-car str)
      (lazy-nth (lazy-cdr str) (- n 1))))

(defun lazy-take (str n)
  (if (or (= n 0) (lazy-empty? str))
      *empty-stream*
      (lazy-cons (lazy-car str)
                 (lazy-take (lazy-cdr str) (1- n)))))

(defun lazy-reduce2 (fn init &rest strs)
  (if (some #'lazy-empty? strs)
      init
      (funcall #'lazy-reduce2 fn
               (funcall fn init (mapcar #'lazy-car strs))
               (mapcar #'lazy-cdr strs))))

(defun lazy-reduce (fn init str)
  (if (lazy-empty? str)
      init
      (lazy-reduce fn
                   (funcall fn init (lazy-car str))
                   (lazy-cdr str))))

(defun lazy-map (fn &rest strs)
  (if (some #'lazy-empty? strs)
      *empty-stream*
      (lazy-cons (apply fn (mapcar #'lazy-car strs))
                 (apply #'lazy-map fn (mapcar #'lazy-cdr strs)))))

(defun lazy-filter (fn str)
  (cond ((lazy-empty? str) *empty-stream*)
        ((funcall fn (lazy-car str))
         (lazy-cons (lazy-car str)
                    (lazy-filter fn (lazy-cdr str))))
        (T (lazy-filter fn (lazy-cdr str)))))

(defun lazy-foreach (fn str)
  (unless (lazy-empty? str)
    (progn
      (funcall fn (lazy-car str))
      (lazy-foreach fn (lazy-cdr str)))))

(defun print-seq (str)
  (lazy-foreach #'print str))

(defun show (val)
  (print val)
  val)

(defun integers-starting (n)
  (lazy-cons n (integers-starting (1+ n))))

(defvar integers (integers-starting 1))

(defun divisible? (x y)
  (= (rem x y) 0))

(defun sieve (str)
  (lazy-cons
   (lazy-car str)
   (sieve (lazy-filter #'(lambda (x) (not (divisible? x (lazy-car str))))
                       (lazy-cdr str)))))

(defvar primes (sieve (integers-starting 2)))

(defun list->lazy (lst)
  (if lst
      (lazy-cons (car lst) (list->lazy (cdr lst)))
      *empty-stream*))
