(in-package :paip)

(defun fib (n)
  "Compute the nth number in Fib sequence"
  (if (<= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2)))))

(defun memo (fn)
  "Return a memo-function of fn"
  (let ((table (make-hash-table)))
    #'(lambda (x)
        (multiple-value-bind (val found-p)
            (gethash x table)
          (if found-p
              val
              (setf (gethash x table) (funcall fn x)))))))


(defun memoize (fn-name)
  "Replace fn-name's global definition with a memoized version."
  (setf (symbol-function fn-name) (memo (symbol-function fn-name))))


(defun multiple-value-memo (fn name key test)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'multiple-value-memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p
                val
                (setf (gethash k table) (apply fn args))))))))



(defun sum (a b)
  (declare (fixnum a b))
  (+ a b))
