(in-package :paip)

(defmacro run-it (expr)
  `(format t "~a => ~a" (quote ,expr) ,expr))

;; All the def* forms define global variables. Use `let` or `labels`
;; to create stuff that's local.

;; For the purpose of conditionals, everything except `nil` is
;; considered true. Empty list is nil.

(if '() (print 'true) (print 'false))
(if '(1) (print 'true) (print 'false))

(let* ((x 6)
       (y 12))
  (+ x y))

((lambda (x y)
   (+ x y))
 6 12) 

;; Ex 3.1
(let* ((x 6)
       (y (* x x)))
  (+ x y))

((lambda (x)
   ((lambda ()
        (+ x (* x x)))))
 6)

;; OR

((lambda (x)
   ((lambda (y)
      (+ x y))
    (* x x)))
 6)

;; INCF, DECF, PUSH, POP are careful enough to only expand the complex
;; 'place' only once during the operation and reuse the value
;; throughout the calculation.

(defstruct player
  (score 0) (wins 0))


;; Converts to the following (sbcl specific) code:

;; (LET ((P (MAKE-PLAYER :SCORE 1 :WINS 0)))
;;   (LET* ((#:OBJ 1)
;;          (#:NEW1 (+ 1 (SB-KERNEL:%INSTANCE-REF (THE PLAYER #:OBJ) 1))))
;;     (SB-KERNEL:%INSTANCE-SET (THE PLAYER #:OBJ) 1 #:NEW1)))

(defvar *my-list* '(1 2 3 4 5))

(defun length1 (list)
  (let ((len 0))
    (dolist  (a list)
      (incf len))
    len))

(run-it (length1 *my-list*))

;; mapc is used for the side-effects. For returning the list, use
;; mapcar.
(defun length2 (list)
  (let ((len 0))
    (mapc #'(lambda (a) (incf len))
          list)
    len))

(run-it (length2 *my-list*))

(defun length4 (list)
  (loop for i in list
     count i))

(run-it (length4 *my-list*))

(defun length7 (list)
  (count-if #'(lambda (a) t) list))

(defun length9 (list)
  (if (null list)
      0
      (1+ (length9 (rest list)))))

(run-it (length9 *my-list*))

(defun length11 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (length11 (rest list) (1+ len-so-far))))

(defun length12 (the-list)
  (labels
      ((length-12-aux (list len-so-far)
         (if (null list)
             len-so-far
             (length-12-aux (rest list) (1+ len-so-far)))))
    (length-12-aux the-list 0)))

(defun product (numbers)
  (let ((prod 1))
    (dolist (n numbers prod)
      (if (= n 0)
          (RETURN 0)
          (setf prod (* n prod))))))

(run-it (product '(1 2 3 4)))
(run-it (product '(1 0 3 4)))

(defmacro while0 (test &rest body)
  "Repeat body while the test is true"
  (list* 'loop
         (list 'unless test '(return nil))
         body))

(macroexpand-1 '(while0 (< i 10)
                 (print (* i i))
                 (setf i (+ i 1))))

(defmacro while1 (test &rest body)
  (let ((code '(loop (unless test (return nil)) . body)))
    (subst test 'test (subst body 'body code))))

(macroexpand-1 '(while1 (< i 10)
           (print (* i i))
                 (setf i (+ i 1))))

(defmacro while (test &rest body)
  `(loop (unless ,test (return nil))
      ,@body))

(macroexpand-1 '(while (< i 10)
           (print (* i i))
                 (setf i (+ i 1))))




