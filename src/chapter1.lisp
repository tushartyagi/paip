(in-package :paip)

(declaim (optimize (debug 3)))

;; The distinction between a user and a programmer is that a user
;; provides new input or data and the programmer defines new
;; operations, programs, as well as data.

;; Symbolic Computation: Using symbols as objects of computation.
;; There are 2 elements defined below, one is a symbol and another is
;; a list of 2 symbols.

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General Prof MD))

(defun first-name (name)
  "Find the first name from a list representation of a name, ignoring titles"
  (let ((first (first name)))
    (if (member first *titles*)
        (first-name (rest name))
        (first name))))

(defun last-name (name)
  "Find the last name from a list representation of a name"
  (first (last name)))

(defun mappend (fn the-list)
  "Apply function to each element of the list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun self-and-double (x)
  (list x (+ x x)))

(defun number-and-negation (x)
  (if (numberp x)
      (list x (- x))
      nil))

(defun numbers-and-negation (list)
  "Given a list, return only their numbers and negations"
  (mappend #'number-and-negation list))

;; A list is evaluated in one of two ways, either the first element is
;; name of a special form, then it is evaluated as per the rules of
;; that special form. Otherwise, it has to be a function in which case
;; the first element can be either a symbol or a lambda
;; expression. Function in any other position than the first has to be
;; used with #' notation.

;; Exercises

(defun last-element (list)
  (first (last list)))

(defun last-name (name)
  (first-name (reverse name)))

;; Will break for negative powers.
(defun power-without-negative (base exp)
  (if (= exp 0)
      1
      (* base (power base (1- exp)))))

(defun count-atoms (list)
  (cond ((null list) 0)
        ((atom list) 1)
        ((not (listp (cdr list))) (+ (count-atoms (car list)) 1)) ;; dotted pair
        ((listp list) (apply #'+ (mapcar #'count-atoms list)))))

(defun dot-product (a b)
  (apply #'+ (mapcar #'* a b)))
