(in-package :paip)

;; (pat-match '(?x . ?y) '(2 . 1)) ; ((?y . 2) (?x . 1))

;; (unify '(?x . 2) '(1 . ?y)) ; ((?y . 2) (?x . 1))
;; (unify '(f . ?y) '(f . ?x)) ; ((?y . ?x))

(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
  "Indicates pat-match success, with no variables.")

(defun variable-p (x)
  "Is x a variable (a symbol begining with ?x)"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in the binding list"
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding"
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list"
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to the binding list"
  (cons (cons var val)
        (if (and (eq bindings no-bindings))
            nil
            bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and return bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((eql x y) bindings)
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) bindings)))
        (t fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings"
  (if (get-binding var bindings)
      (unify (lookup var bindings) x bindings)
      (extend-bindings var x bindings)))
