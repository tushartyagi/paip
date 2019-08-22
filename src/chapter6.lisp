(in-package :paip)

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((variable-p pattern))
        ((constant-p pattern))
        ((segment-pattern-p pattern))
        ((single-pattern-p pattern))
        ((consp pattern))))

(defun variable-p (pattern)
  (and (symbolp pattern) (equal (char (symbol-name pattern) 0) #\?)))
