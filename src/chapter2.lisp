(in-package :paip)

;; https://www.cs.rochester.edu/~nelson/courses/csc_173/grammars/cfg.html
;; Context Free Grammer consists of:
;; 1. Terminal Symbols
;; 2. Non terminal symbols
;; 3. Rules of generation
;; 4. Starting Rule(s).

(defun create-simple-sentence ()
  (defun sentence () (append (noun-phrase) (verb-phrase)))
  (defun noun-phrase () (append (article) (noun)))
  (defun verb-phrase () (append (verb) (noun-phrase)))
  (defun article () (one-of '(the a)))
  (defun noun () (one-of '(man ball woman table)))
  (defun verb () (one-of '(hit took saw liked)))

  (defun one-of (elts)
    (list (random-elt elts)))

  (defun random-elt (elts)
    (elt elts (random (length elts))))

  (sentence))


;; defparameter's parameter usually doesn't change during the
;; execution of the program. Any change to a parameter is considered a
;; change *to* the program, and not a change *by* the program.

(defparameter *simple-grammer*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (article noun))
    (verb-phrase -> (verb noun-phrase))
    (article -> the a)
    (noun -> man ball woman table)
    (verb -> hit took saw liked)))

(defvar *grammer* *simple-grammer*)

;; Just an expression I can execute if the value is different.
(setf *grammer* *simple-grammer*)

(defun rule-lhs (rule)
  (first rule))

(defun rule-rhs (rule)
  (rest (rest rule)))

(defun rewrites (category)
  "Returns a list of the possible rewrites of the category."
  (rule-rhs (assoc category *grammer*)))

(defun generate (phrase)
  "Generates a sentence given a phrase.

  A phrase can be either a symbol defined in the language grammer, in
  which case it is recursively expanded and sub-phrases are generated,
  or it can be a list of phrases which are again generated and
  combined."
  (cond ((listp phrase) (mappend #'generate phrase))
        ((rewrites phrase) (generate (random-elt (rewrites phrase))))
        (t (list phrase))))


;; Ex 2.1
(defun generate1 (phrase)
  (let ((rewrites (rewrites phrase)))
    (cond ((listp phrase) (mappend #'generate phrase))
          (rewrites (generate (random-elt rewrites)))
          (t (list phrase)))))

;; Ex 2.2
(defun terminalp (p)
  (not (null (rest (rewrites p)))))

(defun non-terminal-p (p)
  (not (terminalp p)))

(defun generate2 (phrase)
  (cond ((listp phrase) (mappend #'generate phrase))
        ((terminalp phrase) (random-elt (rewrites phrase)))
        ((not (terminalp phrase)) (generate phrase))
        (t (list phrase))))


;; There are almost always 2 ways of developing programs:
;; 1. Use the most simple problem description and map it directly into
;; the programming language.
;; 2. Create an abstraction/domain layer for the problem space and
;; then build an interpreter for that layer. 

(defparameter *bigger-grammer*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammer* *bigger-grammer*)

;; PAIP> (generate 'sentence)
;; (THE BIG BIG TABLE LIKED A BIG BIG ADIABATIC WOMAN)

(defun generate-tree (phrase)
  "Generates a sentence given a phrase.

  A phrase can be either a symbol defined in the language grammer, in
  which case it is recursively expanded and sub-phrases are generated,
  or it can be a list of phrases which are again generated and
  combined."
  (cond ((listp phrase) (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))) )
        (t (list phrase))))

(defun generate-all (phrase)
  "Generate a list of all possible explanations of this phrase"
  (cond ((null phrase) (list phrase))
        ((listp phrase) (combine-all (generate-all (first phrase))
                                     (generate-all (rest phrase))))
        ((rewrites phrase) (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by append a y to an x.
   E.g. (combine-all '((a) (b)) '((1) (2)))
   -> ((A 1) (B 1) (A 2) (B 2))"
  ;; For each y in ylist, append it to each x in xlist.
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))

;; Ex 2.3



;; Ex 2.4
;; Cross-Products of 2 arrays with a function! That's an almost
;; impossible way of looking at this, entirely oblivious to me. 
(defun cross-product (fn xlist ylist)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y)) xlist))
           ylist))

(defun combine-all-with-cross-product (xlist ylist)
  (cross-product #'append xlist ylist))
