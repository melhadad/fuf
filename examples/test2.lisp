;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test2.l
;;; Description:  Test special attributes
;;; Author:       Michael Elhadad
;;; Created:      18 May 1990
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")


(defun unify-numbers (n1 n2 &optional path)
  (max n1 n2))

(define-procedural-type 'num 'unify-numbers :syntax 'numberp)


(setf a '((num 1)))
(setf b0 '((num 2)))
(setf b1 '((num 0)))

(setf c '((num 1)))
(setf d0 '(%conflict% (a {num})))
(setf d1 '(%conflict% (num {a})))
(setf d2 '((a ((num {^ ^ num})))))


(defun unify-lists (l1 l2 &optional path)
  (if (> (length l1) (length l2))
      l1
    l2))

(define-procedural-type 'list 'unify-lists :syntax 'sequencep)

(setf e '((list (1 2 3))))
(setf f0 '((list (1 2 3 4))))
(setf f1 '((list (1 2))))

(setf g '((list (1 2 3))))
(setf h0 '(%conflict% (a {list})))
(setf h1 '(%conflict% (list {a})))
(setf h2 '((a ((list {^ ^ list})))))
