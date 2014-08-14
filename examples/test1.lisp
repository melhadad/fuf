;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test1.l
;;; Description:  Test fset features
;;; Author:       Michael Elhadad
;;; Created:       7 May 1990
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(setf a '((a 1) (b 2)))
(setf b0 '((fset (a b))))
(setf b1 '(%a% (fset (a))))
(setf b2 '((fset nil)))   ;; illegal
(setf b3 '((fset (a b c))))

(setf c '((a ((b 1)))))
(setf d0 '((a ((fset (c))))))
(setf d1 '((a ((fset (b))))))
(setf d2 '((a ((c ((fset (x y))))))))

(setf e '((a ((b {x y})))))
(setf f0 '((a ((fset (z))))))
(setf f1 '((a ((fset (z)))) (x 1)))
(setf f2 '((a ((fset (z)))) (x ((y 1)))))

(setf g '((a ((fset (b c)) (b (x y))))))
(setf h0 '(%a% (a ((fset (x y))))))
(setf h1 '((a ((fset (c))))))
