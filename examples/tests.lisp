;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; ------------------------------------------------------------
;;; File        : TESTS.LISP
;;; Description : Run all tests in fuf
;;; Author      : Michael Elhadad 
;;; Created     : Jun 16 2011
;;; Modified    : 
;;; Language    : Common Lisp
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun fuf54-test ()
  (trace-off)
  (isetup0)
  (gsetup0)
  (test)
  (isetup1)
  (gsetup1)
  (test)
  (isetup2)
  (gsetup2)
  (test)
  (isetup3)
  (gsetup3)
  (test)
  (isetup4)
  (gsetup4)
  (test)
  (isetup5)
  (gsetup5)
  (test)
  (isetup6)
  (gsetup6)
  (test)
  (isetup7)
  (gsetup7)
  (test)
  (isetup8)
  (gsetup8)
  (test)
  (isetup9)
  (gsetup9)
  (test)
  (isetup10)
  (gsetup10)
  (test)
  (isetup-bk-class)
  (test)
  (isetup-con3)
  (gsetup-con3)
  (test)
)
