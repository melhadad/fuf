;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         lattice.l
;;; Description:  Example of lattice type definitions
;;;               Tests new typed features unifier.
;;; Author:       Michael Elhadad
;;; Created:      13 Aug 1991
;;; Modified:     
;;; Log file:     ChangeLog
;;; Tag file:     TAGS
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")


(define-feature-type a (b c d))
(define-feature-type b (e f g))
(define-feature-type c (g))
(define-feature-type d (h))
(define-feature-type e (i j))
(define-feature-type f (j))
(define-feature-type g (k))
(define-feature-type h (k l))
(define-feature-type j (m))
(define-feature-type l (m))

