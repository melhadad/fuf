;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         gr.lisp
;;; Description:  System file to load the grammar updated
;;;               for testing measure, noun-compound, partitive &
;;;               complex proper
;;; Author:       Michael Elhadad
;;; Created:      17 Jul 1992
;;; Modified:     18 Jan 93: added adverbial, mood, special & nba (JR)
;;;                5 Jul 95: SURGE 2.2 VERSION
;;; -----------------------------------------------------------------------
;;; FUF - a functional unification-based text generation system. (Ver. 5.4)
;;;
;;; Copyright (c) 1987-2014 by Michael Elhadad. all rights reserved.
;;;
;;; Permission to use, copy, and/or distribute for any purpose and
;;; without fee is hereby granted, provided that both the above copyright
;;; notice and this permission notice appear in all copies and derived works.
;;; Fees for distribution or use of this software or derived works may only
;;; be charged with express written permission of the copyright holder.
;;; THIS SOFTWARE IS PROVIDED ``AS IS'' WITHOUT EXPRESS OR IMPLIED WARRANTY.
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(defvar *surge-dir* (concatenate 'string *fuf-dir* "surge/code/")
  "Folder containing the SURGE code")

(defvar *surge-input-dir* (concatenate 'string *fuf-dir* "surge/inputs/")
  "Folder containing the SURGE input examples")

(defun reload-gr ()
  "Reload all grammar"
  (let ((cur *default-pathname-defaults*))
    (cd *surge-dir*)
    ;; Utilities for temporal patterns  (tpattern unifier)
    (load "tpat.l")
    (load "types.l")
    (load "transitivity.l")
    (load "voice.l")
    (load "adverbial.l")
    (load "clause.l")
    (load "mood.l")
    (load "verb-group.l")
    (load "np.l")
    (load "complex.l")
    (load "determiner.l")
    (load "gr-modular.l")
    (load "special.l")
    (load "nba.l")
    (cd cur)))

(defun load-gr ()
  "Load missing parts of the grammar"
  (let ((cur *default-pathname-defaults*))
    (cd *surge-dir*)
    ;; Utilities for temporal patterns  (tpattern unifier)
    (require "tpat" "tpat.lisp")
    (require "types" "types.lisp")
    (require "clause" "clause.lisp")
    (require "verb-group" "verb-group.lisp")
    (require "np" "np.lisp")
    (require "determiner" "determiner.lisp")
    (require "complex" "complex.lisp")
    (require "gr-modular" "gr-modular.lisp")
    (require "nba" "nba.lisp")
    (require "special" "special.lisp")
    (cd cur)))

(defun load-gr-inputs ()
  "Load sample SURGE inputs"
  (let ((cur *default-pathname-defaults*))
    (cd *surge-input-dir*)
    (load "ir.lisp")
    (cd cur)))

;; (load-gr)

;; (load-gr-inputs)

;; (test :from 't1 :to 't2 :timed t)

;; ------------------------------------------------------------
(provide "$gr/gr")
;; ------------------------------------------------------------
