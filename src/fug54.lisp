;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         fug54.l
;;; Description:  System definition for FUG (Functional Unifier) with UNDO
;;; Author:       Michael Elhadad
;;; Created:      05-Aug-88
;;; Modified:     13 Nov 89
;;;               31 Jan 90
;;;               30-Apr-90 - Defined *special-attributes*
;;;                         - Put all exports here
;;;               20 Feb 91 - added export for path functions
;;;               28 Jul 91 - update to FUG5 started
;;;               01 Jun 93 - added file findcset
;;;               06 Jun 93 - added path-value and set-path-value
;;;               09 Jun 93 - added ^~ notation.
;;;               04 Jan 94 - added fd-to-graph to package.
;;;               30 May 94 - added relocater to cset's definition.
;;;               08 May 95 - added make-load-form
;;;                           (Murugan Kannan <kannan@steve.iit.edu>)
;;;               01 Dec 95 - load linearize2 (with HTML support)
;;;               08 Jul 14 - added test3, get-test-result, uni-fd-string
;;; Package:      FUG5
;;; -----------------------------------------------------------------------
;;;
;;; FUF - a functional unification-based text generation system. (Ver. 5.4)
;;;
;;; Copyright (c) 1987-2011 by Michael Elhadad. all rights reserved.
;;;
;;; Permission to use, copy, and/or distribute for any purpose and
;;; without fee is hereby granted, provided that both the above copyright
;;; notice and this permission notice appear in all copies and derived works.
;;; Fees for distribution or use of this software or derived works may only
;;; be charged with express written permission of the copyright holder.
;;; THIS SOFTWARE IS PROVIDED ``AS IS'' WITHOUT EXPRESS OR IMPLIED WARRANTY.
;;; -----------------------------------------------------------------------

(defpackage "FUG5"
  (:use "COMMON-LISP")
  (:export ;; variables
   *agenda-policy*
   *all-trace-off*
   *all-trace-on*
   *any-at-unification*
   *cat-attribute*
   *check-ambiguities*
   *cset*
   *cset-attribute*
   *default-external-function*
   *dictionary*
   *disjunctive-attributes*
   *input*
   *lexical-categories*
   *special-attributes*
   *top*
   *trace-determine*
   *trace-marker*
   *typed-features*
   *u-grammar*
   *use-given*
   *use-wait*
   *warn-type-redefinition*

   ;; functions
   all-tracing-flags
   avg-complexity
   call-linearizer
   categories-not-unified
   check-cset
   clean-fd
   clear-bk-class
   clear-grammar
   clear-tests
   complexity
   control-demo
   control-demo
   def-alt
   def-conj
   def-grammar
   def-test
   define-bk-class
   define-feature-type
   define-procedural-type
   disabled-tracing-flags
   do-tenses
   draw-grammar
   draw-types
   enabled-tracing-flags
   external
   fd-p
   fd-sem
   fd-syntax
   fd-to-graph
   FD-to-list
   FD-to-prolog
   filter-flags
   filter-nils
   filter-nones
   fset
   fuf-postscript
   gdp
   gdpp
   get-test
   get-test-result
   grammar-p
   hyper-trace-category
   insert-fd
   internal-trace-off
   internal-trace-on
   lexfetch
   lexstore
   list-cats
   list-to-FD
   normalize-fd
   path-car
   path-cdr
   path-cons
   path-butlast
   path-last
   path-append
   path-push
   path-pop
   path-extend
   path-null
   path-equal
   path-value
   register-categories-not-unified
   register-category-not-unified
   relativize-fd
   relocate
   reset-procedural-type
   reset-procedural-types
   reset-typed-features
   set-path-value
   store-plurals
   store-verbs
   subtype
   subsume
   sunder
   top-fd-to-list
   top-gdp
   top-gdpp
   trace-alts
   trace-bp
   trace-bk-class
   trace-category
   trace-cset
   trace-disable
   trace-disable-all
   trace-disable-alt
   trace-disable-match
   trace-enable
   trace-enable-all
   trace-enable-alt
   trace-enable-match
   trace-level
   trace-off
   trace-on
   trace-wait
   types-postscript
   u
   u-exhaust
   u-exhaust-top
   u-rel
   under
   uni
   uni-fd
   uni-num
   uni-string
   uni-fd-string
   unif
   unify-cset
   unregister-category-not-unified
   test
   test3
   test-with

   ;; symbols explicitly used in the code
   ;; (used with quote). Don't import them if already exist.
   ==
   ===
   ;; * already exists in LISP
   ;; trace already exists in USER
   \@
   ^
   ^~
   %TRACE-OFF%
   %TRACE-ON%
   %BREAK%
   *done*
   a-an
   adj
   adv
   after
   alt
   an
   animate
   any
   article
   before
   capitalize
   cardinal
   case
   cat
   conj
   consonant
   control
   cset
   demo-message
   demonstrative
   det
   determiner
   digit
   distance
   dots
   dual
   ending
   far
   feature
   feminine
   first
   gap
   gender
   given
   index
   infinitive
   interrogative
   lex
   masculine
   mergeable
   modal
   mood
   near
   neuter
   no
   none
   not-one
   noun
   number
   objective
   opt
   ordinal
   past
   past-participle
   pattern
   person
   personal
   phrase
   plural
   possessive
   pound
   prep
   present
   present-participle
   present-third-person-singular
   pronoun
   pronoun-type
   punctuation
   quantified
   question
   ralt
   reflexive
   relative
   relpro
   roman
   root
   second
   singular
   subjective
   tense
   test
   third
   value
   verb
   yes
   ))

(in-package "FUG5")
(format t "Loading system FUG5...~%")
(setf *print-array* t)  ;; for external

(defun pwd ()
  "Current directory in string form"
  (namestring *default-pathname-defaults*))

(defun cd (stringname)
  "Change directory - given in string form"
  (setf *default-pathname-defaults* (pathname stringname)))

(defun compile-fug5 ()
  "Compile all files required by system fug5"
  (let ((cur *default-pathname-defaults*))
    (proclaim '(optimize (compilation-speed 0) (speed 3) (safety 0) (debug 0)))
    (cd *fuf-src-dir*)
    (load "macros.lisp")
    (compile-file "vars.lisp")
    (compile-file "define.lisp")
    (compile-file "trace.lisp")
    (compile-file "generator.lisp")
    (compile-file "backtrack.lisp")
    (compile-file "external.lisp")
    (compile-file "fd-to-graph.lisp")
    (compile-file "determine.lisp")
    (compile-file "path.lisp")
    (compile-file "ignore.lisp")
    (compile-file "wait.lisp")
    (compile-file "alt.lisp")
    (compile-file "ralt.lisp")
    (compile-file "fset.lisp")
    (compile-file "control.lisp")
    (compile-file "type.lisp")
    (compile-file "pattern.lisp")
    (compile-file "findcset.lisp")
    (compile-file "graph.lisp")
    (compile-file "top.lisp")
    (compile-file "lexicon.lisp")
    (compile-file "linearize3.lisp")
    (compile-file "checker.lisp")
    (compile-file "complexity.lisp")
    (compile-file "fdlist.lisp")
    (compile-file "continue.lisp")
    (compile-file "test.lisp")
    (compile-file "psgraph.lisp")
    (cd cur)))

(defun reload-fug5 ()
  "Reload all files of system in right order"
  (let ((cur *default-pathname-defaults*))
    (cd *fuf-src-dir*)
    (load "macros.lisp")
    (load "vars")
    (load "define")
    (load "generator")
    (load "trace")
    (load "backtrack")
    (load "external")
    (load "path")
    (load "fd-to-graph")
    (load "determine")
    (load "lexicon")
    (load "linearize3")
    (load "pattern")
    (load "ignore")
    (load "wait")
    (load "alt")
    (load "ralt")
    (load "fset")
    (load "control")
    (load "type")
    (load "findcset")
    (load "graph")
    (load "top")
    (load "complexity")
    (load "fdlist")
    (load "continue")
    (load "checker")
    (load "test")
    (cd cur)))

(defun reload-fug5-src ()
  "Reload all files of system in right order"
  (let ((cur *default-pathname-defaults*))
    (cd *fuf-src-dir*)
    (load "macros.lisp")
    (load "vars.lisp")
    (load "define.lisp")
    (load "generator.lisp")
    (load "trace.lisp")
    (load "backtrack.lisp")
    (load "external.lisp")
    (load "path.lisp")
    (load "fd-to-graph.lisp")
    (load "determine.lisp")
    (load "lexicon.lisp")
    (load "linearize3.lisp")
    (load "pattern.lisp")
    (load "ignore.lisp")
    (load "wait.lisp")
    (load "alt.lisp")
    (load "ralt.lisp")
    (load "fset.lisp")
    (load "control.lisp")
    (load "type.lisp")
    (load "findcset.lisp")
    (load "graph.lisp")
    (load "top.lisp")
    (load "complexity.lisp")
    (load "fdlist.lisp")
    (load "continue.lisp")
    (load "checker.lisp")
    (load "test.lisp")
    (cd cur)))

(defun warranty ()
  (format t
    "
			    NO WARRANTY

  BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE PROGRAM ``AS IS'' WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE
PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
REPAIR OR CORRECTION.

  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING
OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED
TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY
YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER
PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGES.
    ")
  (values))

(defun license ()
  (format t
    "
;;; FUF - a functional unification-based text generation system. (Ver. 5.4)
;;;
;;; Copyright (c) 1987-2011 by Michael Elhadad. all rights reserved.
;;;
;;; ================================================================
;;;         General License Agreement and Lack of Warranty
;;; ================================================================
;;;
;;; This software is distributed in the hope that it will be useful
;;; but WITHOUT ANY WARRANTY. The author does not accept responsibility to
;;; anyone for the consequences of using it or for whether it serves any
;;; particular purpose or works at all. No warranty is made about the
;;; software or its performance.
;;;
;;; Use and copying of this software and the preparation of derivative
;;; works based on this software are permitted, so long as the following
;;; conditions are met:
;;; 	o  The copyright notice and this entire notice are included intact
;;; 	   and prominently carried on all copies.
;;; 	o  No fees or compensation are charged for use, copies, or
;;; 	   access to this software. You may charge a nominal
;;; 	   distribution fee for the physical act of transferring a
;;; 	   copy, but you may not charge for the program itself.
;;; 	o  If you modify this software, you must cause the modified
;;; 	   file(s) to carry prominent notices (a Change Log)
;;; 	   describing the changes, who made the changes, and the date
;;; 	   of those changes.
;;; 	o  Any work distributed or published that in whole or in part
;;; 	   contains or is a derivative of this software or any part
;;; 	   thereof is subject to the terms of this agreement. The
;;; 	   aggregation of another unrelated program with this software
;;; 	   or its derivative on a volume of storage or distribution
;;; 	   medium does not bring the other program under the scope
;;; 	   of these terms.
;;;
;;; This software is made available AS IS, and is distributed without
;;; warranty of any kind, either expressed or implied.
;;;
;;; In no event will the author or his institutions be liable to you
;;; for damages, including lost profits, lost monies, or other special,
;;; incidental or consequential damages arising out of the use or
;;; inability to use (including but not limited to loss of data or data
;;; being rendered inaccurate or losses sustained by third parties or a
;;; failure of the program to operate as documented) the program, even if
;;; you have been advised of the possibility of such damanges, or for any
;;; claim by any other party.
;;;
;;; The current version of this software may be obtained at http://www.cs.bgu.ac.il/surge
;;;
;;; Please send bug reports, comments, questions and suggestions to
;;; elhadad@cs.bgu.ac..il.
;;; I would also appreciate receiving any changes or improvements you may
;;; make.
;;;
    ")
  (values))


(format t
    "
    FUF Version 5.4, Copyright (C) 1987-2011 Michael Elhadad.
    FUF comes with absolutely no warranty; for details type (fug5::warranty).
    This is free software, and you are welcome to redistribute it
    under certain conditions, type (fug5::license) for details.

")

(provide "$fug5/fug5")
