;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         continue.l
;;; Description:  Exhaust a unification continuation
;;; Author:       Michael Elhadad
;;; Created:      25 Sep 1989
;;; Modified:     05 Nov 1989 (Me: added limit to bp)
;;;               15 Jun 1990 Added success arg to determine.
;;;               25 Jun 1990 Used toplevel-uni.
;;;               02 Jul 1990 Added path2 arg.
;;;               11 Feb 1991 Removed catch/throw
;;;               13 Nov 1991 Updated calls to *fail* for bk-class
;;;               26 Nov 1991 Made cset a parameter
;;;               11 Feb 1992 Fixed (partially) calls to determine.
;;;                           Should be made into continuation style *****
;;;               15 May 1992 Added *from-top* nil
;;;               26 Oct 1993 Added arg cat-attribute in call-linearizer
;;;               05 Jun 1994 Added msg argument to fail.
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

(in-package "FUG5")

(defun u-exhaust (fd1 fd2 &key (test t) result (limit 10000)
		      (cset-attribute *cset-attribute*))
  "Unifier exhaust. Takes 2 functional descriptions. 
   Returns the list of all possible unifications until test is satisfied.
   In test, refer to the list being built as fug5::result.
   Does NOT recurse on constituents."
  (toplevel-uni 
   fd1 fd2 limit cset-attribute
   (unify *input* fd2 path path new-frame
	  #'(lambda (msg) (declare (ignore msg)) (nreverse result))
	  #'(lambda (fd fail frame)
	      (push (copy-tree fd) result)
	      (if (eval `(let ((result ',result)) ,test))
		  (nreverse result)
		(*fail* fail frame (make-path) (make-path) *input-pair*))))))
  
(defun default-continuation2 (fd fail frame)
  "default-continuation does a throw 'top.  This one just returns the value."
  (declare (ignore fd fail frame)
	   (special *input*))
  *input*)

(defun u-exhaust-top (input &key (grammar *u-grammar*)
			    (cat-attribute *cat-attribute*)
			    (cset-attribute *cset-attribute*)
			    non-interactive (test t) result (limit 10000))
  "Unify a fd with a grammar and return all possible unifications.
   Preprocess input, unify top-level, then constituents.
   If non-interactive is nil, statistics are printed."
  (let ((*from-top* nil))
    (declare (special *from-top*))
    (toplevel-uni
     input grammar limit cset-attribute
     (unify-sub-constituents
      *input* grammar path new-frame
      #'(lambda (msg) (declare (ignore msg)) (nreverse result))
      #'(lambda (fd fail frame)
	  (unless non-interactive
	    (format t "~%[Used ~D backtracking points ~
		      - ~D wrong branches - ~D undo~:P]~%" 
		    *counter* *wrong-branches* *number-of-undo*))
	  (push (copy-tree (determine
			    fd fail frame 'default-continuation2 
			    grammar cat-attribute cset-attribute))
		result)
	  (if (eval `(let ((result ',result)) ,test))
	    (nreverse result)
	    (*fail* fail frame (make-path) (make-path) *input-pair*)))))))

(defun u-exhaust-top-2 (input &key (grammar *u-grammar*)
			      (cat-attribute *cat-attribute*)
			      (cset-attribute *cset-attribute*)
			      non-interactive (test t) (count 1) (limit 10000))
  "Unify a fd with a grammar and return all possible unifications.
   Preprocess input, unify top-level, then constituents.
   If non-interactive is nil, statistics are printed.
   Does not keep the successive values returned.  Just keep producing,
   hoping that test does something with it, bit by bit.  
   In test, fd is the value of the fd being returned (after determination),
   Count keeps track of how many times it has been called."
  (let ((*from-top* nil))
    (declare (special *from-top*))
    (toplevel-uni 
     input grammar limit cset-attribute
     (unify-sub-constituents
      *input* grammar path new-frame
      #'(lambda (msg) (declare (ignore msg)) count)
      #'(lambda (fd fail frame)
	  (incf count)
	  (unless non-interactive
	    (format t "~%[Used ~D backtracking points ~
		    - ~D wrong branches - ~D undo~:P]~%" 
		    *counter* *wrong-branches* *number-of-undo*))
	  (if (eval `(let ((count ',count)
			   (fd ',(determine 
				  fd fail frame 'default-continuation2
				  grammar cat-attribute cset-attribute)))
		       ,test))
	    count
	    (*fail* fail frame (make-path) (make-path) *input-pair*)))))))


(defun uni-num (input n &key (grammar *u-grammar*) (limit 10000)
		      (*cat-attribute* *cat-attribute*))
  "Unifies input n  successive times with grammar, and outputs the
   successive strings.
   n must be a numeric constant."
  (let ((fd (u-exhaust-top-2 
	     input :grammar grammar
	     :non-interactive nil
	     :limit limit
	     :test 
	     `(if (<= count ,n)
		(progn 
		  (print-sentence (call-linearizer fd))
		  nil)
		t))))
    (if (consp fd) (print-sentence (call-linearizer fd)))))


(provide "$fug5/continue")