;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         ralt.l
;;; Description:  Random alt - choose a branch in random order
;;; Author:       Michael Elhadad
;;; Created:      21 May 1990
;;; Modified:     15 Jun 1990: added pair argument
;;;               02 Jul 1990: added path2 arg.
;;;               28 Jul 1991: Removed ralt-unify as alt-unify is made more
;;;                            general with order-given.
;;;               13 Nov 1991: changed call to *fail*
;;;               09 Dec 1991: added after-wait.
;;;               06 Jan 1992: avoid problem with position
;;;               08 Feb 1992: the other position was not fixed...
;;;               20 Oct 92: Added level to trace-format
;;; Package:      FUG5
;;; -----------------------------------------------------------------------
;;;
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
(format t "Ralt unifier...~%")

;; ---------------------------------------------------------------------
;; RALT-UNIFY-SIMPLE: Traverse each branch, and backtrack
;;                    to a next branch, randomly chosen, if fail.
;;                    When no more branches, fail.
;;                   (change the fail argument when trying a branch)
;; ---------------------------------------------------------------------

(defun ralt-unify-simple (fd1 fd2 path1 path2 bk-class frame fail success after-wait
			 traced branches branch-number orig-branches
			 &key (pair :unknown))

  ;; eat the tracing flags between branches
  (when (and (tracing-flag (car branches))
	     (trace-enabled (car branches)))
    (handle-trace (pop branches) frame))

  ;; No more branches - fail
  (cond ((null branches)
	 (when (trace-enabled traced)
	   (trace-format traced frame 30
			 "Fail in Ralt ~s at level ~s~%" traced path1)
	   (handle-trace traced frame t))
	 (*fail* fail frame *same* path2 pair))

	;; Choose a branch to use: randomly, between 1 and
	;; length(branches)
	(t
	 (let* ((chosen (random (length branches)))
		(the-branch (nth chosen branches))
		(tmp (car branches)))
	   (trace-format traced frame 10
			"Entering Ralt ~s -- ~:R try: Branch #~s"
			traced branch-number
			(let ((pos (position the-branch orig-branches
					     :test #'equalp)))
			  (if (integerp pos)
			    (1+ pos)
			    'unknown)))
	   ;; Swap car and the-branch
	   (setf (car branches) the-branch
		 (nth chosen branches) tmp)
	  (backtrack frame new-frame bk-class traced
	    (unify fd1 (car branches) path1 path2 new-frame
		   (make-failure fail
		      (ralt-unify-simple
		       fd1 fd2 path1 path2 bk-class frame fail success after-wait
		       traced
		       (cdr branches)
		       (1+ branch-number)
		       orig-branches
		       :pair pair))
		   #'(lambda (fd fail frame)
		       (when (trace-enabled traced)
			 (trace-format
			  traced frame 10
			  "Success with branch ~s in Ralt ~s~%"
			  (let ((pos (position the-branch orig-branches
					       :test #'equalp)))
			    (if (integerp pos) (1+ pos) 'unknown))
			  traced)
			 (handle-trace traced frame t))
		       (when after-wait (funcall after-wait))
		       (unify fd fd2 path1 path2 frame fail success
			      :pair pair))
		   :pair pair))))))


;;; ------------------------------------------------------------
(provide "$fug5/ralt")
;;; ------------------------------------------------------------
