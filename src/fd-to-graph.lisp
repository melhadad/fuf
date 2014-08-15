;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         fd-to-graph.l
;;; Description:  Convert an FD to canonical form (graph-based).
;;; Author:       Michael Elhadad
;;; Created:       5 Nov 1993
;;; Modified:     30 May 1994: Cleaned up version.
;;;               10 Jun 1994: Rewrote innermost-ls without score function (JR)
;;;               11 Apr 1995: replaced all {} in code with (make-path) (ME)
;;; Package:      FUG5
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

(defun ph (hash-table)
  "Print the contents of a hash-table one entry per line."
  (maphash #'(lambda (key entry) (declare (ignore key)) (pprint entry))
           hash-table))

;; Name of a function that is used to determine which of two paths is
;; "more canonical" than the other.
;; The default function chooses the shortest path and for equal lengths,
;; the smaller path in lexicographic order.
(defparameter *smaller-path-fct* 'innermost-ls)

;; ==================================================
;; Data Structures
;; ==================================================

;; Key is path, Value is triplet <path,id,value>
;;   e.g. FD = '((a {b})                o
;;               (b ((c 1))))          / \
;;                                    a   b
;;   One id for each node              \ /
;;   in the graph.                      o
;;   Here: 3 ids (gensymed)             |
;;                                      c--o 1
;;
;; Here table will be:
;; (<{},    N1, FD>
;;  <{a},   N2, ((c 1))>,
;;  <{b},   N2, ((c 1))>,
;;  <{a c}, N3, 1>,
;;  <{b c}, N3, 1>)
(defvar *path-table* (make-hash-table :test #'equal :size 500)
  "A hashtable of all triplets: <path,id,value>
   identified in the fd during fd-to-graph conversion indexed by path.")

(defparameter *unbound* (gensym)
  "Mark that a path-entry has not received a value.")

(defstruct path-entry
  path
  (id (gentemp "N"))
  (visited nil)
  (value *unbound*))

(defun find-entry (path)
  "Return the path entry for a given path"
  (gethash (path-l path) *path-table*))

(defun add-entry (&key path (id (gentemp "N")) visited (value *unbound*))
  "Add an entry to the hash-table."
  (setf (gethash (path-l path) *path-table*)
	(make-path-entry :path path :id id :visited visited :value value)))

;; Map path to id - error if not found.
;; path-to-id2 returns nil if not found.
(defun path-to-id (path &optional (*path-table* *path-table*))
  "Convert a path to an id.  Assume that path-table has already been built."
  (let ((entry (find-entry path)))
    (if (null entry)
      (multiple-value-bind (val phys missing) (gdc *input* path)
	(declare (ignore val))
	(if (path-null missing)
	  (setf entry (find-entry phys)))))
    (assert (path-entry-p entry) (path)
	    "Internal problem in fd-to-graph: *path-table* not complete.~%~
             Path ~s missing." path)
    (path-entry-id entry)))

(defun path-to-id2 (path *path-table*)
  "Convert a path to an id if found, return nil otw."
  (let ((entry (find-entry path)))
    (when entry (path-entry-id entry))))

;; Map from id to path-entry.
(defun find-id (id &optional (*path-table* *path-table*))
  (maphash #'(lambda (pathl pe)
	       (declare (ignore pathl))
	       (if (eq id (path-entry-id pe)) (return-from find-id pe)))
	   *path-table*))

(defun id-to-path (id &optional (*path-table* *path-table*))
  "Convert id to path in the graph representation of an FD."
  (let ((entry (find-id id)))
    (if (path-entry-p entry) (path-entry-path entry) nil)))


;; Mapping from old physical representant to new one during relocate process.
;; ******
;; <old-phys-rep-path,new-phys-rep-path>
(defvar *new-physical-path* (make-hash-table :test #'eq)
  "A hash table to remember relocation of fds during relocate and optimize.")

(defun get-new-physical-path (path)
  (gethash (path-to-id path) *new-physical-path*))

(defun record-new-physical-path (tpath npath)
  (setf (gethash (path-to-id tpath) *new-physical-path*) npath))



;; ==================================================
;; Equivalence class management
;; ==================================================

;; Build quotient set from path table - table <id, list-of-paths>
;; For example table above, quotient set is:
;; ( <N1, (<{}, N1, FD>)>
;;   <N2, (<{a}, N2,((c 1))> <{b},N2,((c 1))>)>
;;   <N3, (<{a c},N3,1> <{b c},N3,1>)> )
;; In each list, the first element is assured to be the shortest path in the
;; equivalence class.  Among all representants of same length,
;; choose alphabetically.
(defun quotient-set (path-table)
  "Get a hash-table of path entries and return a table of pairs (id,list)
   where each list is an equivalence class with shortest path first
   in every class.  Indexed by id."
  (let ((q (make-hash-table :size 500)))
    (maphash #'(lambda (pathl pe)
		 (declare (ignore pathl))
		 (let* ((id (path-entry-id pe))
			(current (gethash id q)))
		   (setf
		    (gethash id q)
		    ;; If new elt shorter, put it first, else second.
		    (if (and current
			     (funcall *smaller-path-fct*
				      (path-entry-path (first current))
				      (path-entry-path pe)))
		      (progn
                        ;; (format T "~& I am calling ~s" *smaller-path-fct*)
                        ;; (format T "~& putting ~s second" (path-entry-path pe))
                        (cons (first current) (cons pe (rest current))))
		      (progn
                        ;; (format T "~& I am calling ~s" *smaller-path-fct*)
                        ;; (format T "~& putting ~s first" (path-entry-path pe))
                        (cons pe current))))))
	     path-table)
    q))

;; Definition of who is the canonical representant in a class: choose among
;; 2 paths who is the canonical rep ("smaller") path.
(defun smaller-path (p1 p2)
  (let ((l1 (path-len p1))
	(l2 (path-len p2)))
    (cond ((< l1 l2) t)
	  ((= l1 l2)
	   (let ((s1 (format nil "~s" (path-l p1)))
		 (s2 (format nil "~s" (path-l p2))))
	     (string< s1 s2)))
	  (t nil))))

;; Takes as input a path in list form and two features that are cues
;; to determine the canonicity of the path.
;; Returns the sublist of path delimited by the two cues, including the first
;; - cue1 - excluding the second - cue2
;; (cue1 is assumed to appear before cue2).
;; e.g., (between '(a b dss c d sss e f) 'dss 'sss) = (dss c d),
;; but (between '(a b dss c d sss e f) 'sss 'dss) = (sss e f)
;; (cue2 before cue1 ignored)
(defun between (path-as-list cue1 cue2)
  (let* ((other-cues (set-difference '(sss sss-root dss dss-buf ents)
                                     `(,cue1 ,cue2)))
	 (me1 (member cue1 path-as-list))
	 (me2 (member cue2 path-as-list))
	 (me1-me2 (ldiff me1 me2)))
    (when (and me1 me2 (loop for other-cue in other-cues
			     for res = (member other-cue me1-me2)
                                     then (or res (member other-cue me1-me2))
			     finally (return (not res))))
      me1-me2)))

;; Returns the cue features of a path
(defun find-cues (path-as-list)
  (intersection path-as-list '(sss sss-root adss dss dss-buf ents)))

;; Ranks the cue features
(defun cue-rank (cue)
  (case cue ((sss) 4) ((sss-root) 3) ((adss) 2) ((dss dss-buf) 1) ((ents) 0)))

;; Boolean testing which of two paths is most canonic
;; (and should thus contain the physical representant)
(defun innermost-ls (path1 path2)
  (let* ((p1 `(start ,@(path-l path1)))
	 (p2 `(start ,@(path-l path2)))
	 (p1-cues (find-cues p1))
	 (p2-cues (find-cues p2))
	 (p1-1st-cue (first p1-cues))
	 (p2-1st-cue (first p2-cues))
	 (smaller (smaller-path path1 path2)))
;;    (format T "~&p1 = ~s" p1)
;;    (format T "~&p2 = ~s" p2)
;;    (format T "~&p1-cues = ~s" p1-cues)
;;    (format T "~&p2-cues = ~s" p2-cues)
    (cond
     ;; if neither paths contain a cue feature, use the default length +
     ;; alphabetical canonicity criteria, e.g, {a b} < {c d e}
     ((not (or p1-1st-cue p2-1st-cue)) smaller)

     ;; if only one path contains a cue feature, it is the most canonic
     ;; e.g., {a dss b} < {c d}
     ((not p2-1st-cue) T)
     ((not p1-1st-cue) nil)

     ;; if both paths contain cue(s)
     (T (let ((p1-1st-cue-rank (cue-rank p1-1st-cue))
	      (p2-1st-cue-rank (cue-rank p2-1st-cue)))
	  (cond

	   ;; 1st criteria is the rank of their 1st (i.e. leftmost) cue
	   ;; e.g., {a dss b} < {a adss b}
	   ((< p1-1st-cue-rank p2-1st-cue-rank) T)
	   ((> p1-1st-cue-rank p2-1st-cue-rank) nil)

	   ;; if that rank is the same,
	   (T (let* (;; (p1-start (find-start p1))
		     ;; (p2-start (find-start p2))
		     (p1-preflen (length (between p1 'start p1-1st-cue)))
		     (p2-preflen (length (between p2 'start p2-1st-cue))))
		(cond

		 ;; 2nd criteria is the length of the subpath before the 1st cue
		 ;; e.g., {a b sss c} < {e sss f g}
		 ((< p1-preflen p2-preflen) T)
		 ((> p1-preflen p2-preflen) nil)

		 ;; if that length is the same then,
		 (T (let ((p1-2nd-cue (second p1-cues))
			  (p2-2nd-cue (second p2-cues)))
		      (cond

		       ;; if neither path contains a 2nd cue, use the default
		       ;; tie-breaker e.g., {a b sss c} < {e f sss g h}
		       ((not (or p1-2nd-cue p2-2nd-cue)) smaller)

		       ;; if only one path contains a 2nd cue,
                       ;; it is the most canonic
		       ;; e.g., {a sss b dss e} < {f sss g h}
		       ((not p2-2nd-cue) T)
		       ((not p1-2nd-cue) nil)

		       ;; if both path contains a 2nd cue feature
		       (T (let ((p1-2nd-cue-rank (cue-rank p1-2nd-cue))
				(p2-2nd-cue-rank (cue-rank p2-2nd-cue)))
			    (cond

			     ;; 3rd criteria is the rank of that 2nd cue,
			     ;; e.g., {sss a b dss} < {sss c adss}
			     ((< p1-2nd-cue-rank p2-2nd-cue-rank) T)
			     ((> p1-2nd-cue-rank p2-2nd-cue-rank) nil)

			     ;; if that rank is the same
			     (T (let ((p1-inflen
				       (length (between p1 p1-1st-cue p1-2nd-cue)))
				      (p2-inflen
				       (length (between p2 p2-1st-cue p2-2nd-cue))))
				  (cond

				   ;; 3rd criteria is length of the subpath
				   ;; between the 1st and 2nd cue
				   ;; e.g., {sss dss a b} < {sss c dss d}
				   ((< p1-inflen p2-inflen) T)
				   ((> p1-inflen p2-inflen) nil)

				   ;; if that length is the same
				   (T (let ((p1-3rd-cue (third p1-cues))
					    (p2-3rd-cue (third p2-cues)))
					(cond

					 ;; if neither path has a 3rd cue,
					 ;; use default tie-breaker
					 ;; e.g., {sss dss a} < {sss dss b c}
					 ((not (or p1-3rd-cue p2-3rd-cue)) smaller)

					 ;; if only one path has a 3rd cue,
					 ;; it is the most canonic,
					 ;; e.g., {sss dss a ents b} < {sss dss c d}
					 ((not p2-3rd-cue) T)
					 ((not p1-3rd-cue) nil)

					 ;; any path has at most 3 cues and there is
					 ;; only a single possible 3rd cue: ents
					 ;; The default tie-breaker is used for cases
					 ;; where both paths have that 3rd cue
					 (T smaller))))))))))))))))))))))

;; Get physical representant of path as defined in quotient-set.
;; Ex: (rep {b c} q1) = {a c}
(defun rep (path quotient &optional pe)
  "Find the representative path of a path given the quotient set.
   If the path's path entry is known, use it."
  (let ((class (g-get-class path quotient pe)))
    (if class (path-entry-path (first class)))))

(defun get-class (path quotient &optional pe)
  "Get the equivalence class of a path given the quotient set.
   If path's path-entry is known use it, else find it."
  (let ((pe (or pe (find-entry path))))
    (if (path-entry-p pe)
	(gethash (path-entry-id pe) quotient))))

(defun path-equiv (q p1 p2)
  "Is p1 equivalent to p2 given quotient set q?
   Assumption: *input* bound to total fd."
  ;; Problem with cycles: certain legal paths do not appear in the path-table.
  ;; For ex: (a c c c b) === (a b) but (a c c c b) is not there.
  ;; For these cases, call (gdc p) to find an equivalent path that IS
  ;; in the table.
  ;; path-equiv returns T only if it can prove that 2 paths are equivalent given
  ;; the current fd.  If p1 and p2 are not physically instantiated,
  ;; there is no proof that they are distinct, and path-equiv returns nil.
  (let ((c1 (get-class p1 q))
	(c2 (get-class p2 q))
	(m1 nil)
	(m2 nil))
    (unless c1
      (multiple-value-bind (v1 phys1 missing1) (gdc *input* p1)
	(declare (ignore v1))
	(setf c1 (get-class phys1 q))
	(setf m1 missing1)))
    (unless c2
      (multiple-value-bind (v2 phys2 missing2) (gdc *input* p2)
	(declare (ignore v2))
	(setf c2 (get-class phys2 q))
	(setf m2 missing2)))
    (and (eq c1 c2) (path-equal m1 m2))))


(defun g-get-class (path quotient &optional pe)
  "Get the equivalence class of a path given the quotient set.
   If path's path-entry is known use it, else find it.
   Works also for cases when path goes through loops."
  (let ((pe (or pe (find-entry path))))
    (unless pe
      (multiple-value-bind (v phys missing) (gdc *input* path)
	(declare (ignore v))
	(when (path-null missing)
	  (setf pe (find-entry phys)))))
    (if (path-entry-p pe)
      (gethash (path-entry-id pe) quotient))))

;; ==================================================
;; Build the tables for paths and edges
;; ==================================================

;; Build a table of path-entries describing the graph equivalent to fd.
;; Two paths receive the same id if they point to the same node.
;; Side effect: every path that is mentioned in an indirection is physically
;; instantiated in fd (physically modified) so that a regular traversal
;; of fd will necessarily reach all paths in the table.

(defun build-id-table (fd path)
  (let ((*input* fd))
    (clrhash *path-table*)
    (build-id-table-aux fd path path)))

;; phys-path: where does fd appear physical in the total fd.
;; path: where we come from visiting fd.
;; Example: fd = ((a {b c}) (b ((c ((d 1))))))
;; can call (build ((d 1)) {a} {b c}) to add ids for paths {a d}.
;; Assumption: fd is in canonical-1 form (list of pairs with no path on
;; left and no attribute appearing twice on the left eg, ((a 1) (a {b})).
;; Associate a unique id to each existing path in the fd
;; Return a table of path/id/value.
;; This is a depth-first search of the physical FD graph repeated as many
;; times as necessary for structure shared subgraphs.  Ie, if an arc is
;; shared 3 times (it belongs to 3 distinct paths), it is traversed 3
;; times.
;; Conflations are computed here so that 2 paths leading to the same
;; node receive the same id.

;; fd parameter = fd under path in total-fd
(defun build-id-table-aux (fd path phys-path)
  ;; (format t "~&p = ~s / pp = ~s" path phys-path)
  ;; (ph *path-table*)
  ;; (print "1")
  (let ((pe1 (find-entry path))
	(pe2 (find-entry phys-path)))
    (cond
     ((and pe1 (path-entry-visited pe1)) *path-table*)
     ((leaf-p fd)
      (multiple-value-bind (pe1 pe2) (merge-ids path phys-path fd pe1 pe2)
	(declare (ignore pe2))
	(setf (path-entry-visited pe1) t))
      *path-table*)
     ((path-p fd)
      (setf fd (absolute-path fd phys-path));; phys-path to interpret ^.
      ;; We now have 4 paths playing a role here (not necessarily distinct):
      ;; path and phys-path: where we are.
      ;; fd and phys-to: where we are told to go.
      ;; All 4 have to receive the same id.
      (multiple-value-bind (val phys-to missing node-tried arc-tried)
          (gdc *input* fd)
	(unless (path-null missing)
	  ;; Uninstantiated path: make sure all arcs of path get an id
	  ;; from below the physical leaf (phys-to) to the end of fd.
	  (add-chain node-tried arc-tried phys-to missing)
	  (setf phys-to (path-append phys-to missing)))
	;; Same path as 1st argument each time, so that at the end all
	;; receive the id of phys-to.
	(let ((pe3 (find-entry phys-to))
	      (pe4 (find-entry fd)))
	  (multiple-value-setq (pe3 pe1)
	      (merge-ids phys-to path val pe3 pe1))
	  (multiple-value-setq (pe3 pe2)
	      (merge-ids phys-to phys-path val pe3 pe2))
	  (multiple-value-setq (pe3 pe4)
	      (merge-ids phys-to fd val pe3 pe4))
	  ;; Compute all extensions of path in val.
	  ;; The extensions of fd, phys-to and phys-path will be computed when
	  ;; they will be traversed physically
          ;; (in the regular depth first traversal).
	  ;; Avoid cycles
	  (unless (and (not (path-equal phys-path phys-to))
		       (path-prefix phys-path phys-to))
	    (build-id-table-aux val path phys-to)))
	*path-table*))
     (t
      (multiple-value-setq (pe2 pe1) (merge-ids phys-path path fd pe2 pe1))
      (setf (path-entry-visited pe1) t)
      (mapc #'(lambda (pair)
		(assert (consp pair) (pair fd)
			"Ill-formed fd in fd-to-graph: ~s" pair)
		(if (special-p (first pair))
		  (let ((new-path (path-extend path (first pair)))
			(new-phys-path (path-extend phys-path (first pair))))
		    (multiple-value-bind (pe1 pe2)
			(merge-ids new-path new-phys-path (second pair)
				   (find-entry new-path)
                                   (find-entry new-phys-path))
		      (declare (ignore pe2))
		      (setf (path-entry-visited pe1) t)))
		  (build-id-table-aux (second pair)
				      (path-extend path (first pair))
				      (path-extend phys-path (first pair)))))
	    fd)
      *path-table*))))


;; Conflate ids for paths p1 and p2
;; Make sure both have an id.
;; If ids already exist and are different,
;; replace (id p2) with (id p1) everywhere.
;; If only one exists, use its id.
;; DO NOT CALL ADD-ENTRY if entry already exists for a path, so that the value
;; of its visited flag remains defined.
(defun merge-ids (p1 p2 val pe1 pe2)
  (cond
   ((and (null pe1) (null pe2))
    (setf pe1 (add-entry :path p1 :value val))
    (if (path-equal p1 p2)
      (setf pe2 pe1)
      (setf pe2 (add-entry :path p2 :value val :id (path-entry-id pe1)))))
   ((and pe1 pe2)
    ;;  (format t "~&Merging ~s and ~s" pe1 pe2)
    (unless (or (path-equal p1 p2)
		(eq (path-entry-id pe1) (path-entry-id pe2)))
      (maphash #'(lambda (l p)
		   (declare (ignore l))
		   (if (eq (path-entry-id p) (path-entry-id pe2))
		       (setf (path-entry-id p) (path-entry-id pe1))))
	       *path-table*)))
   ((null pe1)
    (setf pe1
	  (if (path-equal p1 p2)
	    pe2
	    (add-entry :path p1 :value val :id (path-entry-id pe2)))))
   ((null pe2)
    (setf pe2
	  (if (path-equal p1 p2)
	    pe1
	    (add-entry :path p2 :value val :id (path-entry-id pe1))))))
  (values pe1 pe2))


(defun add-chain (node arc phys-leaf extension)
  "Phys-leaf points to a leaf in the physical fd.
  Node is the last node in fd containing the leaf.
  extension is a path that must be defined under this leaf.
  Make sure the extension is physically present in fd.
  For each arc in extension, make sure an id exists.
  Ex: fd = ((a {x y z}) (x ((b 1))))
  (add-chain {x} {y z}) creates ids for {x y} and {x z}."

  (if (null node)
    (setf (second arc) (build-fd-from-path extension))
    (nconc node (build-fd-from-path extension)))
  (loop for arc in (path-l extension)
	initially (setf p phys-leaf)
	for p = (path-extend p arc)
	do (unless (find-entry p)
	     (add-entry :path p :value nil))))


(defun real-leaf-p (object)
  (and object
       (leaf-p object)
       (null (subtype object))))


;; ======================================================================
;; Relocate
;; ======================================================================

;; First find the physical-representant of rpath so that relative paths are
;; correctly interpreted in the sub-fd.
;; The call to filter-flags is necessary to avoid the creation of
;; spurious tetards.
;; Check ((a {b}) (b ((c nil)))).
(defun relocate (total rpath &optional (*smaller-path-fct* *smaller-path-fct*))
  (let* ((*input* (filter-flags total))
	 (*path-table* (make-hash-table :test #'equal :size 500))
	 (*path-table* (build-id-table *input* (make-path)))
	 (q1 (quotient-set *path-table*))
	 (*new-physical-path* (make-hash-table :test #'eq)))
    (multiple-value-bind (const rpath-phys missing) (gdc *input* rpath)
      (cond
       ((path-null missing)
        ;; (relocpairs q1 rpath-phys rpath-phys (make-path) const const))
	(relocpairs q1 rpath-phys rpath-phys (make-path) const
                    (copy-tree const)))
       (t nil)))))


;; q1 = the quotient set for conflation for total
;; rpath = from where do we relocate
;; tpath = where are we within total
;; cpath = where are we within the relocated fd (result)
;; pairs = the current fd being processed
;; result = accumulator for the relocated constituent
(defun relocpairs (q1 rpath tpath cpath pairs result)
  (if (leaf-p pairs)
    pairs
    (loop for pair in pairs
          for pair-ind = 0 then (+ pair-ind 1)
	  do (relocpair q1 rpath tpath cpath pair-ind pairs result)
	  finally (return result))))

;; pair-ind = index of pair within the fd
(defun relocpair (q1 rpath tpath cpath pair-ind pairs result &aux p)
  (let* ((pair (nth pair-ind pairs))
	 (feature (first pair))
	 (value (second pair))
	 (new-cpath (path-extend cpath feature))
	 (new-tpath (path-extend tpath feature))
	 (new-rep (rep new-tpath q1))
	 (relocated-rep (relocated-rep q1 new-rep rpath)))
    ;; (format t "~&new-tpath = ~s - new-cpath = ~s" new-tpath new-cpath)
    (cond

     ;; Has new-tpath been already copied or displaced physically in result?
     ;; If so, insert into result a pointer to new phys-rep in result.
     ;; This also prevents looping in cycles.
     ((setf p (get-new-physical-path new-tpath))
      (setf (second (nth pair-ind (top-gdc result cpath)))
	    ;; (make-relative-path new-cpath p)
	    p))

     ((not (path-equal new-cpath relocated-rep))
      (setf (second (nth pair-ind (top-gdc result cpath)))
	    relocated-rep))

     ((leaf-p value)
      (record-new-physical-path new-tpath new-cpath)
      result)

     ;; pattern and cset can have paths in their values: patch them.
     ;; The function relocate-special calls the appropriate method given feature.
     ((special-p feature)
      (setf (second (nth pair-ind (top-gdc result cpath)))
	    (relocate-special q1 value feature rpath new-tpath new-cpath)))

     ((not (path-p value))
      (relocpairs q1 rpath new-tpath new-cpath value result)
      (record-new-physical-path new-tpath new-cpath))

     (t
      ;; value is a path where there should be a physical rep.
      ;; In all cases, insert here value instead of path.
      (multiple-value-bind (pointed-val point-to)
	  (gdc *input* (absolute-path value new-tpath))
	(setf (second (nth pair-ind (top-gdc result cpath)))
;;	      pointed-val)
	      (copy-tree pointed-val))
	(record-new-physical-path point-to new-cpath)
	(relocpairs q1 rpath point-to new-cpath
		    pointed-val result))))))


(defun in-scope (q1 path rpath)
  "Does path point to a location that is under the scope of rpath?
  ie, is there a representant of path that has rpath as a prefix?
  Return the prefixed path if exists or else nil.
  Assumption: path is the physical representant.
  If path itself is in-scope, return it (to avoid breaking the conflation)."
  (if (path-prefix path rpath)
      path
    (let ((class (g-get-class path q1)))
      (loop for pe in class
	    do (if (path-prefix (path-entry-path pe) rpath)
		   (return (path-entry-path pe)))))))

(defun relocated-rep (q1 path rpath)
  "Find the canonical representant of path after relocation under rpath."
  ;; Method: get class of path in quotient set of total.
  ;; Keep in this class all those that are extensions of rpath.
  ;; Apply canonical selection on the subset after truncation.
  (loop for pe in (g-get-class path q1)
	for path = (path-entry-path pe)
	for rep = (if (path-prefix path rpath) (strip-prefix path rpath))
	        then (if (and (path-prefix path rpath)
			      (or (null rep)
				  (funcall *smaller-path-fct*
					   (strip-prefix path rpath)
					   rep)))
		       (progn
                         ;; (format T "~& I am calling ~s" *smaller-path-fct*)
                         (strip-prefix path rpath))
		       (progn
                         ;; (format T "~& I am calling ~s" *smaller-path-fct*)
                         rep))
	;; do (format t "~&path = ~s; rep = ~s" path rep)
	finally (return rep)))



(defun longest-common-prefix (l1 l2 &optional accu)
  "Return the lcp of 2 lists.
  Ex: (longest-common-prefix '(1 2 3 4) '(1 2 a)) --> (1 2)"
  (cond ((or (null l1) (null l2)) (nreverse accu))
	((equalp (car l1) (car l2))
	 (longest-common-prefix (cdr l1) (cdr l2) (cons (car l1) accu)))
	(t (nreverse accu))))

(defun strip-prefix (path prefix)
  "Return path without the prefix."
  (make-path :l (nthcdr (length (path-l prefix)) (path-l path))))

(defun make-relative-path (p1 p2)
  "Transform p2 into a path relative to p1.
  Return 3 vals: the relative path, how many ^ are necessary, and the
  length of the shared prefix between p1 and p2."
  (let* ((l1 (path-l p1))
	 (l2 (path-l p2))
	 (lcp (longest-common-prefix l1 l2))
	 (llcp (length lcp))
	 (uplevel (- (length l1) llcp)))
    (values
     (make-path :l (append
		    (make-sequence 'list uplevel :initial-element '^)
		    (subseq l2 llcp)))
     uplevel
     llcp)))

;; ------------------------------------------------------------
;; INSERT-FD: reverse of relocate, insert a total fd within a larger total
;; fd under path subfd-path.
;; ------------------------------------------------------------
;; Example:
;; (insert-fd '((a {b}) (b 1) (c {^ b}))
;;            '((b 2))
;;            {c})
;; =>
;; ((b 2)
;;  (c ((a {c b})  <------ NOTE updated path.
;;      (b 1)
;;      (c {c b}))))  <--- NOTE relative path is resolved
;;

(defun insert-fd (fd total subfd-path)
  (filter-flags (u total (insert-empty-fd fd subfd-path))))

;; Just put an fd under a path
;; Example: (insert-empty-fd '((a {b}) (c {^ a})) {x y} T)
;; =>
;; ((x ((y ((a {x y b})     <--- NOTE updated path
;;          (c {^ a}))))))  <--- NOTE preserve relative path
;;
;; Example: (insert-empty-fd '((a {b}) (c {^ a})) {x y} T)
;; =>
;; ((x ((y ((a {b})         <--- NOTE NOT updated path
;;          (c {^ a}))))))  <--- NOTE preserve relative path
;;
(defun insert-empty-fd (fd path)
  (if (path-null path)
    fd
    (let* ((total (build-fd-from-path path))
	   (pair (the-last-arc-of-path total path)))
      ;; Get rid of all flags after second
      (if (or (leaf-p fd) (path-p fd))
	(setf (cdr pair) (list fd))
	(setf (cdr pair) (list (insert-patch fd path path))))
      total)))

;; Copy the fd appearing at level path and patches the paths according to
;; relocation under path rpath in total fd.
(defun insert-patch (fd path rpath)
  (cond ((null fd) fd)
	(t (cons (insert-patch-pair (car fd) path rpath)
		 (insert-patch (cdr fd) path rpath)))))

(defun insert-patch-pair (pair path rpath)
  (let* ((attr (car pair))
	 (value (second pair)))
    (cond ((leaf-p value) (list attr value))
	  ((and (path-p value) (path-relative-p value))
	   (list attr value))
	  ((path-p value) ;; an absolute path
	   (list attr (path-append rpath value)))
	  ((special-p attr)
	   (copy-special-pair pair path))
	  (t (list attr
		   (insert-patch value (path-extend path attr) rpath))))))


;; ======================================================================
;; Extensional To Intensional Conversion
;; ======================================================================

(defun compare-features (f1 f2)
  (string< (format nil "~s" (car f1)) (format nil "~s" (car f2))))

(defun outgoing-arcs (fd)
  (mapcar #'first fd))

(defun ext-equal (fd1 fd2)
  (cond
   ((and (leaf-p fd1) (leaf-p fd2)) (equalp fd1 fd2))
   ((or (leaf-p fd1) (leaf-p fd2)) nil)
   (t (and (= (length fd1) (length fd2))
	   (set-equal (outgoing-arcs fd1) (outgoing-arcs fd2))
	   (equal (sort (unfold fd1) #'compare-features)
		  (sort (unfold fd2) #'compare-features))))))

(defun ext-equal2 (fd1 fd2)
  "Check that 2 UNFOLDED fds are equal modulo feature movement"
  (cond
   ((and (leaf-p fd1) (leaf-p fd2)) (equalp fd1 fd2))
   ((or (leaf-p fd1) (leaf-p fd2)) nil)
   (t (and (= (length fd1) (length fd2))
	   (set-equal (outgoing-arcs fd1) (outgoing-arcs fd2))
	   (equal (sort fd1 #'compare-features)
		  (sort fd2 #'compare-features))))))

(defun unfold (fd)
  "Replace all paths in an fd with the value they point to.
  In case of cycles, keep a path (only case where a path remains in output)."
  (let ((*input* fd))
    (unfold-aux fd (make-path))))

(defun unfold-aux (fd path)
  "Get an FD with absolute paths in equations (e.g., (a {x y a b})), and
  replace them with the value they point to."
  (cond
   ((leaf-p fd) fd)
   ((path-p fd)
    (multiple-value-bind (val phys) (gdc *input* (absolute-path fd path))
    (if (path-prefix path phys)     ;; Cycle condition
	fd
      (unfold-aux val phys))))
   (t (mapcar
       #'(lambda (pair)
	   (list
	    (car pair)
	    (if (special-p (car pair))
		(second pair)
	      (let ((newpath (path-extend path (car pair))))
		(unfold-aux (second pair) newpath)))))
       fd))))

(defun ext2int (fd path)
  ;; Check to see if the fd@path is extensionally identical to another path
  ;; in fd, and if yes, return the path where they match (not path itself).
  ;; Traverse fd and check at each node if (ext-equal fd@path node)
  (let* ((unfolded (unfold fd))
	 (const (top-gdc unfolded path)))
    (ext2int-aux unfolded const path (make-path))))

(defun ext2int-aux (fd const origpath path)
  (cond ((path-equal origpath path) nil)    ;; don't want original place.
	((leaf-p fd) (if (equalp const fd) path))
	((path-p fd) ;; remains because of cycles.
	 nil)
	((ext-equal2 fd const) path)
	(t (some #'(lambda (pair)
		     (let ((newpath (path-extend path (car pair))))
		       (ext2int-aux (second pair) const origpath newpath)))
		 fd))))

(defun s-ext2int (fd path)
  "Check to see if the fd appearing under path is extensionally equivalent
to another path, if yes replace fd@path with a pointer to that other path,
if not, try again for all defined extensions of path to find a match in the
rest of the fd."
  (let* ((unfolded (unfold fd))
	 (const (top-gdc unfolded path))
	 (res (copy-tree fd)))
    (s-ext2int-aux res path unfolded const)
    res))

(defun s-ext2int-aux (fd path unfolded const)
  (let ((match (ext2int-aux unfolded const path (make-path))))
    (if match
	(setf (second (top-gdpp fd path)) match)
      (mapc #'(lambda (pair)
		(let ((newpath (path-extend path (first pair))))
		  (s-ext2int-aux fd newpath unfolded (second pair))))
	    const))))


;; ======================================================================
;; GDC: New version of gdp that returns value AND physical path.
;; ======================================================================

(defun ban-path-as-total (fd)
  (cond ((and (path-p fd) (path-relative-p fd))
	 (error "A total fd cannot be a relative path"))
	((path-p fd) (setf fd
                           (filter-flags (build-fd-from-path fd (make-path)))))
	(T fd)))

(defun top-gdc (fd path)
  (let ((*input* (ban-path-as-total fd)))
    (gdc fd path)))

(defun gdc (fd path)
  "given an fd (representing a connected graph) and a path,
  return 5 values:
  1. value pointed by path
  2. physical path where this value occurs in fd.
  3. what's missing from path in the physical rep.
  4. the last node of the path traversed in fd.
  5. the last arc of the path traversed in fd."
  ;; Check for cycles in the way. Mark our way in already-tried.
  ;; For fds in canonical form, all paths point to the physical
  ;; representant directly, and already-tried is not necessary.  For other
  ;; fds, you need it as shown by example: ((a {o}) (o ((m {a m}))))

  (do* ((fd fd)
	(path path)
	(already-tried (list path))
	(cpath (make-path))
	(current-fset (find-fset fd))
	(arc-tried (list :top fd))
	(node-tried (list arc-tried)))
      ((and (not (path-p fd)) (path-null path))
       (values fd cpath path node-tried arc-tried))
    ;; (format t "~&DO path = ~s - cpath = ~s - fd = ~s" path cpath fd)
    (cond
     ;; Below any/given/nil is unspecified.
     ((or (eq fd 'any) (eq fd 'given) (eq fd nil))
      (return (values nil cpath path fd arc-tried)))

     ;; Below any other atom is impossible: none
     ((leaf-p fd) (return (values 'none cpath path node-tried arc-tried)))

     ;; Need to go up to the root and go down again to the indirection.
     ((path-p fd)
      ;; A non-special cannot point to a special: check validity here.
      #+check-types
      (let ((from (car arc-tried))
	    (to   (car (last (path-l fd)))))
	(cond
	 ((eq from to))  ;; fine - type equality
	 ((or (special-p from) (special-p to))
	  (return (values 'none cpath path node-tried arc-tried)))))
      ;; ok proceed...
      (let* ((path-prefix (absolute-path fd cpath))
	     (npath (path-append path-prefix path)))
	;; This is a loop: ({a b c} {a b c d e})
	;; Note: the problem is only when a short points to a long:
	;; ((a {a b})) loops, ((a ((b {a})))) is fine.
	;; (format t "~&already tried = ~s - pp = ~s" already-tried path-prefix)
	(cond
	 ;; Find a short path pointing to an extension of itself:
	 ;; Replace (a {a b}) by (a ((b {a}))) and continue.
	 ;; Ignore cases (a {a}) which are tetard cases dealt with below.
	 ((and (path-prefix path-prefix cpath)
	       (not (path-equal path-prefix cpath)))
	  ;; (format t "~&Found a short to long pointer: ~s ~s" path-prefix cpath)
	  (let ((extension (strip-prefix path-prefix cpath)))
;;	    (format T "~& short->long")
;;	    (format T "~& arc-tried1 = ~s" arc-tried)
;;	    (format T "~& already-tried1 = ~s" already-tried)
	    (setf (second arc-tried)
		  (build-fd-from-path extension cpath)))
	  ;; Reinit already-tried for the new structure
	  (setf already-tried nil))
;;	  (format T "~& arc-tried2 = ~s" arc-tried)
;;	  (format T "~& already-tried2 = ~s" already-tried)

	 ;; Find a long path pointing to a prefix of itself.  This is a loop, eat
	 ;; the elements of the path and do not check for tetard cases here, because
	 ;; you want to cut the tetard head where it is redundant, not here.
	 ;; Example: ((a ((b ((c {a d d d b c}))) (d {a}))))
	 ;; You do NOT want to cut (d nil) but instead (c nil).
	 ((and (path-prefix cpath path-prefix)
	       (not (path-equal cpath path-prefix))))
;;	  (format T "~& long->short")

	 ;; Tetard cases: cut the redundant path eg ((a {a})) is ((a nil))
	 ;; or  ((a {b}) (b ((m {a m})))) is ((a {b}) (b ((m nil))))
	 ((member npath already-tried :test #'path-equal)
;;	  (format T "~& tetard")
	  (setf (second arc-tried) nil)
	  (return (values nil cpath path node-tried arc-tried))))

;;	(format T "~& arc-tried3 = ~s" arc-tried)
;;	(format T "~& already-tried3 = ~s" already-tried)
	(setf path npath)
	(push path already-tried)
	(setf cpath (make-path))
	(setf fd *input*)
	(setf arc-tried (list :top fd))
	(setf node-tried (list arc-tried))))
;;	(format T "~& arc-tried4 = ~s" arc-tried)
;;	(format T "~& already-tried4 = ~s" already-tried)

     ;; Special attributes are also atomic: cannot go below them.
     ;; but can have a path as value.
     ((member (path-car path) *special-attributes*)
      (cond ((cdr (path-l path))
             (return (values 'none cpath path node-tried arc-tried)))
	    ((or (null current-fset)
		 (member (path-car path) current-fset)
		 (eq (path-car path) 'fset))
	     (setf arc-tried (safe-assoc (path-car path) fd))
	     (setf node-tried fd)
	     (if (consp arc-tried)
		 (setf fd (second arc-tried))
	       (return (values nil cpath path node-tried arc-tried)))
	     (setf cpath (path-extend cpath (path-car path)))
	     (setf path (path-cdr path)))
	    (t (return (values 'none cpath path node-tried arc-tried)))))

     ;; Are we within the authorized FSET? If so, go down one level.
     ((or (null current-fset)
	  (member (path-car path) current-fset))
      (setf arc-tried (safe-assoc (path-car path) fd))
      (setf node-tried fd)
      (if (consp arc-tried)
	  (setf fd (second arc-tried))
	  (return (values nil cpath path node-tried arc-tried)))
      (setf cpath (path-extend cpath (path-car path)))
      (setf path (path-cdr path))
      (setf current-fset (find-fset fd)))

     ;; Not within the authorized FSET: value is none.
     (t (return (values 'none cpath path node-tried arc-tried))))))

(defun phys-rep (path)
  (multiple-value-bind (val phys missing) (gdc *input* path)
    (declare (ignore val))
    (path-append phys missing)))


;; ==============================================================================
;; Convert a unified FD to a pattern-tree to be displayed graphically.
;; - Convert to graph
;; - Label constituents by cat
;; - Extract pattern recursively
;; Output format as sexpr: (root (path child) ...)
;; where: root = a cat
;;        path = a path {a b c}
;;        child = a tree or a lex or nil

(defun fd-to-pattern-tree (fd)
  (let ((table (build-id-table fd {})))
    (format t "~%Graph ready.~%")
    (fd-to-pattern-tree-traverse {})))

(defun make-label (lex cat)
  (let ((lexs (if (null lex) "" (path-entry-value lex)))
	(cats (if (null cat) "" (path-entry-value cat))))
    (cond ((and (null lex) (null cat)) nil)
	  ((and (equal lexs "") (equal cats "")) nil)
	  ((or (null lex) (null lexs) (equal lexs "")) cats)
	  ((null cat) lexs)
	  (t (format nil "~a:~a" lexs cats)))))

(defun make-pattern-tree (label edges)
  (cons label edges))

(defun make-edge (edge-label pattern-tree)
  (list edge-label pattern-tree))

(defun fd-to-pattern-tree-traverse (path)
  (let ((fd (find-entry path)))
    (if (leaf-p fd)
	fd
	(let ((cat (find-entry (path-extend path 'cat)))
	      (pattern (find-entry (path-extend path 'pattern)))
	      (lex (find-entry (path-extend path 'lex))))
	  (cond ((null pattern) (make-label lex cat))
		(t (make-pattern-tree
		    (make-label lex cat)
		    (mapcar
                     #'(lambda (pattern-elt)
                         (let ((target-path
                                (if (leaf-p pattern-elt)
                                    (path-extend path pattern-elt)
                                    (absolute-path
                                     pattern-elt
                                     (path-extend path 'pattern)))))
                           (make-edge target-path
                                      (fd-to-pattern-tree-traverse
                                       target-path))))
                     (clean-pattern (path-entry-value pattern))))))))))

(defun filter-pattern-tree (pattern-tree)
  (cond ((null pattern-tree) nil)
	((leaf-p pattern-tree) pattern-tree)
	(t (let ((non-empty-edges
                  (mapcan #'(lambda (edge)
                              (let ((filtered-subtree
                                     (filter-pattern-tree (second edge))))
                                (if (null filtered-subtree)
                                    nil
                                    (list (list (car edge) filtered-subtree)))))
                          (cdr pattern-tree))))
	     (if (null non-empty-edges)
		 nil
		 (make-pattern-tree (car pattern-tree) non-empty-edges))))))

(defun pattern-tree-children (edge)
  (if (consp edge)
      (if (consp (second edge))
	  (cdr edge)
	  nil)
      nil))

(defun draw-patterns (pattern-tree)
  (print-structure
   (list pattern-tree)
   :node-id #'(lambda (n) (gensym))
   :node-children #'pattern-tree-children
   :node-name #'(lambda (edge) (if (consp (second edge))
				   (format nil "~s" (first edge))
				   (format nil "~s: ~s"
                                           (first edge) (second edge)))))
  (values))


(defun draw-test (item &key (filter t))
  (test3 :item item)
  (let* ((result (get-test-result item))
	 (pt (fd-to-pattern-tree result))
	 (fpt (filter-pattern-tree pt)))
    (if filter
	(draw-patterns fpt)
	(draw-patterns pt))))

;; Same but without the long fd-to-graph
(defun redraw-test (item &key (filter t))
  (let* ((result (get-test-result item))
	 (pt (fd-to-pattern-tree-traverse {}))
	 (fpt (filter-pattern-tree pt)))
    (if filter
	(draw-patterns fpt)
	(draw-patterns pt))))
