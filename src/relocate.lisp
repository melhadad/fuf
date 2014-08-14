;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         relocate.l
;;; Description:  Cycling bugs in relocate
;;; Author:       Michael Elhadad
;;; Created:       4 Jan 1994
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(define-procedural-type 'surfmap-cset 'unify-cset 
  :syntax 'check-cset :relocater 'relocate-pattern)
(define-procedural-type 'deepmap-cset 'unify-cset 
  :syntax 'check-cset :relocater 'relocate-pattern)


;; Try > (relocate br1 {rls elt1 dss})
;; ((HEAD ((G THANG)))
;;  (ARG1 NIL)
;;  (ARG2 NIL))
;;
;; > (reloqate br1 {rls dss rel1})
;; ((HEAD ((G THANG)))
;;  (ARG1 ((SNOOP DOGG) (DOGGY STYLE)))
;;  (ARG2 ((DR DRE) (THE CHRONIC))))

(setf br1
      '((rls ((dss ((rel1 ((head ((g thang)))
			   (arg1 {^2 ent1})
			   (arg2 {^2 ent2})))
		    (ent1 ((snoop dogg) (doggy style)))
		    (ent2 ((dr dre) (the chronic)))))
	      (elt1 ((dss {^2 dss rel1})))))))


;; Try > (relocate br2 {rls elt1 dss}) 
;; ((HEAD ((A B)))
;;  (ARG0 ((C D)))
;;  (ARG0B {^ ARG0})    
;;  (ARG0C ((C D)))     
;;  (ARG1 NIL)          <<-- BUG
;;  (ARG1B ((E F)))     
;;  (ARG2 ((G H)))      
;;  (ARG2B ((G H)))     
;;  (ARG3 ((I J))))

;; > (relocate br2 {elt1b dss})
;; ((HEAD ((A B)))
;;  (ARG0 ((C D)))
;;  (ARG0B {^ ARG0})
;;  (ARG0C ((C D)))
;;  (ARG1 NIL)          <<-- BUG
;;  (ARG1B NIL)         <<-- BUG
;;  (ARG2 NIL)          <<-- BUG
;;  (ARG2B ((G H)))
;;  (ARG3 ((I J))))

;; > (relocate br2 {rls dss elt1c dss})
;; ((HEAD ((A B)))
;;  (ARG0 ((C D)))
;;  (ARG0B {^ ARG0})
;;  (ARG0C NIL)         <<-- BUG  
;;  (ARG1 NIL)          <<-- BUG  
;;  (ARG1B NIL)         <<-- BUG  
;;  (ARG2 NIL)          <<-- BUG  
;;  (ARG2B NIL)         <<-- BUG  
;;  (ARG3 NIL))         <<-- BUG  

(setf br2
      '((ent3 ((i j)))
	(rls ((dss ((rel1 ((head ((a b)))
			   (arg0 ((c d)))
			   (arg0b {^ arg0})
			   (arg0c {^4 rls dss rel1 arg0})
			   (arg1 {^2 ent1})
			   (arg1b {^3 dss ent1})
			   (arg2 {^3 ent2})
			   (arg2b {^4 rls ent2})
			   (arg3 {^4 ent3})))
		    (ent1 ((e f)))
		    (elt1c ((dss {^2 rel1})))))
	      (ent2 ((g h)))
	      (elt1 ((dss {^2 dss rel1})))))
	(elt1b ((dss {^2 rls dss rel1})))))


;; Try (relocate lst {gargs ga1}) 
;; Desired:
;;   ((GCAT GA)
;;    (VAL {^ SSS VAL})
;;    (SSS ((SCAT SA)
;;          (VAL D1)          ;; or {^ dss val}
;;          (DSS ((DCAT DA)
;;	          (VAL D1))))))

(setf lst
      '((ghead gh)
	(controlled {^ gargs ga3})
	(gargs ((ga1 ((gcat ga)
		      (val {^ sss val})
		      (sss {^3 sss-root sargs sa1})))
		(ga2 ((gcat ga)
		      (val g2)
		      (sss {^3 sss-root sargs sa2})))))
	(surfmap-cset ((+ {gargs ga1} {gargs ga2})))
	(pattern ({^ ghead} {^ gargs ga1} {gargs ga2}))
	(sss-root {^ sss})
	(sss ((shead sh)
	      (sargs ((sa1 ((scat sa)
			    (val {^ dss val})
			    (dss {^3 dss rels dargs da1})))
		      (sa2 ((scat s2)
			    (val s2)
			    (dss {^3 dss rels dargs da2})))))
	      (deepmap-cset ((+ {sargs sa1} {sargs sa2})))
	      (dss ((rels ((dhead dh)
			   (dargs ((da1 {^3 ents da1})
				   (da2 {^3 ents da2})))))
		    (ents ((da1 ((dcat da) (val d1)))
			   (da2 ((dcat da) (val d2)))))))))))


;; Try (relocate rel1 {bls sss-root})

(setf 
 rel1
 '((bls
  ((sss
    ((surfsemcat rhetor)
     (struct hypotax)
     (rels
      ((time ((elts none)))
       (location
	((dss
	  ((deepsemcat entity)
	   (concept address)
	   (token hartford-ct)
	   (attrs
	    ((city "hartford")
	     (state "ct")))))
	 (surfsemcat encyclo)
	 (concept {bls sss rels location dss concept})
	 (restrictors {bls sss rels location dss attrs})
	 (root {bls sss rels location restrictors})
	 (onto place)))
       (co-occur
	((surfsemcat rhetor)
	 (struct event)
	 (root transf-loc)
	 (args
	  ((agent
	    ((fset
	      (dss))))
	   (affected
	    ((dss
	      ((deepsemcat entity)
	       (concept team)
	       (token jazz)
	       (attrs
		((home "utah")
		 (franchise "jazz")))))
	     (surfsemcat encyclo)
	     (onto indiv)
	     (concept {bls sss rels co-occur args affected dss concept})
	     (ref full)
	     (names {bls sss rels co-occur args affected dss attrs})
	     (root {bls sss rels co-occur args affected names})))
	   (located {bls sss rels co-occur args affected})
	   (location
	    ((surfsemcat rhetor)
	     (struct hypotax)
	     (focus {bls sss rels co-occur args location rels score})
	     (root
	      ((dss
		((deepsemcat relation)
		 (role winner)
		 (token uta-at-bos-winner)
		 (args
		  ((game top-level)
		   (winner
		    ((deepsemcat entity)
		     (concept team)
		     (token jazz)
		     (attrs
		      ((home "utah")
		       (franchise "jazz")))))))))
	       (surfsemcat encyclo)
	       (concept {bls sss rels co-occur args location root dss role})
	       (onto indiv)))
	     (rels
	      ((score
		((dss
		  ((deepsemcat entity)
		   (concept score)
		   (token uta-at-bos-score)
		   (attrs
		    ((win 98)
		     (lose 94)))))
		 (surfsemcat encyclo)
		 (concept {bls sss rels co-occur args location rels score dss concept})
		 (restrictors {bls sss rels co-occur args location rels score dss attrs})
		 (root {bls sss rels co-occur args location rels score restrictors})
		 (onto quantity)))
	       (opposition
		((dss
		  ((deepsemcat entity)
		   (concept team)
		   (token celts)
		   (attrs
		    ((home "boston")
		     (franchise "celtic")))))
		 (surfsemcat encyclo)
		 (onto indiv)
		 (concept {bls sss rels co-occur args location rels
			  opposition dss concept}) 
		 (ref full)
		 (names {bls sss rels co-occur args location rels
			opposition dss attrs}) 
		 (root {bls sss rels co-occur args location rels opposition
		       names}))))))))) 
	 (onto none)))))
     (root
      ((surfsemcat rhetor)
       (struct hypotax)
       (root
	((surfsemcat encyclo)
	 (concept =)
	 (onto event)
	 (dss {bls sss dss attrs stats rels histo-stat0-update})
	 (args
	  ((agent
	    ((dss
	      ((deepsemcat entity)
	       (concept player)
	       (token kmalone)
	       (attrs
		((first-name "karl")
		 (last-name "malone")))))
	     (surfsemcat encyclo)
	     (onto indiv)
	     (concept player)
	     (names
	      ((first-name "karl")
	       (last-name "malone")))
	     (root
	      ((first-name "karl")
	       (last-name "malone")))
	     (sss {bls sss root root args agent})))
	   (affected
	    ((surfsemcat rhetor)
	     (struct hypotax)
	     (focus {bls sss-root args affected rels})
	     (root
	      ((surfsemcat encyclo)
	       (concept max-val)
	       (onto indiv)))
	     (rels
	      ((duration
		((surfsemcat encyclo)
		 (concept season)
		 (onto indiv)))))
	     (dss {bls sss dss attrs stats})))))))
       (rels
	((instrument
	  ((dss {bls sss dss attrs stats ents stat0})
	   (surfsemcat encyclo)
	   (concept game-stat)
	   (restrictors
	    ((value 39)
	     (unit pt)))
	   (root
	    ((value 39)
	     (unit pt)))
	   (onto quantity)))))))
     (dss
      ((deepsemcat entity)
       (concept game)
       (token uta-at-bos)
       (attrs
	((floats
	  ((ents
	    ((addr
	      ((deepsemcat entity)
	       (concept address)
	       (token hartford-ct)
	       (attrs
		((city "hartford")
		 (state "ct")))))
	     (date
	      ((deepsemcat entity)
	       (concept date)
	       (token fri-nite)
	       (attrs
		((day-name "friday")
		 (day-part night)))))))))
	 (stats
	  ((ents
	    ((stat0-ca
	      ((deepsemcat entity)
	       (concept player)
	       (token kmalone)
	       (attrs
		((first-name "karl")
		 (last-name "malone")))))
	     (stat0
	      ((deepsemcat entity)
	       (concept game-stat)
	       (token kmalone-pt-at-bos)
	       (attrs
		((value 39)
		 (unit pt)))))
	     (histo-stat0
	      ((deepsemcat entity)
	       (concept histo-stat)
	       (token kmalone-pt-at-bos-ref-set)))
	     (histo-stat0-duration
	      ((deepsemcat entity)
	       (concept season)
	       (token kmalone-pt-at-bos-ref-set-duration)))
	     (histo-stat0-gen-elt
	      ((deepsemcat entity)
	       (concept game-stat)
	       (attrs
		((unit pt)))))
	     (histo-stat0-gen-elt-ca
	      ((deepsemcat entity)
	       (concept player)
	       (token kmalone)
	       (attrs
		((first-name "karl")
		 (last-name "malone")))))
	     (histo-stat0-extr
	      ((deepsemcat entity)
	       (concept integer)
	       (token kmalone-pt-at-bos-ref-set-extr)))))
	   (rels
	    ((stat0
	      ((deepsemcat relation)
	       (role game-stat-rel)
	       (token kmalone-scoring-at-bos)
	       (args
		((carrier {bls sss dss attrs stats ents stat0-ca})
		 (stat {bls sss dss attrs stats ents stat0})))))
	     (histo-stat0-duration
	      ((deepsemcat relation)
	       (role duration)
	       (token kmalone-pt-at-bos-ref-set-duration-rel)
	       (args
		((set {bls sss dss attrs stats ents histo-stat0})
		 (duration {bls sss dss attrs stats histo-stat0-duration})))))
	     (histo-stat0-gen-elt
	      ((deepsemcat relation)
	       (role gen-elt)
	       (token kmalone-pt-at-bos-ref-set-gen-elt)
	       (args
		((set {bls sss dss attrs stats ents histo-stat0})
		 (gen-elt {bls sss dss attrs stats ents
			  histo-stat0-gen-elt}))))) 
	     (histo-stat0-gen-elt-ca
	      ((deepsemcat relation)
	       (role game-stat-rel)
	       (token kmalone-pt-at-bos-ref-set-gen-elt-ca)
	       (args
		((carrier {bls sss dss attrs stats ents
			  histo-stat0-gen-elt-ca}) 
		 (stat {bls sss dss attrs stats ents histo-stat0-gen-elt})))))
	     (histo-stat0-extr
	      ((deepsemcat relation)
	       (role max-val)
	       (token kmalone-pt-at-bos-ref-set-extr-rel)
	       (args
		((extr-val {bls sss dss attrs stats ents histo-stat0-extr})
		 (set {bls sss dss attrs stats ents histo-stat0})))))
	     (histo-stat0-update
	      ((deepsemcat relation)
	       (role =)
	       (token kmalone-tie-pt-season-high-at-bos)
	       (args
		((stat-val 39)
		 (histo-stat-extr {bls sss dss attrs stats ents
				  histo-stat0-extr}))))))))) 
	 (results
	  ((ents
	    ((host
	      ((deepsemcat entity)
	       (concept team)
	       (token celts)
	       (attrs
		((home "boston")
		 (franchise "celtic")))))
	     (visitor
	      ((deepsemcat entity)
	       (concept team)
	       (token jazz)
	       (attrs
		((home "utah")
		 (franchise "jazz")))))
	     (score
	      ((deepsemcat entity)
	       (concept score)
	       (token uta-at-bos-score)
	       (attrs
		((win 98)
		 (lose 94)))))))
	   (rels
	    ((winner
	      ((deepsemcat relation)
	       (role winner)
	       (token uta-at-bos-winner)
	       (args
		((game top-level)
		 (winner {bls sss dss attrs results ents visitor})))))
	     (loser
	      ((deepsemcat relation)
	       (role loser)
	       (token uta-at-bos-loser)
	       (args
		((game top-level)
		 (loser {bls sss dss attrs results ents host})))))
	     (result
	      ((deepsemcat relation)
	       (role beat)
	       (token uta-at-bos-result)
	       (args
		((winner {bls sss dss attrs results ents visitor})
		 (loser {bls sss dss attrs results ents host})))))))))))))))
   (circum
    ((location
      ((sss
	((dss
	  ((deepsemcat entity)
	   (concept address)
	   (token hartford-ct)
	   (attrs
	    ((city "hartford")
	     (state "ct")))))
	 (surfsemcat encyclo)
	 (concept address)
	 (restrictors
	  ((city "hartford")
	   (state "ct")))
	 (root
	  ((city "hartford")
	   (state "ct")))
	 (onto place)))
       (position header)
       (sss-root {bls circum location sss})
       (cat address)
       (city
	((lex {bls circum location sss restrictors city})))
       (state
	((lex {bls circum location sss restrictors state})))))
     (time
      ((sss
	((dss
	  ((deepsemcat entity)
	   (concept date)
	   (token fri-nite)
	   (attrs
	    ((day-name "friday")
	     (day-part night)))))
	 (surfsemcat encyclo)
	 (concept date)
	 (restrictors
	  ((day-name "friday")
	   (day-part night)))
	 (root
	  ((day-name "friday")
	   (day-part night)))
	 (onto time)))
       (fills time-rel)
       (sss-root {bls circum time sss})
       (cat date)
       (day-name
	((lex {bls circum time sss restrictors day-name})))
       (day-part
	((lex {bls circum time sss restrictors day-part})))
       (position end)))
     (co-event
      ((sss
	((surfsemcat rhetor)
	 (struct event)
	 (root transf-loc)
	 (args
	  ((agent
	    ((fset
	      (dss))))
	   (affected
	    ((dss
	      ((deepsemcat entity)
	       (concept team)
	       (token jazz)
	       (attrs
		((home "utah")
		 (franchise "jazz")))))
	     (surfsemcat encyclo)
	     (onto indiv)
	     (concept team)
	     (ref full)
	     (names
	      ((home "utah")
	       (franchise "jazz")))
	     (root
	      ((home "utah")
	       (franchise "jazz")))))
	   (located
	    ((dss
	      ((deepsemcat entity)
	       (concept team)
	       (token jazz)
	       (attrs
		((home "utah")
		 (franchise "jazz")))))
	     (surfsemcat encyclo)
	     (onto indiv)
	     (concept team)
	     (ref full)
	     (names
	      ((home "utah")
	       (franchise "jazz")))
	     (root
	      ((home "utah")
	       (franchise "jazz")))))
	   (location
	    ((surfsemcat rhetor)
	     (struct hypotax)
	     (focus
	      ((dss
		((deepsemcat entity)
		 (concept score)
		 (token uta-at-bos-score)
		 (attrs
		  ((win 98)
		   (lose 94)))))
	       (surfsemcat encyclo)
	       (concept score)
	       (restrictors
		((win 98)
		 (lose 94)))
	       (root
		((win 98)
		 (lose 94)))
	       (onto quantity)))
	     (root
	      ((dss
		((deepsemcat relation)
		 (role winner)
		 (token uta-at-bos-winner)
		 (args
		  ((game top-level)
		   (winner
		    ((deepsemcat entity)
		     (concept team)
		     (token jazz)
		     (attrs
		      ((home "utah")
		       (franchise "jazz")))))))))
	       (surfsemcat encyclo)
	       (concept winner)
	       (onto indiv)))
	     (rels
	      ((score
		((dss
		  ((deepsemcat entity)
		   (concept score)
		   (token uta-at-bos-score)
		   (attrs
		    ((win 98)
		     (lose 94)))))
		 (surfsemcat encyclo)
		 (concept score)
		 (restrictors
		  ((win 98)
		   (lose 94)))
		 (root
		  ((win 98)
		   (lose 94)))
		 (onto quantity)))
	       (opposition
		((dss
		  ((deepsemcat entity)
		   (concept team)
		   (token celts)
		   (attrs
		    ((home "boston")
		     (franchise "celtic")))))
		 (surfsemcat encyclo)
		 (onto indiv)
		 (concept team)
		 (ref full)
		 (names
		  ((home "boston")
		   (franchise "celtic")))
		 (root
		  ((home "boston")
		   (franchise "celtic")))))))))))
	 (onto none)))
       (position end)
       (mood present-participle)
       (sss-root {bls circum co-event sss})
       (cat clause)
       (tense past)
       (fills none)
       (controlled {bls circum co-event partic agent})
       (proc
	((type composite)
	 (relation-type locative)
	 (lex "propel")))
       (partic
	((located
	  ((sss {bls circum co-event sss-root args located})
	   (sss-root {bls circum co-event partic located sss})
	   (cat compound-proper)
	   (head
	    ((cat team-name)
	     (home
	      ((lex {bls circum co-event partic located sss-root names home})))
	     (franchise
	      ((lex {bls circum co-event partic located sss-root names
		    franchise}))))) 
	   (number plural)))
	 (affected {bls circum co-event partic located})
	 (location
	  ((cat pp)
	   (np
	    ((sss {bls circum co-event sss-root args location})
	     (sss-root {bls circum co-event partic location np sss root})
	     (cat common)
	     (definite no)
	     (classifier
	      ((sss {bls circum co-event partic location np sss rels score})
	       (synt-funct classifier)
	       (sss-root {bls circum co-event partic location np classifier sss})
	       (cat score)
	       (win
		((value {bls circum co-event partic location np classifier
			sss restrictors win}))) 
	       (lose
		((value {bls circum co-event partic location np classifier
			sss restrictors lose}))))) 
	     (surfmap-cset
	      ((+ {circum co-event partic location np classifier} {circum
		  co-event partic location np qualifier np}) 
	       (-)))
	     (qualifier
	      ((cat pp)
	       (prep
		((lex "over")))
	       (np
		((sss {bls circum co-event partic location np sss rels
		      opposition}) 
		 (sss-root {bls circum co-event partic location np qualifier np sss})
		 (cat compound-proper)
		 (head
		  ((cat team-name)
		   (home
		    ((lex {bls circum co-event partic location np qualifier
			  np sss-root names home}))) 
		   (franchise
		    ((lex {bls circum co-event partic location np qualifier
			  np sss-root names franchise}))))) 
		 (number plural)))))
	     (head
	      ((lex "victory")))))
	   (prep
	    ((lex "to")))))))
       (surfmap-cset
	((+ {circum co-event partic located} {circum co-event partic
	    location np}) 
	 (-)))))))
   (sss-root {bls sss root root})
   (cat clause)
   (tense past)
   (fills none)
   (proc
    ((type material)
     (lex "equal")))
   (partic
    ((agent
      ((sss
	((dss
	  ((deepsemcat entity)
	   (concept player)
	   (token kmalone)
	   (attrs
	    ((first-name "karl")
	     (last-name "malone")))))
	 (surfsemcat encyclo)
	 (onto indiv)
	 (concept player)
	 (names
	  ((first-name "karl")
	   (last-name "malone")))
	 (root
	  ((first-name "karl")
	   (last-name "malone")))))
       (sss-root {bls partic agent sss})
       (cat compound-proper)
       (head
	((cat person-name)
	 (first-name
	  ((lex {bls partic agent sss names first-name})))
	 (last-name
	  ((lex {bls partic agent sss names last-name})))))))
     (affected
      ((sss-root {bls partic affected sss root})
       (cat common)
       (definite no)
       (classifier
	((synt-funct classifier)
	 (sss-root {bls partic affected classifier sss})
	 (cat noun)
	 (lex "season")))
       (head
	((lex "best")))
       (sss {bls sss-root args affected})))))
   (pred-modif
    ((instrument
      ((cat pp)
       (np
	((sss {bls sss root rels instrument})
	 (sss-root {bls pred-modif instrument np sss})
	 (cat measure)
	 (quantity
	  ((value {bls pred-modif instrument np sss restrictors value})))
	 (unit
	  ((lex "point")))))))))))
 (adss
  ((deepsemcat entity)
   (concept game)
   (token uta-at-bos)
   (attrs
    ((stats
      ((ents
	((stat1-ca
	  ((deepsemcat entity)
	   (concept player)
	   (token humphries)
	   (attrs
	    ((first-name "jay")
	     (last-name "humphries")))))
	 (stat1
	  ((deepsemcat entity)
	   (concept game-stat)
	   (token humphries-pt-at-bos)
	   (attrs
	    ((value 24)
	     (unit pt)))))))
       (rels
	((stat1
	  ((deepsemcat relation)
	   (role game-stat-rel)
	   (token humphries-scoring-at-bos)
	   (args
	    ((carrier {adss attrs stats ents stat1-ca})
	     (stat {adss attrs stats ents stat1})))))))))))))
 (rls
  ((sss
    ((surfsemcat rhetor)
     (struct hypotax)
     (rels
      ((time
	((elts none)))
       (location
	((dss
	  ((deepsemcat entity)
	   (concept address)
	   (token hartford-ct)
	   (attrs
	    ((city "hartford")
	     (state "ct")))))
	 (surfsemcat encyclo)
	 (concept {rls sss rels location dss concept})
	 (restrictors {rls sss rels location dss attrs})
	 (root {rls sss rels location restrictors})
	 (onto place)))
       (co-occur
	((surfsemcat rhetor)
	 (struct event)
	 (root transf-loc)
	 (args
	  ((agent
	    ((fset
	      (dss))))
	   (affected
	    ((dss
	      ((deepsemcat entity)
	       (concept team)
	       (token jazz)
	       (attrs
		((home "utah")
		 (franchise "jazz")))))
	     (surfsemcat encyclo)
	     (onto indiv)
	     (concept {rls sss rels co-occur args affected dss concept})
	     (ref full)
	     (names {rls sss rels co-occur args affected dss attrs})
	     (root {rls sss rels co-occur args affected names})))
	   (located {rls sss rels co-occur args affected})
	   (location
	    ((surfsemcat rhetor)
	     (struct hypotax)
	     (focus {rls sss rels co-occur args location rels score})
	     (root
	      ((dss
		((deepsemcat relation)
		 (role winner)
		 (token uta-at-bos-winner)
		 (args
		  ((game top-level)
		   (winner
		    ((deepsemcat entity)
		     (concept team)
		     (token jazz)
		     (attrs
		      ((home "utah")
		       (franchise "jazz")))))))))
	       (surfsemcat encyclo)
	       (concept {rls sss rels co-occur args location root dss role})
	       (onto indiv)))
	     (rels
	      ((score
		((dss
		  ((deepsemcat entity)
		   (concept score)
		   (token uta-at-bos-score)
		   (attrs
		    ((win 98)
		     (lose 94)))))
		 (surfsemcat encyclo)
		 (concept {rls sss rels co-occur args location rels score
			  dss concept}) 
		 (restrictors {rls sss rels co-occur args location rels
			      score dss attrs}) 
		 (root {rls sss rels co-occur args location rels score
		       restrictors}) 
		 (onto quantity)))
	       (opposition
		((dss
		  ((deepsemcat entity)
		   (concept team)
		   (token celts)
		   (attrs
		    ((home "boston")
		     (franchise "celtic")))))
		 (surfsemcat encyclo)
		 (onto indiv)
		 (concept {rls sss rels co-occur args location rels
			  opposition dss concept}) 
		 (ref full)
		 (names {rls sss rels co-occur args location rels
			opposition dss attrs}) 
		 (root {rls sss rels co-occur args location rels opposition
		       names}))))))))) 
	 (onto none)))))
     (root
      ((surfsemcat rhetor)
       (struct hypotax)
       (root
	((surfsemcat rhetor)
	 (struct paratax)))
       (rels
	((instrument
	  ((dss {rls sss dss attrs stats ents stat0})
	   (surfsemcat encyclo)
	   (concept game-stat)
	   (restrictors
	    ((value 39)
	     (unit pt)))
	   (root
	    ((value 39)
	     (unit pt)))
	   (onto quantity)))))))
     (dss
      ((deepsemcat entity)
       (concept game)
       (token uta-at-bos)
       (attrs
	((floats
	  ((ents
	    ((addr
	      ((deepsemcat entity)
	       (concept address)
	       (token hartford-ct)
	       (attrs
		((city "hartford")
		 (state "ct")))))
	     (date
	      ((deepsemcat entity)
	       (concept date)
	       (token fri-nite)
	       (attrs
		((day-name "friday")
		 (day-part night)))))))))
	 (stats
	  ((ents
	    ((stat0-ca
	      ((deepsemcat entity)
	       (concept player)
	       (token kmalone)
	       (attrs
		((first-name "karl")
		 (last-name "malone")))))
	     (stat0
	      ((deepsemcat entity)
	       (concept game-stat)
	       (token kmalone-pt-at-bos)
	       (attrs
		((value 39)
		 (unit pt)))))
	     (histo-stat0
	      ((deepsemcat entity)
	       (concept histo-stat)
	       (token kmalone-pt-at-bos-ref-set)))
	     (histo-stat0-duration
	      ((deepsemcat entity)
	       (concept season)
	       (token kmalone-pt-at-bos-ref-set-duration)))
	     (histo-stat0-gen-elt
	      ((deepsemcat entity)
	       (concept game-stat)
	       (attrs
		((unit pt)))))
	     (histo-stat0-gen-elt-ca
	      ((deepsemcat entity)
	       (concept player)
	       (token kmalone)
	       (attrs
		((first-name "karl")
		 (last-name "malone")))))
	     (histo-stat0-extr
	      ((deepsemcat entity)
	       (concept integer)
	       (token kmalone-pt-at-bos-ref-set-extr)))))
	   (rels
	    ((stat0
	      ((deepsemcat relation)
	       (role game-stat-rel)
	       (token kmalone-scoring-at-bos)
	       (args
		((carrier {rls sss dss attrs stats ents stat0-ca})
		 (stat {rls sss dss attrs stats ents stat0})))))
	     (histo-stat0-duration
	      ((deepsemcat relation)
	       (role duration)
	       (token kmalone-pt-at-bos-ref-set-duration-rel)
	       (args
		((set {rls sss dss attrs stats ents histo-stat0})
		 (duration {rls sss dss attrs stats histo-stat0-duration})))))
	     (histo-stat0-gen-elt
	      ((deepsemcat relation)
	       (role gen-elt)
	       (token kmalone-pt-at-bos-ref-set-gen-elt)
	       (args
		((set {rls sss dss attrs stats ents histo-stat0})
		 (gen-elt {rls sss dss attrs stats ents
			  histo-stat0-gen-elt}))))) 
	     (histo-stat0-gen-elt-ca
	      ((deepsemcat relation)
	       (role game-stat-rel)
	       (token kmalone-pt-at-bos-ref-set-gen-elt-ca)
	       (args
		((carrier {rls sss dss attrs stats ents
			  histo-stat0-gen-elt-ca}) 
		 (stat {rls sss dss attrs stats ents histo-stat0-gen-elt})))))
	     (histo-stat0-extr
	      ((deepsemcat relation)
	       (role max-val)
	       (token kmalone-pt-at-bos-ref-set-extr-rel)
	       (args
		((extr-val {rls sss dss attrs stats ents histo-stat0-extr})
		 (set {rls sss dss attrs stats ents histo-stat0})))))
	     (histo-stat0-update
	      ((deepsemcat relation)
	       (role =)
	       (token kmalone-tie-pt-season-high-at-bos)
	       (args
		((stat-val 39)
		 (histo-stat-extr {rls sss dss attrs stats ents
				  histo-stat0-extr}))))))))) 
	 (results
	  ((ents
	    ((host
	      ((deepsemcat entity)
	       (concept team)
	       (token celts)
	       (attrs
		((home "boston")
		 (franchise "celtic")))))
	     (visitor
	      ((deepsemcat entity)
	       (concept team)
	       (token jazz)
	       (attrs
		((home "utah")
		 (franchise "jazz")))))
	     (score
	      ((deepsemcat entity)
	       (concept score)
	       (token uta-at-bos-score)
	       (attrs
		((win 98)
		 (lose 94)))))))
	   (rels
	    ((winner
	      ((deepsemcat relation)
	       (role winner)
	       (token uta-at-bos-winner)
	       (args
		((game top-level)
		 (winner {rls sss dss attrs results ents visitor})))))
	     (loser
	      ((deepsemcat relation)
	       (role loser)
	       (token uta-at-bos-loser)
	       (args
		((game top-level)
		 (loser {rls sss dss attrs results ents host})))))
	     (result
	      ((deepsemcat relation)
	       (role beat)
	       (token uta-at-bos-result)
	       (args
		((winner {rls sss dss attrs results ents visitor})
		 (loser {rls sss dss attrs results ents host})))))))))))))))
   (sss-root {rls sss root root})))))


;; Cycle is in:
;; ((DSS
;;  ((DEEPSEMCAT ENTITY) (CONCEPT PLAYER) (TOKEN KMALONE)
;;   (ATTRS ((FIRST-NAME "karl") (LAST-NAME "malone")))))
;; (SURFSEMCAT ENCYCLO) (ONTO INDIV) (CONCEPT PLAYER)
;; (NAMES ((FIRST-NAME "karl") (LAST-NAME "malone")))
;; (ROOT ((FIRST-NAME "karl") (LAST-NAME "malone")))
;; (SSS {BLS SSS ROOT ROOT ARGS AGENT}))   <--------------


;; Here try:
;; 
;; (relocate rel2 {rls sss-root elts cdr car dss})
;; (relocate rel2 {rls sss dss attrs stats rels stat1})
;; The 2 paths are conflated and should return the same relocate.
;;
(setf 
 rel2
 '((bls
    ((sss
      ((dss
	((deepsemcat entity) (concept game) (token uta-at-lac)
	 (attrs
	  ((floats
	    ((ents
	      ((addr
		((deepsemcat entity) (concept address) (token la)
		 (attrs ((city "los angeles")))))
	       (date
		((deepsemcat entity) (concept date) (token sat)
		 (attrs ((day-name "saturday")))))))))
	   (stats
	    ((ents
	      ((stat0-ca
		((deepsemcat entity) (concept player) (token kmalone)
		 (attrs ((first-name "karl") (last-name "malone")))))
	       (stat0
		((deepsemcat entity) (concept game-stat)
		 (token kmalone-pt-at-la)
		 (attrs ((value 28) (unit pt)))))))
	     (rels
	      ((stat0
		((deepsemcat relation) (role game-stat-rel)
		 (token kmalone-scoring-at-la)
		 (args
		  ((carrier {^ ^ ^ ^ ents stat0-ca})
		   (stat {^ ^ ^ ^ ents stat0})))))))))
	   (results
	    ((ents
	      ((host
		((deepsemcat entity) (concept team) (token clippers)
		 (attrs ((home "los angeles") (franchise "clipper")))))
	       (visitor
		((deepsemcat entity) (concept team) (token jazz)
		 (attrs ((home "utah") (franchise "jazz")))))
	       (score
		((deepsemcat entity) (concept score)
		 (token uta-at-lac-score)
		 (attrs ((win 127) (lose 111)))))))
	     (rels
	      ((winner
		((deepsemcat relation) (role winner)
		 (token uta-at-lac-winner)
		 (args
		  ((game top-level) (winner {^ ^ ^ ^ ents visitor})))))
	       (loser
		((deepsemcat relation) (role loser)
		 (token uta-at-lac-loser)
		 (args
		  ((game top-level) (loser {^ ^ ^ ^ ents host})))))
	       (result
		((deepsemcat relation) (role beat)
		 (token uta-at-lac-result)
		 (args
		  ((winner {^ ^ ^ ^ ents visitor})
		   (loser {^ ^ ^ ^ ents host})))))))))))))
       (surfsemcat rhetor) (struct hypotax)
       (root
	((surfsemcat rhetor) (struct hypotax)
	 (root
	  ((dss {^ ^ ^ dss attrs stats rels stat0})
	   (surfsemcat encyclo) (onto event) (concept {^ dss role})
	   (args
	    ((agent
	      ((dss {^ ^ ^ dss args carrier}) (surfsemcat encyclo)
	       (onto indiv) (concept {^ dss concept})
	       (names {^ dss attrs}) (root {^ names})))
	     (created
	      ((dss {^ ^ ^ dss args stat}) (surfsemcat encyclo)
	       (concept {^ dss concept}) (restrictors {^ dss attrs})
	       (root {^ restrictors}) (onto quantity)))
	     (fset (agent created affected range))))
	   (deepmap-cset ((= {^ args agent} {^ args created})))
	   (struct none)))
	 (rels
	  ((time ((dss {^ ^ ^ ^ dss attrs floats ents date})
		  (surfsemcat encyclo) (concept {^ dss concept})
		  (restrictors {^ dss attrs}) (root {^ restrictors})
		  (onto time)))))))
       (rels
	((location
	  ((dss {^ ^ ^ dss attrs floats ents addr})
	   (surfsemcat encyclo) (concept {^ dss concept})
	   (restrictors {^ dss attrs}) (root {^ restrictors})
	   (onto place)))
	 (co-occur
	  ((surfsemcat rhetor) (struct event) (root transf-loc)
	   (args
	    ((agent
	      ((dss {^ ^ ^ ^ ^ dss root root args carrier})
	       (fset (dss))))
	     (affected
	      ((dss
		{^ ^ ^ ^ ^ dss attrs results rels winner args winner})
	       (surfsemcat encyclo) (onto indiv)
	       (concept {^ dss concept}) (ref full)
	       (names {^ dss attrs}) (root {^ names})))
	     (located {^ affected})
	     (location
	      ((surfsemcat rhetor) (struct hypotax)
	       (focus {^ rels score})
	       (root
		((dss {^ ^ ^ ^ ^ ^ dss attrs results rels winner})
		 (surfsemcat encyclo) (concept {^ dss role})
		 (onto indiv)))
	       (rels
		((score
		  ((dss {^ ^ ^ ^ ^ ^ ^ dss attrs results ents score})
		   (surfsemcat encyclo) (concept {^ dss concept})
		   (restrictors {^ dss attrs}) (root {^ restrictors})
		   (onto quantity)))
		 (opposition
		  ((dss
		    {^ ^ ^ ^ ^ ^ ^ dss attrs results rels loser args loser})
		   (surfsemcat encyclo) (onto indiv)
		   (concept {^ dss concept}) (ref full)
		   (names {^ dss attrs}) (root {^ names})))))))))
	   (onto none)))))
       (deepmap-cset
	((= {^ root root}
	    {^ root rels time}
	    {^ rels location}
	    {^ rels co-occur args affected}
	    {^ rels co-occur args location root}
	    {^ rels co-occur args location rels score}
	    {^ rels co-occur args location rels opposition})))))
     (sss-root {^ sss root root}) (cat clause) (tense past)
     (fills none)
     (circum
      ((location
	((sss {^ ^ ^ sss rels location}) (position header)
	 (sss-root {^ sss}) (cat address)
	 (city ((lex {^ ^ sss restrictors city})))))
       (time ((sss {^ ^ ^ sss root rels time}) (fills time-rel)
	      (sss-root {^ sss}) (cat date)
	      (day-name ((lex {^ ^ sss restrictors day-name})))))
       (co-event
	((sss {^ ^ ^ sss rels co-occur}) (position end)
	 (mood present-participle) (sss-root {^ sss}) (cat clause)
	 (tense past) (fills none) (controlled {^ partic agent})
	 (proc
	  ((type composite) (relation-type locative) (lex "power")))
	 (partic
	  ((located
	    ((sss {^ ^ ^ sss-root args located}) (sss-root {^ sss})
	     (cat compound-proper)
	     (head
	      ((cat team-name)
	       (home ((lex {^ ^ ^ sss-root names home})))
	       (franchise ((lex {^ ^ ^ sss-root names franchise})))))
	     (number plural)))
	   (affected {^ located})
	   (location
	    ((cat pp)
	     (np
	      ((sss {^ ^ ^ ^ sss-root args location})
	       (sss-root {^ sss root}) (cat common) (definite no)
	       (classifier
		((sss {^ ^ sss rels score}) (synt-funct classifier)
		 (sss-root {^ sss}) (cat score)
		 (win ((value {^ ^ sss restrictors win})))
		 (lose ((value {^ ^ sss restrictors lose})))))
	       (surfmap-cset
		((+ {circum co-event partic location np classifier}
		    {circum co-event partic location np qualifier np})
		 (-)))
	       (qualifier
		((cat pp) (prep ((lex "against")))
		 (np
		  ((sss {^ ^ ^ sss rels opposition}) (sss-root {^ sss})
		   (cat compound-proper)
		   (head
		    ((cat team-name)
		     (home ((lex {^ ^ ^ sss-root names home})))
		     (franchise
		      ((lex {^ ^ ^ sss-root names franchise})))))
		   (number plural)))))
	       (head ((lex "win")))))
	     (prep ((lex "to")))))))
	 (surfmap-cset
	  ((+ {circum co-event partic located}
	      {circum co-event partic location np})
	   (-)))))))
     (surfmap-cset
      ((+ {circum location}
	  {circum time}
	  {circum co-event}
	  {partic agent}
	  {partic created})
       (-)))
     (partic
      ((agent
	((sss {^ ^ ^ sss-root args agent}) (sss-root {^ sss})
	 (cat compound-proper)
	 (head
	  ((cat person-name)
	   (first-name ((lex {^ ^ ^ sss names first-name})))
	   (last-name ((lex {^ ^ ^ sss names last-name})))))))
       (created
	((sss {^ ^ ^ sss-root args created}) (sss-root {^ sss})
	 (cat measure) (quantity ((value {^ ^ sss restrictors value})))
	 (unit ((lex "point")))))))
     (proc ((type material) (effect-type creative) (lex "provide")))))
   (adss
    ((deepsemcat entity) (concept game) (token uta-at-lac)
     (attrs
      ((stats
	((ents
	  ((stat1-ca
	    ((deepsemcat entity) (concept player) (token stockton)
	     (attrs ((first-name "john") (last-name "stockton")))))
	   (stat1
	    ((deepsemcat entity) (concept game-stat)
	     (token stockton-pt-at-lac)
	     (attrs ((value 27) (unit pt)))))))
	 (rels
	  ((stat1
	    ((deepsemcat relation) (role game-stat-rel)
	     (token stockton-scoring-at-lac)
	     (args
	      ((carrier {^ ^ ^ ^ ents stat1-ca})
	       (stat {^ ^ ^ ^ ents stat1})))))))))))))
   (rls
    ((cat clause) (complex conjunction)
     (sss
      ((dss
	((deepsemcat entity) (concept game) (token uta-at-lac)
	 (attrs
	  ((floats
	    ((ents
	      ((addr
		((deepsemcat entity) (concept address) (token la)
		 (attrs ((city "los angeles")))))
	       (date
		((deepsemcat entity) (concept date) (token sat)
		 (attrs ((day-name "saturday")))))))))
	   (stats
	    ((ents
	      ((stat0-ca
		((deepsemcat entity) (concept player) (token kmalone)
		 (attrs ((first-name "karl") (last-name "malone")))))
	       (stat0
		((deepsemcat entity) (concept game-stat)
		 (token kmalone-pt-at-la)
		 (attrs ((value 28) (unit pt)))))
	       (stat1-ca
		((deepsemcat entity) (concept player) (token stockton)
		 (attrs ((first-name "john") (last-name "stockton")))))
	       (stat1
		((deepsemcat entity) (concept game-stat)
		 (token stockton-pt-at-lac)
		 (attrs ((value 27) (unit pt)))))))
	     (rels
	      ((stat0
		((deepsemcat relation) (role game-stat-rel)
		 (token kmalone-scoring-at-la)
		 (args
		  ((carrier {^ ^ ^ ^ ents stat0-ca})
		   (stat {^ ^ ^ ^ ents stat0})))))
	       (stat1
		((deepsemcat relation) (role game-stat-rel)
		 (token stockton-scoring-at-lac)
		 (args
		  ((carrier {^ ^ ^ ^ ents stat1-ca})
		   (stat {^ ^ ^ ^ ents stat1})))))))))
	   (results
	    ((ents
	      ((host
		((deepsemcat entity) (concept team) (token clippers)
		 (attrs ((home "los angeles") (franchise "clipper")))))
	       (visitor
		((deepsemcat entity) (concept team) (token jazz)
		 (attrs ((home "utah") (franchise "jazz")))))
	       (score
		((deepsemcat entity) (concept score)
		 (token uta-at-lac-score)
		 (attrs ((win 127) (lose 111)))))))
	     (rels
	      ((winner
		((deepsemcat relation) (role winner)
		 (token uta-at-lac-winner)
		 (args
		  ((game top-level) (winner {^ ^ ^ ^ ents visitor})))))
	       (loser
		((deepsemcat relation) (role loser)
		 (token uta-at-lac-loser)
		 (args
		  ((game top-level) (loser {^ ^ ^ ^ ents host})))))
	       (result
		((deepsemcat relation) (role beat)
		 (token uta-at-lac-result)
		 (args
		  ((winner {^ ^ ^ ^ ents visitor})
		   (loser {^ ^ ^ ^ ents host})))))))))))))
       (surfsemcat rhetor) (struct hypotax)
       (root
	((surfsemcat rhetor) (struct hypotax) (root {^ sss root root})
	 (rels
	  ((time ((dss {^ ^ ^ ^ dss attrs floats ents date})
		  (surfsemcat encyclo) (concept {^ dss concept})
		  (restrictors {^ dss attrs}) (root {^ restrictors})
		  (onto time)))))
	 (sss
	  ((root
	    ((root
	      ((rel teammate) (surfsemcat rhetor) (struct paratax)
	       (elts
		((car ((surfsemcat encyclo) (onto event)
		       (concept {^ dss role})
		       (args
			((agent
			  ((dss {^ ^ ^ dss args carrier})
			   (surfsemcat encyclo) (onto indiv)
			   (concept {^ dss concept})
			   (names {^ dss attrs}) (root {^ names})))
			 (created
			  ((dss {^ ^ ^ dss args stat})
			   (surfsemcat encyclo)
			   (concept {^ dss concept})
			   (restrictors {^ dss attrs})
			   (root {^ restrictors}) (onto quantity)))
			 (fset (agent created affected range))))
		       (deepmap-cset
			((= {^ args agent} {^ args created})))
		       (struct none)
		       (dss {^ ^ ^ dss attrs stats rels stat0})))
		 (cdr ((car ((surfsemcat encyclo) (onto event)
			     (concept {^ dss role})
			     (args
			      ((agent
				((dss {^ ^ ^ dss args carrier})
				 (surfsemcat encyclo)
				 (onto indiv)
				 (concept {^ dss concept})
				 (names {^ dss attrs})
				 (root {^ names})))
			       (created
				((dss {^ ^ ^ dss args stat})
				 (surfsemcat encyclo)
				 (concept {^ dss concept})
				 (restrictors {^ dss attrs})
				 (root {^ restrictors})
				 (onto quantity)))))
			     (deepmap-cset
			      ((= {^ args agent} {^ args created})))
			     (dss
			      {^ ^ ^ ^ ^ ^ ^ ^ dss attrs stats rels
			      stat1}))))))))))))))) 
       (rels
	((location
	  ((dss {^ ^ ^ dss attrs floats ents addr})
	   (surfsemcat encyclo) (concept {^ dss concept})
	   (restrictors {^ dss attrs}) (root {^ restrictors})
	   (onto place)))
	 (co-occur
	  ((surfsemcat rhetor) (struct event) (root transf-loc)
	   (args
	    ((agent
	      ((dss {^ ^ ^ ^ ^ dss root root args carrier})
	       (fset (dss))))
	     (affected
	      ((dss
		{^ ^ ^ ^ ^ dss attrs results rels winner args winner})
	       (surfsemcat encyclo) (onto indiv)
	       (concept {^ dss concept}) (ref full)
	       (names {^ dss attrs}) (root {^ names})))
	     (located {^ affected})
	     (location
	      ((surfsemcat rhetor) (struct hypotax)
	       (focus {^ rels score})
	       (root
		((dss {^ ^ ^ ^ ^ ^ dss attrs results rels winner})
		 (surfsemcat encyclo) (concept {^ dss role})
		 (onto indiv)))
	       (rels
		((score
		  ((dss {^ ^ ^ ^ ^ ^ ^ dss attrs results ents score})
		   (surfsemcat encyclo) (concept {^ dss concept})
		   (restrictors {^ dss attrs}) (root {^ restrictors})
		   (onto quantity)))
		 (opposition
		  ((dss
		    {^ ^ ^ ^ ^ ^ ^ dss attrs results rels loser args loser})
		   (surfsemcat encyclo) (onto indiv)
		   (concept {^ dss concept}) (ref full)
		   (names {^ dss attrs}) (root {^ names})))))))))
	   (onto none)))))
       (deepmap-cset
	((= {^ root root}
	    {^ root rels time}
	    {^ rels location}
	    {^ rels co-occur args affected}
	    {^ rels co-occur args location root}
	    {^ rels co-occur args location rels score}
	    {^ rels co-occur args location rels opposition})))))
     (sss-root {^ sss root root}) (tense past) (fills none)
     (circum
      ((location
	((sss
	  ((dss
	    ((deepsemcat entity) (concept address) (token la)
	     (attrs ((city "los angeles")))))
	   (surfsemcat encyclo) (concept address)
	   (restrictors ((city "los angeles")))
	   (root ((city "los angeles"))) (onto place)))
	 (position header) (sss-root {^ sss}) (cat address)
	 (city ((lex {^ ^ sss restrictors city})))))
       (time ((sss
	       ((dss
		 ((deepsemcat entity) (concept date) (token sat)
		  (attrs ((day-name "saturday")))))
		(surfsemcat encyclo) (concept date)
		(restrictors ((day-name "saturday")))
		(root ((day-name "saturday"))) (onto time)))
	      (fills time-rel) (sss-root {^ sss}) (cat date)
	      (day-name ((lex {^ ^ sss restrictors day-name})))))
       (co-event
	((sss
	  ((surfsemcat rhetor) (struct event) (root transf-loc)
	   (args
	    ((agent ((fset (dss))))
	     (affected
	      ((dss
		((deepsemcat entity) (concept team) (token jazz)
		 (attrs ((home "utah") (franchise "jazz")))))
	       (surfsemcat encyclo) (onto indiv) (concept team)
	       (ref full) (names ((home "utah") (franchise "jazz")))
	       (root ((home "utah") (franchise "jazz")))))
	     (located
	      ((dss
		((deepsemcat entity) (concept team) (token jazz)
		 (attrs ((home "utah") (franchise "jazz")))))
	       (surfsemcat encyclo) (onto indiv) (concept team)
	       (ref full) (names ((home "utah") (franchise "jazz")))
	       (root ((home "utah") (franchise "jazz")))))
	     (location
	      ((surfsemcat rhetor) (struct hypotax)
	       (focus
		((dss
		  ((deepsemcat entity) (concept score)
		   (token uta-at-lac-score)
		   (attrs ((win 127) (lose 111)))))
		 (surfsemcat encyclo) (concept score)
		 (restrictors ((win 127) (lose 111)))
		 (root ((win 127) (lose 111))) (onto quantity)))
	       (root
		((dss
		  ((deepsemcat relation) (role winner)
		   (token uta-at-lac-winner)
		   (args
		    ((game top-level)
		     (winner
		      ((deepsemcat entity) (concept team) (token jazz)
		       (attrs ((home "utah") (franchise "jazz")))))))))
		 (surfsemcat encyclo) (concept winner) (onto indiv)))
	       (rels
		((score
		  ((dss
		    ((deepsemcat entity) (concept score)
		     (token uta-at-lac-score)
		     (attrs ((win 127) (lose 111)))))
		   (surfsemcat encyclo) (concept score)
		   (restrictors ((win 127) (lose 111)))
		   (root ((win 127) (lose 111))) (onto quantity)))
		 (opposition
		  ((dss
		    ((deepsemcat entity) (concept team)
		     (token clippers)
		     (attrs
		      ((home "los angeles") (franchise "clipper")))))
		   (surfsemcat encyclo) (onto indiv) (concept team)
		   (ref full)
		   (names ((home "los angeles") (franchise "clipper")))
		   (root
		    ((home "los angeles")
		     (franchise "clipper")))))))))))
	   (onto none)))
	 (position end) (mood present-participle) (sss-root {^ sss})
	 (cat clause) (tense past) (fills none)
	 (controlled {^ partic agent})
	 (proc
	  ((type composite) (relation-type locative) (lex "power")))
	 (partic
	  ((located
	    ((sss {^ ^ ^ sss-root args located}) (sss-root {^ sss})
	     (cat compound-proper)
	     (head
	      ((cat team-name)
	       (home ((lex {^ ^ ^ sss-root names home})))
	       (franchise ((lex {^ ^ ^ sss-root names franchise})))))
	     (number plural)))
	   (affected {^ located})
	   (location
	    ((cat pp)
	     (np
	      ((sss {^ ^ ^ ^ sss-root args location})
	       (sss-root {^ sss root}) (cat common) (definite no)
	       (classifier
		((sss {^ ^ sss rels score}) (synt-funct classifier)
		 (sss-root {^ sss}) (cat score)
		 (win ((value {^ ^ sss restrictors win})))
		 (lose ((value {^ ^ sss restrictors lose})))))
	       (surfmap-cset
		((+ {circum co-event partic location np classifier}
		    {circum co-event partic location np qualifier np})
		 (-)))
	       (qualifier
		((cat pp) (prep ((lex "against")))
		 (np
		  ((sss {^ ^ ^ sss rels opposition}) (sss-root {^ sss})
		   (cat compound-proper)
		   (head
		    ((cat team-name)
		     (home ((lex {^ ^ ^ sss-root names home})))
		     (franchise
		      ((lex {^ ^ ^ sss-root names franchise})))))
		   (number plural)))))
	       (head ((lex "win")))))
	     (prep ((lex "to")))))))
	 (surfmap-cset
	  ((+ {circum co-event partic located}
	      {circum co-event partic location np})
	   (-)))))
       (sss
	((rels
	  ((location {^ ^ ^ location sss})
	   (co-occur {^ ^ ^ co-event sss})))
	 (root ((rels ((time {^ ^ ^ ^ time sss})))))))))
     (surfmap-cset
      ((+ {circum location}
	  {circum time}
	  {circum co-event}
	  {partic agent}
	  {partic created})
       (-)))
     (partic
      ((agent
	((sss {^ ^ ^ sss-root args agent}) (sss-root {^ sss})
	 (cat compound-proper)
	 (head
	  ((cat person-name)
	   (first-name ((lex {^ ^ ^ sss names first-name})))
	   (last-name ((lex {^ ^ ^ sss names last-name})))))))
       (created
	((sss {^ ^ ^ sss-root args created}) (sss-root {^ sss})
	 (cat measure) (quantity ((value {^ ^ sss restrictors value})))
	 (unit ((lex "point")))))))
     (proc ((type material) (effect-type creative) (lex "provide")))
     (distinct
      ((car ((sss-root {^ sss root root}) (cat clause) (tense past)
           (fills none)
           (surfmap-cset
            ((+ {circum location}
                {circum time}
                {circum co-event}
                {partic agent}
                {partic created})
             (-)))
           (partic
            ((agent
              ((sss {^ ^ ^ sss-root args agent}) (sss-root {^ sss})
               (cat compound-proper)
               (head
                ((cat person-name)
                 (first-name ((lex {^ ^ ^ sss names first-name})))
                 (last-name ((lex {^ ^ ^ sss names last-name})))))))
             (created
              ((sss {^ ^ ^ sss-root args created}) (sss-root {^ sss})
               (cat measure)
               (quantity ((value {^ ^ sss restrictors value})))
               (unit ((lex "point")))))))
           (proc
            ((type material) (effect-type creative) (lex "provide")))
           (sss {^ ^ ^ ^ sss root root elts car})))))))))


;; > (relocate rel3 {distinct car})
;; gives a bad value to {sss} and {sss-root}.
;; In (top-gdp rel3 {distinct car}) get
;; (sss {^ ^ ^ sss root root elts car})
;; (sss-root {^ sss})
;;
;; In relocate, get:
;; (sss {^ sss-root})
;; (sss-root {^ sss})
;;
(setf 
 rel3
 '((sss
  ((dss
    ((deepsemcat entity) (concept game) (token uta-at-lac)
     (attrs
      ((floats
        ((ents
          ((addr
            ((deepsemcat entity) (concept address) (token la)
             (attrs ((city "los angeles")))))
           (date
            ((deepsemcat entity) (concept date) (token sat)
             (attrs ((day-name "saturday")))))))))
       (stats
        ((ents
          ((stat0-ca
            ((deepsemcat entity) (concept player) (token kmalone)
             (attrs ((first-name "karl") (last-name "malone")))))
           (stat0
            ((deepsemcat entity) (concept game-stat)
             (token kmalone-pt-at-la)
             (attrs ((value 28) (unit pt)))))
           (stat1-ca
            ((deepsemcat entity) (concept player) (token stockton)
             (attrs ((first-name "john") (last-name "stockton")))))
           (stat1
            ((deepsemcat entity) (concept game-stat)
             (token stockton-pt-at-lac)
             (attrs ((value 27) (unit pt)))))))
         (rels
          ((stat0
            ((deepsemcat relation) (role game-stat-rel)
             (token kmalone-scoring-at-la)
             (args
              ((carrier {^ ^ ^ ^ ents stat0-ca})
               (stat {^ ^ ^ ^ ents stat0})))))
           (stat1
            ((deepsemcat relation) (role game-stat-rel)
             (token stockton-scoring-at-lac)
             (args
              ((carrier
                ((deepsemcat entity) (concept player)
                 (token stockton)
                 (attrs
                  ((first-name "john") (last-name "stockton")))))
               (stat
                ((deepsemcat entity) (concept game-stat)
                 (token stockton-pt-at-lac)
                 (attrs ((value 27) (unit pt)))))))))))))
       (results
        ((ents
          ((host
            ((deepsemcat entity) (concept team) (token clippers)
             (attrs ((home "los angeles") (franchise "clipper")))))
           (visitor
            ((deepsemcat entity) (concept team) (token jazz)
             (attrs ((home "utah") (franchise "jazz")))))
           (score
            ((deepsemcat entity) (concept score)
             (token uta-at-lac-score)
             (attrs ((win 127) (lose 111)))))))
         (rels
          ((winner
            ((deepsemcat relation) (role winner)
             (token uta-at-lac-winner)
             (args
              ((game top-level) (winner {^ ^ ^ ^ ents visitor})))))
           (loser
            ((deepsemcat relation) (role loser)
             (token uta-at-lac-loser)
             (args ((game top-level) (loser {^ ^ ^ ^ ents host})))))
           (result
            ((deepsemcat relation) (role beat)
             (token uta-at-lac-result)
             (args
              ((winner {^ ^ ^ ^ ents visitor})
               (loser {^ ^ ^ ^ ents host})))))))))))))
   (surfsemcat rhetor) (struct hypotax)
   (root
    ((surfsemcat rhetor) (struct hypotax)
     (rels
      ((time ((dss {^ ^ ^ ^ dss attrs floats ents date})
              (surfsemcat encyclo) (concept {^ dss concept})
              (restrictors {^ dss attrs}) (root {^ restrictors})
              (onto time)))))
     (root
      ((surfsemcat rhetor) (struct paratax)
       (elts
        ((car ((surfsemcat encyclo) (onto event)
               (concept {^ ^ ^ ^ ^ ^ sss-root elts car dss role})
               (args
                ((agent
                  ((dss
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car dss args carrier})
                   (surfsemcat encyclo) (onto indiv)
                   (concept
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args agent dss concept})
                   (names
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args agent dss attrs})
                   (root
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args agent names})))
                 (created
                  ((dss
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car dss args stat})
                   (surfsemcat encyclo)
                   (concept
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args created dss concept})
                   (restrictors
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args created dss attrs})
                   (root
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args created restrictors})
                   (onto quantity)))
                 (fset (agent created affected range))))
               (deepmap-cset ((= {^ args agent} {^ args created})))
               (struct none)
               (dss {^ ^ ^ ^ ^ dss attrs stats rels stat0})))
         (cdr ((car ((surfsemcat encyclo) (onto event)
                     (concept
                      {^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car dss role})
                     (args
                      ((agent
                        ((dss
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car dss args carrier})
                         (surfsemcat encyclo) (onto indiv)
                         (concept
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args agent dss concept})
                         (names
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args agent dss attrs})
                         (root
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args agent names})))
                       (created
                        ((dss
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car dss args stat})
                         (surfsemcat encyclo)
                         (concept
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args created dss concept})
                         (restrictors
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args created dss attrs})
                         (root
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args created restrictors})
                         (onto quantity)))))
                     (deepmap-cset
                      ((= {^ args agent} {^ args created})))
                     (dss
                      {^ ^ ^ ^ ^ ^ dss attrs stats rels stat1})))))))
       (rel teammate)))))
   (rels
    ((location
      ((dss {^ ^ ^ dss attrs floats ents addr}) (surfsemcat encyclo)
       (concept {^ dss concept}) (restrictors {^ dss attrs})
       (root {^ restrictors}) (onto place)))
     (co-occur
      ((surfsemcat rhetor) (struct event) (root transf-loc)
       (args
        ((agent
          ((dss {^ ^ ^ ^ ^ dss root root args carrier})
           (fset (dss))))
         (affected
          ((dss
            {^ ^ ^ ^ ^ dss attrs results rels winner args winner})
           (surfsemcat encyclo) (onto indiv)
           (concept {^ dss concept}) (ref full) (names {^ dss attrs})
           (root {^ names})))
         (located {^ affected})
         (location
          ((surfsemcat rhetor) (struct hypotax)
           (focus {^ rels score})
           (root
            ((dss {^ ^ ^ ^ ^ ^ dss attrs results rels winner})
             (surfsemcat encyclo) (concept {^ dss role})
             (onto indiv)))
           (rels
            ((score
              ((dss {^ ^ ^ ^ ^ ^ ^ dss attrs results ents score})
               (surfsemcat encyclo) (concept {^ dss concept})
               (restrictors {^ dss attrs}) (root {^ restrictors})
               (onto quantity)))
             (opposition
              ((dss
                {^ ^ ^ ^ ^ ^ ^ dss attrs results rels loser args loser})
               (surfsemcat encyclo) (onto indiv)
               (concept {^ dss concept}) (ref full)
               (names {^ dss attrs}) (root {^ names})))))))))
       (onto none)))))
   (deepmap-cset
    ((= {^ root root}
        {^ root rels time}
        {^ rels location}
        {^ rels co-occur args affected}
        {^ rels co-occur args location root}
        {^ rels co-occur args location rels score}
        {^ rels co-occur args location rels opposition})))))
 (sss-root {^ sss root root}) (cat clause) (complex conjunction)
 (distinct
  ((car ((sss-root {^ sss}) (cat clause) (tense past) (fills none)
         (surfmap-cset
          ((+ {circum location}
              {circum time}
              {circum co-event}
              {partic agent}
              {partic created})
           (-)))
         (partic
          ((agent
            ((sss {^ ^ ^ sss-root args agent}) (sss-root {^ sss})
             (cat compound-proper)
             (head
              ((cat person-name)
               (first-name ((lex {^ ^ ^ sss names first-name})))
               (last-name ((lex {^ ^ ^ sss names last-name})))))))
           (created
            ((sss {^ ^ ^ sss-root args created}) (sss-root {^ sss})
             (cat measure)
             (quantity ((value {^ ^ sss restrictors value})))
             (unit ((lex "point")))))))
         (proc ((type material) (effect-type creative) (lex "net")))
         (SSS {^ ^ ^ SSS ROOT ROOT ELTS CAR})))  ;; <<-- SEE, I TOLD YOU SO
   (cdr ((car ((stat-num 1)
               (partic
                ((created
                  ((unit ((gap yes) (lex "point")))
                   (sss {^ ^ ^ sss-root args created})
                   (sss-root {^ sss}) (cat measure)
                   (quantity ((value {^ ^ sss restrictors value})))))
                 (agent
                  ((sss {^ ^ ^ sss-root args agent})
                   (sss-root {^ sss}) (cat compound-proper)
                   (head
                    ((cat person-name)
                     (first-name
                      ((lex {^ ^ ^ sss names first-name})))
                     (last-name
                      ((lex {^ ^ ^ sss names last-name})))))))))
               (sss-root {^ sss}) (cat clause) (tense past)
               (fills none)
               (surfmap-cset
                ((+ {partic agent} {partic created}) (-)))
               (proc
                ((type material) (effect-type creative) (lex "add")))
               (sss {^ ^ ^ ^ sss-root elts cdr car})))))))
 (circum
  ((location
    ((sss
      ((dss {^ ^ ^ dss attrs floats ents addr}) (surfsemcat encyclo)
       (concept {^ dss concept}) (restrictors {^ dss attrs})
       (root {^ restrictors}) (onto place)))
     (position header) (sss-root {^ sss}) (cat address)
     (city ((lex {^ ^ sss restrictors city})))))
   (time ((sss
           ((surfsemcat encyclo) (concept {^ dss concept})
            (restrictors {^ dss attrs}) (root {^ restrictors})
            (onto time)))
          (fills time-rel) (sss-root {^ sss}) (cat date)
          (day-name ((lex {^ ^ sss restrictors day-name})))))
   (co-event
    ((sss
      ((surfsemcat rhetor) (struct event) (root transf-loc)
       (args
        ((agent
          ((dss {^ ^ ^ ^ ^ dss root root args carrier})
           (fset (dss))))
         (affected
          ((dss
            {^ ^ ^ ^ ^ dss attrs results rels winner args winner})
           (surfsemcat encyclo) (onto indiv)
           (concept {^ dss concept}) (ref full) (names {^ dss attrs})
           (root {^ names})))
         (located {^ affected})
         (location
          ((surfsemcat rhetor) (struct hypotax)
           (focus {^ rels score})
           (root
            ((dss {^ ^ ^ ^ ^ ^ dss attrs results rels winner})
             (surfsemcat encyclo) (concept {^ dss role})
             (onto indiv)))
           (rels
            ((score
              ((dss {^ ^ ^ ^ ^ ^ ^ dss attrs results ents score})
               (surfsemcat encyclo) (concept {^ dss concept})
               (restrictors {^ dss attrs}) (root {^ restrictors})
               (onto quantity)))
             (opposition
              ((dss
                {^ ^ ^ ^ ^ ^ ^ dss attrs results rels loser args loser})
               (surfsemcat encyclo) (onto indiv)
               (concept {^ dss concept}) (ref full)
               (names {^ dss attrs}) (root {^ names})))))))))
       (onto none)))
     (position end) (mood present-participle) (sss-root {^ sss})
     (cat clause) (tense past) (fills none)
     (controlled {^ partic agent})
     (proc ((type composite) (relation-type locative) (lex "power")))
     (partic
      ((located
        ((sss {^ ^ ^ sss-root args located}) (sss-root {^ sss})
         (cat compound-proper)
         (head
          ((cat team-name) (home ((lex {^ ^ ^ sss-root names home})))
           (franchise ((lex {^ ^ ^ sss-root names franchise})))))
         (number plural)))
       (affected {^ located})
       (location
        ((cat pp)
         (np
          ((sss {^ ^ ^ ^ sss-root args location})
           (sss-root {^ sss root}) (cat common) (definite no)
           (classifier
            ((sss {^ ^ sss rels score}) (synt-funct classifier)
             (sss-root {^ sss}) (cat score)
             (win ((value {^ ^ sss restrictors win})))
             (lose ((value {^ ^ sss restrictors lose})))))
           (surfmap-cset
            ((+ {circum co-event partic location np classifier}
                {circum co-event partic location np qualifier np})
             (-)))
           (qualifier
            ((cat pp) (prep ((lex "over")))
             (np
              ((sss {^ ^ ^ sss rels opposition}) (sss-root {^ sss})
               (cat compound-proper)
               (head
                ((cat team-name)
                 (home ((lex {^ ^ ^ sss-root names home})))
                 (franchise
                  ((lex {^ ^ ^ sss-root names franchise})))))
               (number plural)))))
           (head ((lex "win")))))
         (prep ((lex "to")))))))
     (surfmap-cset
      ((+ {circum co-event partic located}
          {circum co-event partic location np})
       (-)))))))))

(setf rel4
      '((sss
  ((dss
    ((deepsemcat entity) (concept game) (token uta-at-lac)
     (attrs
      ((floats
        ((ents
          ((addr
            ((deepsemcat entity) (concept address) (token la)
             (attrs ((city "los angeles")))))
           (date
            ((deepsemcat entity) (concept date) (token sat)
             (attrs ((day-name "saturday")))))))))
       (stats
        ((ents
          ((stat0-ca
            ((deepsemcat entity) (concept player) (token kmalone)
             (attrs ((first-name "karl") (last-name "malone")))))
           (stat0
            ((deepsemcat entity) (concept game-stat)
             (token kmalone-pt-at-la)
             (attrs ((value 28) (unit pt)))))
           (stat1-ca
            ((deepsemcat entity) (concept player) (token stockton)
             (attrs ((first-name "john") (last-name "stockton")))))
           (stat1
            ((deepsemcat entity) (concept game-stat)
             (token stockton-pt-at-lac)
             (attrs ((value 27) (unit pt)))))))
         (rels
          ((stat0
            ((deepsemcat relation) (role game-stat-rel)
             (token kmalone-scoring-at-la)
             (args
              ((carrier {^ ^ ^ ^ ents stat0-ca})
               (stat {^ ^ ^ ^ ents stat0})))))
           (stat1
            ((deepsemcat relation) (role game-stat-rel)
             (token stockton-scoring-at-lac)
             (args
              ((carrier
                ((deepsemcat entity) (concept player)
                 (token stockton)
                 (attrs
                  ((first-name "john") (last-name "stockton")))))
               (stat
                ((deepsemcat entity) (concept game-stat)
                 (token stockton-pt-at-lac)
                 (attrs ((value 27) (unit pt)))))))))))))
       (results
        ((ents
          ((host
            ((deepsemcat entity) (concept team) (token clippers)
             (attrs ((home "los angeles") (franchise "clipper")))))
           (visitor
            ((deepsemcat entity) (concept team) (token jazz)
             (attrs ((home "utah") (franchise "jazz")))))
           (score
            ((deepsemcat entity) (concept score)
             (token uta-at-lac-score)
             (attrs ((win 127) (lose 111)))))))
         (rels
          ((winner
            ((deepsemcat relation) (role winner)
             (token uta-at-lac-winner)
             (args
              ((game top-level) (winner {^ ^ ^ ^ ents visitor})))))
           (loser
            ((deepsemcat relation) (role loser)
             (token uta-at-lac-loser)
             (args ((game top-level) (loser {^ ^ ^ ^ ents host})))))
           (result
            ((deepsemcat relation) (role beat)
             (token uta-at-lac-result)
             (args
              ((winner {^ ^ ^ ^ ents visitor})
               (loser {^ ^ ^ ^ ents host})))))))))))))
   (surfsemcat rhetor) (struct hypotax)
   (root
    ((surfsemcat rhetor) (struct hypotax)
     (rels
      ((time ((dss {^ ^ ^ ^ dss attrs floats ents date})
              (surfsemcat encyclo) (concept {^ dss concept})
              (restrictors {^ dss attrs}) (root {^ restrictors})
              (onto time)))))
     (root
      ((surfsemcat rhetor) (struct paratax)
       (elts
        ((car ((dss {^ ^ ^ ^ ^ dss attrs stats rels stat0})
               (surfsemcat encyclo) (onto event)
               (concept {^ ^ ^ ^ ^ ^ sss-root elts car dss role})
               (args
                ((agent
                  ((dss
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car dss args carrier})
                   (surfsemcat encyclo) (onto indiv)
                   (concept
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args agent dss concept})
                   (names
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args agent dss attrs})
                   (root
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args agent names})))
                 (created
                  ((dss
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car dss args stat})
                   (surfsemcat encyclo)
                   (concept
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args created dss concept})
                   (restrictors
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args created dss attrs})
                   (root
                    {^ ^ ^ ^ ^ ^ ^ ^ sss-root elts car args created restrictors})
                   (onto quantity)))
                 (fset (agent created affected range))))
               (deepmap-cset ((= {^ args agent} {^ args created})))
               (struct none)))
         (cdr ((car ((surfsemcat encyclo) (onto event)
                     (concept
                      {^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car dss role})
                     (args
                      ((agent
                        ((dss
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car dss args carrier})
                         (surfsemcat encyclo) (onto indiv)
                         (concept
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args agent dss concept})
                         (names
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args agent dss attrs})
                         (root
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args agent names})))
                       (created
                        ((dss
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car dss args stat})
                         (surfsemcat encyclo)
                         (concept
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args created dss concept})
                         (restrictors
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args created dss attrs})
                         (root
                          {^ ^ ^ ^ ^ ^ ^ ^ ^ sss-root elts cdr car args created restrictors})
                         (onto quantity)))))
                     (deepmap-cset
                      ((= {^ args agent} {^ args created})))
                     (dss
                      {^ ^ ^ ^ ^ ^ dss attrs stats rels stat1})))))))
       (rel teammate)))))
   (rels
    ((location
      ((dss {^ ^ ^ dss attrs floats ents addr}) (surfsemcat encyclo)
       (concept {^ dss concept}) (restrictors {^ dss attrs})
       (root {^ restrictors}) (onto place)))
     (co-occur
      ((surfsemcat rhetor) (struct event) (root transf-loc)
       (args
        ((agent
          ((dss {^ ^ ^ ^ ^ dss root root args carrier})
           (fset (dss))))
         (affected
          ((dss
            {^ ^ ^ ^ ^ dss attrs results rels winner args winner})
           (surfsemcat encyclo) (onto indiv)
           (concept {^ dss concept}) (ref full) (names {^ dss attrs})
           (root {^ names})))
         (located {^ affected})
         (location
          ((surfsemcat rhetor) (struct hypotax)
           (focus {^ rels score})
           (root
            ((dss {^ ^ ^ ^ ^ ^ dss attrs results rels winner})
             (surfsemcat encyclo) (concept {^ dss role})
             (onto indiv)))
           (rels
            ((score
              ((dss {^ ^ ^ ^ ^ ^ ^ dss attrs results ents score})
               (surfsemcat encyclo) (concept {^ dss concept})
               (restrictors {^ dss attrs}) (root {^ restrictors})
               (onto quantity)))
             (opposition
              ((dss
                {^ ^ ^ ^ ^ ^ ^ dss attrs results rels loser args loser})
               (surfsemcat encyclo) (onto indiv)
               (concept {^ dss concept}) (ref full)
               (names {^ dss attrs}) (root {^ names})))))))))
       (onto none)))))
   (deepmap-cset
    ((= {^ root root}
        {^ root rels time}
        {^ rels location}
        {^ rels co-occur args affected}
        {^ rels co-occur args location root}
        {^ rels co-occur args location rels score}
        {^ rels co-occur args location rels opposition})))))
 (sss-root {^ sss root root}) (cat clause) (complex conjunction)
 (distinct
  ((car ((sss-root {^ sss}) (cat clause) (tense past) (fills none)
         (surfmap-cset
          ((+ {circum location}
              {circum time}
              {circum co-event}
              {partic agent}
              {partic created})
           (-)))
         (partic
          ((agent
            ((sss {^ ^ ^ sss-root args agent}) (sss-root {^ sss})
             (cat compound-proper)
             (head
              ((cat person-name)
               (first-name ((lex {^ ^ ^ sss names first-name})))
               (last-name ((lex {^ ^ ^ sss names last-name})))))))
           (created
            ((sss {^ ^ ^ sss-root args created}) (sss-root {^ sss})
             (cat measure)
             (quantity ((value {^ ^ sss restrictors value})))
             (unit ((lex "point")))))))
         (proc
          ((type material) (effect-type creative) (lex "provide")))
         (sss {^ ^ ^ sss root root elts car})))
   (cdr ((car ((stat-num 1)
               (partic
                ((created
                  ((unit ((gap yes) (lex "point")))
                   (sss {^ ^ ^ sss-root args created})
                   (sss-root {^ sss}) (cat measure)
                   (quantity ((value {^ ^ sss restrictors value})))))
                 (agent
                  ((sss {^ ^ ^ sss-root args agent})
                   (sss-root {^ sss}) (cat compound-proper)
                   (head
                    ((cat person-name)
                     (first-name
                      ((lex {^ ^ ^ sss names first-name})))
                     (last-name
                      ((lex {^ ^ ^ sss names last-name})))))))))
               (sss-root {^ sss}) (cat clause) (tense past)
               (fills none)
               (surfmap-cset
                ((+ {partic agent} {partic created}) (-)))
               (proc
                ((type material) (effect-type creative) (lex "add")))
               (sss {^ ^ ^ ^ sss-root elts cdr car})))))))
 (circum
  ((location
    ((position header) (sss-root {^ sss}) (cat address)
     (city ((lex {^ ^ sss restrictors city})))
     (sss {^ ^ ^ sss rels location})))
   (time ((sss {^ ^ ^ sss root rels time}) (fills time-rel)
          (sss-root {^ sss}) (cat date)
          (day-name ((lex {^ ^ sss restrictors day-name})))))
   (co-event
    ((sss {^ ^ ^ sss rels co-occur}) (position end)
     (mood present-participle) (sss-root {^ sss}) (cat clause)
     (tense past) (fills none) (controlled {^ partic agent})
     (proc ((type composite) (relation-type locative) (lex "power")))
     (partic
      ((located
        ((sss {^ ^ ^ sss-root args located}) (sss-root {^ sss})
         (cat compound-proper)
         (head
          ((cat team-name) (home ((lex {^ ^ ^ sss-root names home})))
           (franchise ((lex {^ ^ ^ sss-root names franchise})))))
         (number plural)))
       (affected {^ located})
       (location
        ((cat pp)
         (np
          ((sss {^ ^ ^ ^ sss-root args location})
           (sss-root {^ sss root}) (cat common) (definite no)
           (classifier
            ((sss {^ ^ sss rels score}) (synt-funct classifier)
             (sss-root {^ sss}) (cat score)
             (win ((value {^ ^ sss restrictors win})))
             (lose ((value {^ ^ sss restrictors lose})))))
           (surfmap-cset
            ((+ {circum co-event partic location np classifier}
                {circum co-event partic location np qualifier np})
             (-)))
           (qualifier
            ((cat pp) (prep ((lex "against")))
             (np
              ((sss {^ ^ ^ sss rels opposition}) (sss-root {^ sss})
               (cat compound-proper)
               (head
                ((cat team-name)
                 (home ((lex {^ ^ ^ sss-root names home})))
                 (franchise
                  ((lex {^ ^ ^ sss-root names franchise})))))
               (number plural)))))
           (head ((lex "win")))))
         (prep ((lex "to")))))))
     (surfmap-cset
      ((+ {circum co-event partic located}
          {circum co-event partic location np})
       (-)))))))))



