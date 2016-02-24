; FUNCTION: UNIFY-FR
; PURPOSE:  Unifies the given variables, frames, or lists of frames by the
;           criteria listed in the spec
; INPUT:    LFR1: a variable, frame, or list of frames
;           LFR2: a variable, frame, or list of frames
;           BDS: [Optional; default: '(T)] A binding list being built during
;                execution
; OUTPUT:   Binding list

(load "../hw-1-solution")

(defun IS-VAR (vari)
;check if vari is a variable as defined in the hw
	(cond
		((null vari) 
			nil
		)
		( (and (listp vari) (equal 2 (length vari)) )
			(if (and (equal (first vari) 'V) (atom (second vari)) )
				t
				nil
			)
		)
		(t nil)
	)
)

(defun IS-LIST-FR (vari)
;check if vari is a list of frames
	(cond
		((null vari)
			nil
		)
		((listp vari)
			(loop for v in vari
				do
					(if (not (listp v))
						;v can be nil
						(return-from IS-LIST-FR nil) 	
					)

				

			)
			t
		)
		(t
			nil
		)
	) 

)

(defun IS-FRAME (vari)
	(if (and (listp vari) (not (IS-LIST-FR vari)))
		t
		nil
	)

)

(defun EXIST-IN-BD (vari bd)
;check if vari already exists in bd, if so return the value it is bound to
	(loop for e in bd
		do
			(if (listp e)
				(if (equal vari (first e))
					(return-from EXIST-IN-BD (second e))
				)
			)
		
	)
	nil
)


(defun UNIFY-VAR (vari x bds)
	(if (equal vari x)
		(return-from UNIFY-VAR bds) 
		;no need to bind
	)
	(if (and (not (IS-VAR (first vari))) (IS-VAR x))
		(if (not (and (IS-VAR x) (IS-VAR vari) )) 
			(return-from UNIFY-VAR (UNIFY-VAR x vari bds))
		)
		
	)
	(if (and (not (IS-VAR x) ) (not (IS-VAR vari)) )
	;both are non-varibale, binding fails
				(return-from UNIFY-VAR nil)
	)
	(let ((corres-var-1 (EXIST-IN-BD vari bds)) (corres-var-2 (EXIST-IN-BD x bds)) )
		(cond
			((not (null corres-var-1))
				(UNIFY-VAR corres-var-1 x bds)
			)
			((not (null corres-var-2))
				(UNIFY-VAR vari corres-var-2 bds)
			)
			(t
				(append bds (list (list vari x)))  
			)
		)
	)

)

(defun UNIFY-FRAMES (f1 f2 bd is-complete)
	(if is-complete
		(if (equal (first f1) (first f2))
			(UNIFY-FRAMES (cdr f1) (cdr f2) bd nil)
			nil
		)
		(if (< (length f1) 2)
			bd
			(let ((tail (member (first f1) f2))  )
				
				(if (null tail)
					nil
					
					(let ((result (UNIFY-FR (second f1) (second tail) bd) )  )
						;check if the slot fillers that have the same predicates match
						(if (null result)
							nil
							
							
							(let ((recurse-result (UNIFY-FRAMES (nthcdr 2 f1) f2 result nil)) )
							
								recurse-result
							)
						)
						
						
					)
				)
			)
		)
	)
	
)

(defun REMOVE-FRAME (fr list)
    ;remove a frame from a list of frame
	(if (null list)
		(return-from REMOVE-FRAME list)
	)
	(if (null fr)
		nil
		(let ((cur (first list)) )
			(if (EQUAL-SF fr cur)
				(cdr list)
				(cons cur (REMOVE-FRAME fr (cdr list)))
			)
		)
	)
)

(defun UNIFY-FR-LIST (l1 l2 bd)

	(if (null l1)
		
		(return-from UNIFY-FR-LIST bd)
	)
	(if (or (null l2) (> (length l1) (length l2)))

		(return-from UNIFY-FR-LIST nil)
		
	)
	
		(let ((f1 (first l1)) )
			(loop for f2 in l2
				do
					(let ((new-bd (UNIFY-FR f1 f2 bd)) )
						
						(if (not (null new-bd)) 
							(let ((recurse-result (UNIFY-FR (cdr l1) (REMOVE-FRAME f2 l2) new-bd)) )
								;check if this binding is consistent with the biinding of the rest of frames
								(cond
									((equal (length l1) 1)
										
										(return-from UNIFY-FR-LIST new-bd)
									)
									(t
										(if (not (null recurse-result))
										
											(let ((u (union new-bd recurse-result)) )
												
												(return-from UNIFY-FR-LIST u)
											)
											
											
											
										)

									)

								)

								

							)
						)
					)

			)
		)

	

)


(defun UNIFY-FR (lfr1 lfr2 &optional (bds '(T)))
    (if (null bds)
    	(return-from UNIFY-FR nil)
    )
    (cond
    	((equal lfr1 lfr2)
    		bds
    	)
    	((IS-VAR lfr1)
    		(if (IS-LIST-FR lfr2)
    			nil
    			(UNIFY-VAR lfr1 lfr2 bds) 
    		)
    	)
    	((IS-VAR lfr2)
    		(if (IS-LIST-FR lfr1)
    			nil
    			(UNIFY-VAR lfr1 lfr2 bds) 
    		)
    	)
    	((and (IS-FRAME lfr1) (IS-FRAME lfr2))
    		(UNIFY-FRAMES lfr1 lfr2 bds t)
    	)
    	((and (IS-LIST-FR lfr1) (IS-LIST-FR lfr2))
    		(UNIFY-FR-LIST lfr1 lfr2 bds)
    		
    	)
    	(t
    		
    		nil
    	)
    )
)




;=======================================================
;	TESTS
;=======================================================

(print (UNIFY-FR '(V X)
 '(HUMAN F-NAME (ANDREW))
))

; SHOULD RETURN:
;	(T (( V X) (HUMAN F-NAME (ANDREW))))

;-------------------------------------------------------

;(print (UNIFY-FR '(HUMAN F-NAME (V X)) '(HUMAN L-NAME (DYER) F-NAME (MICHAEL))) )

; SHOULD RETURN:
;	(T ((V X) (MICHAEL)))

;-------------------------------------------------------
;(print
 ;(UNIFY-FR '(
;			 (HUMAN F-NAME (V X))
;			 (DRUG NAME (V Y)) 
 ;          )
;		   '(
;		     (DRUG NAME (COCAINE)) 
;		     (HUMAN L-NAME LN2) 
 ;           (HUMAN L-NAME LN1
;				    F-NAME (GEORGE))
;			))
 ;)

; SHOULD RETURN:
;	(T ((V X) (GEORGE)) ((V Y) (COCAINE)))

;-------------------------------------------------------

; (setq FR1 '(ENABLES ANTE (HAVE AGENT (V XX01) OBJECT (MONEY)) CONSEQ (TRAVEL AGENT (V XX01) TO (V QQ01))))

 ;(setq FR2 '(ENABLES CONSEQ (TRAVEL TO (FRANCE) AGENT (V YY01)) ANTE (HAVE AGENT (JOE) OBJECT (V ZZ01))))

 ;(print (UNIFY-FR FR1 FR2) )

; SHOULD RETURN:
; (T ((V XX01) (JOE))
;   ((V ZZ01) (MONEY))
;   ((V YY01) (JOE))
;   ((V QQ01) (FRANCE))
; )


;-------------------------------------------------------

;(setq FR3 '(ENABLES ANTE (HAVE AGENT (V XX01) OBJECT (MONEY)) CONSEQ (TRAVEL AGENT (V XX01) TO (FRANCE))))

 ;(setq FR4 '(ENABLES ANTE (HAVE AGENT (JOE) OBJECT (V ZZ01)) CONSEQ (TRAVEL AGENT (V YY01) TO (V YY01))))

 ;(print (UNIFY-FR FR3 FR4) )

; SHOULD RETURN:
; NIL

;-------------------------------------------------------

 ;(setq LST1 '((INFORM SITU (S1) RECIP (V HUMX01) AGENT (HUMAN ROLE (ONCOLOGIST)) OBJECT (STATE AGENT (V HUMX01) OBJECT (V OBJX01))) (AFTER ANTE (S1) CONSEQ (V SXB1))))

 ;(setq LST2 '((INFORM AGENT (V HUMX02) RECIP (HUMAN F-NAME (GEORGE) GENDER (MALE)) OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE) GENDER (MALE)) OBJECT (CANCER TYPE (TERMINAL))) SITU (V SXA1)) (AFTER CONSEQ (S2) ANTE (V SXA1))))

 ;(print (UNIFY-FR LST1 LST2) )

; SHOULD RETURN:
; (T  	((V SXA1) (S1))
;		((V HUMX01) (HUMAN F-NAME (GEORGE) GENDER (MALE)))
;		((V HUMX02) (HUMAN ROLE (ONCOLOGIST))) ((V OBJX01) (CANCER TYPE (TERMINAL))) 
;		((V SXB1) (S2))
; )

;-------------------------------------------------------

 ;(setq FRM-A '(INFORM RECIP (HUMAN F-NAME (GEORGE)) OBJECT (STATE OBJECT (CANCER TYPE (TERMINAL))) AGENT (V HUMX08)))

 ;(setq FRM-B '(INFORM SITU (S1) RECIP (V HUMX09) AGENT (HUMAN ROLE (ONCOLOGIST)) OBJECT (STATE OBJECT (V OBJX07) AGENT (HUMAN F-NAME (GEORGE)))))

 ;(print (UNIFY-FR FRM-A FRM-B) ) 

; SHOULD RETURN:
; (T 	((V HUMX09) (HUMAN F-NAME (GEORGE)))
;		((V OBJX07) (CANCER TYPE (TERMINAL)))
;		((V HUMX08) (HUMAN ROLE (ONCOLOGIST))) 
; )

;(print (UNIFY-FR FRM-B FRM-A) ) 
; (UNIFY-FR FRM-B FRM-A)

; SHOULD RETURN:
; NIL




