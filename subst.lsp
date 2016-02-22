; FUNCTION: SUBST-FR
; PURPOSE:  Substitutes the bindings in the given BDS into the corresponding
;           variables in the given frame
; INPUT:    FRM: a frame with variables
;           BDS: a binding list
; OUTPUT:   FRM with replacements made

(defun RECURSIVELY-CHECK-FRAME (frm bds)
	'UNIMPLEMENTED
)

(defun SUBST-FR (frm bds)
    (if (equal bds nil) 
    	frm
    	(progn
    		(let ((returnFrame '()) (truth nil))
	    		(loop for x in frm do
	    			(if (and (listp x) (equal (length x) 2))
	    				(progn
	    					(loop for y in (rest bds) do
	    						(if (equal (car y) x)
	    							(progn
	    								(setq returnFrame (append returnFrame (cdr y)))
	    								(setq truth t)
	    							)
	    						)
	    					)
	    				)
	    				(if (listp x)
	    					(RECURSIVELY-CHECK-FRAME x (rest bds))
	    					(setq returnFrame (append returnFrame (list x)))
	    				)
	    			)
    			)
    		returnFrame
    		)
		)
    )
)





;=======================================================
;	TESTS
;=======================================================

(setq BD1 '(T 
	((V HX1) (HUMAN F-NAME (GEORGE) GENDER (MALE))) 
	((V SS01) (S2)) 
	((V TY01) (TERMINAL)))
)

(setq FR6 '(KNOWS AGENT (V HX1) 
				  OBJECT (STATE TYPE (PHYSICAL) 
				  				AGENT (V HX1) 
				  				OBJECT (CANCER TYPE (V TY01))) 
  				  SITU (V SS01))
)

(print 
	(SUBST-FR FR6 BD1)
)

; SHOULD RETURN:
;	(KNOWS AGENT (HUMAN F-NAME (GEORGE)
;						GENDER (MALE)) 
;		   OBJECT (STATE TYPE (PHYSICAL)
;						 AGENT (HUMAN F-NAME (GEORGE) 
;									  GENDER (MALE))
;						 OBJECT (CANCER TYPE (TERMINAL)))
;		   SITU (S2))



