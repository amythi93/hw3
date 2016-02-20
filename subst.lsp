; FUNCTION: SUBST-FR
; PURPOSE:  Substitutes the bindings in the given BDS into the corresponding
;           variables in the given frame
; INPUT:    FRM: a frame with variables
;           BDS: a binding list
; OUTPUT:   FRM with replacements made


(defun SUBST-HELPER (frm bd)
	(let ((returnFrame nil))
		(loop for x in frm do
			(if (equal (car bd) x)
				(setq returnFrame (append returnFrame (cdr bd)))
				(if (listp x)
					(SUBST-HELPER x bd)
				)
			)
		)
		returnFrame
	)
)

(defun SUBST-FR (frm bds)
    (if (equal bds nil) 
    	frm
    	(progn
    		(let ((returnFrame '()))
	    		(loop for x in (rest bds) do
	    			(print x)
	    			(if (equal (SUBST-HELPER frm x) nil)
	    				(setq returnFrame (append returnFrame x))
	    				(setq returnFrame (append returnFrame (SUBST-HELPER frm x)))
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

(setq BD1 '(T ((V HX1) (HUMAN F-NAME (GEORGE) GENDER (MALE))) ((V SS01) (S2)) ((V TY01) (TERMINAL))))


(setq FR6 '(KNOWS AGENT (V HX1) 
				OBJECT (STATE TYPE (PHYSICAL) AGENT (V HX1) OBJECT (CANCER TYPE (V TY01))) 
			SITU (V SS01)))

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




