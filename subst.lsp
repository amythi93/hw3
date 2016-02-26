; FUNCTION: SUBST-FR
; PURPOSE:  Substitutes the bindings in the given BDS into the corresponding
;           variables in the given frame
; INPUT:    FRM: a frame with variables
;           BDS: a binding list
; OUTPUT:   FRM with replacements made

(defun CHECKS-AGAINST-BDS (lst bds)
	(loop for y in bds do
		(if (equal (car y) lst)
			(return (rest y))
		)
	)
)


(defun RECURSIVELY-CHECKS-FRAME (frm bds returnFrame)
	(cond
		((null frm) 
			returnFrame
		)
		( (and (atom (car frm)) (listp (car frm)))
			(setq returnFrame (append returnFrame (list (car frm))))
			(RECURSIVELY-CHECKS-FRAME (cdr frm) bds returnFrame)
			;(print "list of 1")
			;(print return frame)
		)
		((atom (car frm))
			(setq returnFrame (append returnFrame (list(car frm))))
			(RECURSIVELY-CHECKS-FRAME (cdr frm) bds returnFrame)
			;(print "atom")
			;(print returnFrame)
		)
		( (and (listp (car frm)) (equal (length (car frm)) 1))
			(setq returnFrame (append returnFrame (list (car frm))))
			(RECURSIVELY-CHECKS-FRAME (cdr frm) bds returnFrame)
			;(print "list of 1")
			;(print returnFrame)
		)
		( (and (listp (car frm)) (equal (length (car frm)) 2) (equal (CHECKS-AGAINST-BDS (car frm) bds) nil))
			(setq returnFrame (append returnFrame (list (car frm))))
			(RECURSIVELY-CHECKS-FRAME (cdr frm) bds returnFrame)
			;(print "list of 2 not bds")
			;(print returnFrame)
		)
		( (and (listp (car frm)) (equal (length (car frm)) 2))
			(setq returnFrame (append returnFrame (CHECKS-AGAINST-BDS (car frm) bds)))
			(RECURSIVELY-CHECKS-FRAME (rest frm) bds returnFrame)
			;(print "match BDS")
			;(print returnFrame)
		)
		( (and (listp (car frm)) (> (length (car frm)) 2))
			 (setq returnFrame (append returnFrame (list (RECURSIVELY-CHECKS-FRAME (car frm) bds nil)))))
		(T (RECURSIVELY-CHECKS-FRAME (cdr frm) bds returnFrame) ;(print "else stmt")(print returnFrame)
		)
	)
)


(defun SUBST-FR (frm bds)
    (if (equal bds nil) 
    	frm
    	(progn
    		(let ((returnFrame '()))
	    		(loop for x in frm do
	    			(if (and (listp x) (equal (length x) 2))
	    				(progn
	    					(loop for y in (rest bds) do
	    						(if (equal (car y) x)
	    							(progn
	    								(setq returnFrame (append returnFrame (cdr y)))
	    							)
	    						)
	    					)
	    				)
	    				(if (listp x)
	    					(progn
	    						(let ((rframe nil))
									(setq returnFrame (append returnFrame (list (RECURSIVELY-CHECKS-FRAME x (rest bds) rframe))))
	    						)
	    					)
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



