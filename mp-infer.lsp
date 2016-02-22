; FUNCTION: MP-INFER
; PURPOSE:  Attempts to unify the given rule's premises on the given facts,
;           and if successful, returns the conclusion of the rule with SUBST-FR
;           called on it using the successful binding list
; INPUT:    RULE: an if-then rule
;           O-FRAMES: a list of facts / concepts
; OUTPUT:   conclusion if successfully unified; nil otherwise
(defun MP-INFER (rule o-frames)
   					;an extra cdr here because of the prem 
   (let* ((prem (cdr (car rule))) (conc (cdr (cdr rule)) )    )  ;setting the premises and conclusion here
   
    (if (null prem  )  ;if prem is nil here, we return nil
 				   nil)
 	(if (null conc)	nil) ;if conclusion is nil here we return nil
 	
 				   
 				   ;cdr here because we don't want no T in our list
 				   
 	(let* ((oriList  (UNIFY-FR  prem o-frames  )))
 
 		(return-from MP-INFER (SUBST-FR oriList conc) ) ;return the result here
 	
 	
 	)

 				   
 	
	
	
  ;general idea: loop through each list of frames and unify it with the rule and then we call 
  ;subst-fr and boom
  
  
  
  
  
  )
)




;=======================================================
;	TESTS
;=======================================================

; INFORMAL, ENGLISH RULE:
; IF (x INFORMs y of z in SITUation sa) AND
;	(SITUation sb occurs right AFTER SITUation sa) 
; THEN (y KNOWS z in SITUation sb)

; FORMAL, FRAME TRANSLATION:
;(setq RULE-2 '((PREMISES (INFORM AGENT (V x) RECIP (V y) OBJECT (V z) SITU (V sa)) (AFTER ANTE (V sa) CONSEQ (V sb))) (CONCLU (KNOWS AGENT (V y) OBJECT (V z) SITU (V sb)))))

; (setq EP2 '(INFORM AGENT (HUMAN ROLE (ONCOLOGIST)) RECIP (HUMAN F-NAME (GEORGE) GENDER (MALE)) OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE) GENDER (MALE)) OBJECT (CANCER TYPE (TERMINAL))) SITU (S1))))

; (setq EP5 '(AFTER ANTE (S1) CONSEQ (S2)))

; (print (MP-INFER RULE-2 (LIST EP-2 EP-5)) )

; SHOULD RETURN:
; INF2 = (KNOWS AGENT (HUMAN F-NAME (GEORGE)
;							 GENDER (MALE))
;				OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE)
;											GENDER (MALE)) 
;							  OBJECT (CANCER TYPE (TERMINAL)))
;				SITU (S2))





(setq RULE-2 '((PREMISES
(INFORM AGENT (V x)
RECIP (V y) OBJECT (V z) SITU (V sa))
(AFTER ANTE (V sa) CONSEQ (V sb))
) (CONCLU
(KNOWS AGENT (V y) OBJECT (V z) SITU (V sb))
)))


(setq test '(T ((V XX01) (JOE))
   ((V ZZ01) (MONEY))
   ((V YY01) (JOE))
   ((V QQ01) (FRANCE))
))


(print (nth 0 RULE-2))


