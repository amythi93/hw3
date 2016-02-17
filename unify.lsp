; FUNCTION: UNIFY-FR
; PURPOSE:  Unifies the given variables, frames, or lists of frames by the
;           criteria listed in the spec
; INPUT:    LFR1: a variable, frame, or list of frames
;           LFR2: a variable, frame, or list of frames
;           BDS: [Optional; default: '(T)] A binding list being built during
;                execution
; OUTPUT:   Binding list
(defun UNIFY-FR (lfr1 lfr2 &optional (bds '(T)))
    'UNIMPLEMENTED
)




;=======================================================
;	TESTS
;=======================================================

; (print (UNIFY-FR '(V X) '(HUMAN F-NAME (ANDREW))) )

; SHOULD RETURN:
;	(T (( V X) (HUMAN F-NAME (ANDREW))))

;-------------------------------------------------------

; (print (UNIFY-FR '(HUMAN F-NAME (V X)) '(HUMAN L-NAME (DYER) F-NAME (MICHAEL))) )

; SHOULD RETURN:
;	(T ((V X) (MICHAEL)))

;-------------------------------------------------------
; (print
; (UNIFY-FR '(
;			 (HUMAN F-NAME (V X))
;			 (DRUG NAME (V Y)) 
;           )
;		   '(
;		     (DRUG NAME (COCAINE)) 
;		     (HUMAN L-NAME LN2) 
;            (HUMAN L-NAME LN1
;				    F-NAME (GEORGE))
;			))
; )

; SHOULD RETURN:
;	(T ((V X) (GEORGE)) ((V Y) (COCAINE)))

;-------------------------------------------------------

; (setq FR1 '(ENABLES ANTE (HAVE AGENT (V XX01) OBJECT (MONEY)) CONSEQ (TRAVEL AGENT (V XX01) TO (V QQ01))))

; (setq FR2 '(ENABLES CONSEQ (TRAVEL TO (FRANCE) AGENT (V YY01)) ANTE (HAVE AGENT (JOE) OBJECT (V ZZ01))))

; (print (UNIFY-FR FR1 FR2) )

; SHOULD RETURN:
; (T ((V XX01) (JOE))
;   ((V ZZ01) (MONEY))
;   ((V YY01) (JOE))
;   ((V QQ01) (FRANCE))
; )


;-------------------------------------------------------

; (setq FR3 '(ENABLES ANTE (HAVE AGENT (V XX01) OBJECT (MONEY)) CONSEQ (TRAVEL AGENT (V XX01) TO (FRANCE))))

; (setq FR4 '(ENABLES ANTE (HAVE AGENT (JOE) OBJECT (V ZZ01)) CONSEQ (TRAVEL AGENT (V YY01) TO (V YY01))))

; (print (UNIFY-FR FR3 FR4) )

; SHOULD RETURN:
; NIL

;-------------------------------------------------------

; (setq LST1 '((INFORM SITU (S1) RECIP (V HUMX01) AGENT (HUMAN ROLE (ONCOLOGIST)) OBJECT (STATE AGENT (V HUMX01) OBJECT (V OBJX01))) (AFTER ANTE (S1) CONSEQ (V SXB1))))

; (setq LST2 '((INFORM AGENT (V HUMX02) RECIP (HUMAN F-NAME (GEORGE) GENDER (MALE)) OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE) GENDER (MALE)) OBJECT (CANCER TYPE (TERMINAL))) SITU (V SXA1)) (AFTER CONSEQ (S2) ANTE (V SXA1))))

; (print (UNIFY-FR LST1 LST2) )

; SHOULD RETURN:
; (T  	((V SXA1) (S1))
;		((V HUMX01) (HUMAN F-NAME (GEORGE) GENDER (MALE)))
;		((V HUMX02) (HUMAN ROLE (ONCOLOGIST))) ((V OBJX01) (CANCER TYPE (TERMINAL))) 
;		((V SXB1) (S2))
; )

;-------------------------------------------------------

; (setq FRM-A '(INFORM RECIP (HUMAN F-NAME (GEORGE)) OBJECT (STATE OBJECT (CANCER TYPE (TERMINAL))) AGENT (V HUMX08)))

; (setq FRM-B '(INFORM SITU (S1) RECIP (V HUMX09) AGENT (HUMAN ROLE (ONCOLOGIST)) OBJECT (STATE OBJECT (V OBJX07) AGENT (HUMAN F-NAME (GEORGE)))))

; (print (UNIFY-FR FRM-A FRM-B) ) 

; SHOULD RETURN:
; (T 	((V HUMX09) (HUMAN F-NAME (GEORGE)))
;		((V OBJX07) (CANCER TYPE (TERMINAL)))
;		((V HUMX08) (HUMAN ROLE (ONCOLOGIST))) 
; )


; (UNIFY-FR FRM-B FRM-A)

; SHOULD RETURN:
; NIL




