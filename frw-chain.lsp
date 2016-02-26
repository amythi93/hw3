; FUNCTION: FRW-CHAIN
; PURPOSE:  Performs simplified forward chaining given the list of rules and
;           facts, returning any new conclusions that are derived
; INPUT:    RULES: a list of if-then rules
;           EPMEM: a list of facts storing an episodic memory
;           NEW-EPMEM: [Optional: default: nil] the list of newly discovered
;                      facts grown through code execution and returned at the
;                      end
; OUTPUT:   NEW-EPMEM
(defun FRW-CHAIN (rules epmem &optional (new-epmem nil))
	
	
	(let* ((newC nil )))
	
		(loop for each mRule in rules do 
		
			(setq newC (MP-INFER mRule epmem) )
	
			(if (null newC) 
				(return-from FRW-CHAIN new-epmem)
				
				
				(FRW-CHAIN (rules epmem (cons newC new-epmem)))
			)
		
		
		
		)
	
	
		
		
		
		
	
	)
	
	

)




;=======================================================
; TESTS
;=======================================================

; (setq RULE-51 '((PREMISES (OWNS AGENT (V a1) OBJECT (V o1)) (ISA OBJECT (V o1) TYPE (ICE-CREAM))) (CONCLU (HAPPY AGENT (V a1)) )))

; (setq RULE-52 '((PREMISES (HAPPY AGENT (V a1))) (CONCLU (AWESOME AGENT (V a1)) )))

; (print (FRW-CHAIN (list RULE-51 RULE-52) '((OWNS AGENT (ANDREW) OBJECT (DRUMSTICK)) (ISA OBJECT (DRUMSTICK) TYPE (ICE-CREAM)))) )

; SHOULD RETURN:
; (
;   (HAPPY AGENT (ANDREW))
;   (AWESOME AGENT (ANDREW))
; )




;function FOL-FC-ASK(KB,α) returns a substitution or false 
;inputs: KB , the knowledge base, a set of first-order definite clauses
;		α, the query, an atomic sentence
;local variables: new, the new sentences inferred on each iteration
;repeat until new is empty 
;	new ← { }
;	for each rule in KB do
;		(p1 ∧ . . . ∧ pn ⇒ q) ← STANDARDIZE-VARIABLES(rule)
;		for each θ such that SUBST(θ,p1 ∧ ... ∧ pn) = SUBST(θ,p1′ ∧ ... ∧ pn′ )for some p1′ ,...,pn′ in KB 
;			q′ ←SUBST(θ,q)
;			if q′ does not unify with some sentence already in KB or new then 
;				add q′ to new
;				φ ← UNIFY(q′, α)
;				if φ is not fail then return φ 
;		add new to KB
;return false