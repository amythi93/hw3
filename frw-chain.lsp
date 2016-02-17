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
    'UNIMPLEMENTED
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




