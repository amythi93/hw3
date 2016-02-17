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


(setq RULE-1 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))
      
      ; RULE-2 was done for you! Yay!
      RULE-2 '((PREMISES
                 (INFORM AGENT (V x)
                         RECIP (V y)
                         OBJECT (V z)
                         SITU (V sa))
                 
                 (AFTER ANTE (V sa)
                        CONSEQ (V sb))
               )
               (CONCLU
                 (KNOWS AGENT (V y)
                        OBJECT (V z)
                        SITU (V sb))
               ))
      
      RULE-3 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))
      
      RULE-4 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))
      
      RULE-5 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))
      
      RULE-6 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))
      
      RULE-7 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))
)
