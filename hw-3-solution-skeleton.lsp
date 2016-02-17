; CS 161 Winter 2016: HW3

; [!] Include OUR HW2 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-2-solution using *our* path, which will likely
;     differ from yours (Note: HW2 also includes HW1)
(load "../hw2/hw-2-solution")


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; TODO



; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------


; FUNCTION: SUBST-FR
; PURPOSE:  Substitutes the bindings in the given BDS into the corresponding
;           variables in the given frame
; INPUT:    FRM: a frame with variables
;           BDS: a binding list
; OUTPUT:   FRM with replacements made
(defun SUBST-FR (frm bds)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


; FUNCTION: MP-INFER
; PURPOSE:  Attempts to unify the given rule's premises on the given facts,
;           and if successful, returns the conclusion of the rule with SUBST-FR
;           called on it using the successful binding list
; INPUT:    RULE: an if-then rule
;           O-FRAMES: a list of facts / concepts
; OUTPUT:   conclusion if successfully unified; nil otherwise
(defun MP-INFER (rule o-frames)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


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

; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; Problem 4 Rule Definitions
; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------