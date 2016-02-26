; CS 161 Winter 2016: HW3

; [!] Include OUR HW2 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-2-solution using *our* path, which will likely
;     differ from yours (Note: HW2 also includes HW1)
;(load "../hw2/hw-2-solution")
; CS 161 Winter 2016: HW2 Solution

; [!] Include OUR HW1 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-1-solution using *our* path, which will likely
;     differ from yours
;(load "../hw1/hw-1-solution")
; CS 161 Winter 2016: HW1
; Solution


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; FUNCTION: front-slot
; PURPOSE:  Return the name of the first SLOT in FRAME
; INPUT:    frame
; OUTPUT:   atom: name of first SLOT
(defun front-slot (frame)
    (second frame)
)

; FUNCTION: front-filler
; PURPOSE:  Return the FILLER of the first SLOT in FRAME
; INPUT:    frame
; OUTPUT:   frame/gap: filler of first SLOT in FRAME
(defun front-filler (frame)
    (third frame)
)

; FUNCTION: pop-slot
; PURPOSE:  Return a copy of FRAME with its first slot-filler removed
; INPUT:    frame
; OUTPUT:   frame (with first slot-filler removed)
(defun pop-slot (frame)
    (cons (first frame) (nthcdr 3 frame))
)

; FUNCTION: f-pred
; PURPOSE:  retrieves the front-predicate of a frame, or the symbol name if
;           it's a gap
; INPUTS:   frame: a gap or a frame
; OUTPUT:   the predicate if it's a frame, or the symbol name if it's a gap
(defun f-pred (frame)
    (cond
        ((listp frame) (first frame))
        (t frame)
    )
)

; FUNCTION: f-length
; PURPOSE:  Safely checks the length of the input if it's a frame vs gap
; INPUTS:   frame: a gap or a frame
; OUTPUT:   length of frame if it's a frame; 1 if it's a gap
(defun f-length (frame)
    (cond
        ((listp frame) (length frame))
        (t 1)
    )
)

; FUNCTION: rm-slot
; PURPOSE:  Return a copy of FRAME, but with a single slot-filler removed
; INPUT:    frame: frame to remove slot from
;           slot: slot name to be removed
; OUTPUT:   frame
(defun rm-slot (slot frame)
    (cond
        ; Base case: no slots left, so we're done
        ((<= (length frame) 1) frame)
        ; Base case: front slot matches, so just pop it
        ((equal (front-slot frame) slot) (pop-slot frame))
        ; Recursive case: front slot doesn't match, so keep looking
        (t (append (rm-slot slot (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)


; -----------------------------------------------------------------------------
; Main Functions
; -----------------------------------------------------------------------------

; FUNCTION: GET-SF
; PURPOSE:  Returns the filler of the given slot-name
; INPUTS:   slot: an atom designating the slot name
;           frame: a frame
; OUTPUT:   A FILLER (FRAME or GAP), according to slot in frame, or NIL if not
;           present
(defun GET-SF (slot frame)
    (cond
        ; Base case: predicate with no slots (or empty frame)
        ((<= (length frame) 1) nil)
        ; If first slot matches, return its filler.
        ((equal slot (front-slot frame)) (front-filler frame))
        ; Else, first slot does not match, so test the rest of the slots
        (t (GET-SF slot (pop-slot frame)))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: GET-NESTED-SF
; PURPOSE:  Returns the filler at the end of the given slot-path, or nil if
;           the path does not exist in the given frame
; INPUTS:   slots: a path of slots
;           concept: a frame
; OUTPUT:   The requested filler, which will be a frame or gap
(defun GET-NESTED-SF (slots concept)
    (cond
        ; Base case: got to the last slot, so stop recursing
        ((null slots) concept)
        ; Base case: null concept
        ((null concept) nil)
        ; If our concept is a gap to expand, we need to recurse on the frame
        ; Return nil if it's a gap but not bound
        ((atom concept) (if (boundp concept) (GET-NESTED-SF slots (eval concept)) nil))
        ; Recursive case: continue following path on sub-frame matched by filler
        ; of current path element
        (t (GET-NESTED-SF (rest slots) (GET-SF (first slots) concept)))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: EXPAND-SLOTS
; PURPOSE:  Looks for gaps in a list of slot-filler pairs, dispatching EXPAND
;           on each filler.
; INPUTS:   sf: list of (slot filler) pairs
; OUTPUT:   List of slot-filler pairs with any gaps replaced by their values
;           (follows any number of successive references)
(defun EXPAND-SLOTS (sf)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch EXPAND on our first filler
        (t (append (append (list (first sf))               ; rebuild our first slot-filler pair
                           (list (EXPAND (second sf))))    ; dispatch EXPAND on the filler
                           (EXPAND-SLOTS (nthcdr 2 sf))))  ; recurse on rest of sf
    )
)

; FUNCTION: EXPAND
; PURPOSE:  Returns the frame represented by the given atom (if it is bound)
;           with all gaps replaced by their values
; INPUTS:   atom: a symbol (possibly) representing a frame
; OUTPUT:   That frame with all bound gaps replaced by their values
(defun EXPAND (atom)
    (cond
        ; Base case: safety for getting nil input - just return nil
        ((null atom) nil)
        ; Base case: we got a non-NIL atom, so evaluate it if bound
        ((atom atom) (if (boundp atom) (EXPAND (eval atom)) atom))
        ; Base case: empty, or single pred frame
        ((<= (length atom) 1) atom)
        ; Main case: dispatch EXPAND-SLOTS on our slot-filler list
        (t (cons (first atom) (EXPAND-SLOTS (rest atom))))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: AMEND-SF-EXEC
; PURPOSE:  (Workhorse Helper for AMEND-SF)
;           Return a FRAME which is a copy of the referenced frame, with the
;           designated SLOT filled in with the given FILLER (or added if the
;           SLOT does not exist) [Same inputs / outputs]
(defun AMEND-SF-EXEC (slot filler frame)
    (cond
        ; Base case: single predicate, so add the slot
        ((<= (length frame) 1) (cons (first frame) (cons slot (list filler))))
        ; Base case: If first slot is target, replace the filler, keep rest slots
        ((equal slot (front-slot frame))
         (cons (first frame) (append (append (list slot) (list filler) (nthcdr 3 frame))))
        )
        ; Recursive case: First slot not target, so pop and recurse
        (t (append (AMEND-SF-EXEC slot filler (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)

; FUNCTION: AMEND-SF
; PURPOSE:  Returns a copy of the input concept with the given slot-filler
;           pair added. If the slot already exists in the frame, its filler
;           will be replaced by the given input filler
; INPUTS:   slot: an atom representing the slot name
;           filler: a filler to place in the corresponding slot
;           frame: the frame being operated on
; OUTPUT:   Frame with added / replaced slot-filler pair
(defun AMEND-SF (slot filler frame)
    ; Check to see if we've been given a bound gap (just in case, you know?)
    (if (atom frame) 
        (if (boundp frame) (AMEND-SF-EXEC slot filler (eval frame)) frame)
        (AMEND-SF-EXEC slot filler frame)
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: EQUAL-SF-COMP
; PURPOSE:  Helper function that performs actual workhorse of frame comparison
;           on EXPANDed frame inputs
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: [EXPANDED] FRAME (first frame to compare)
;           frame2: [EXPANDED] FRAME (second frame to compare)
(defun EQUAL-SF-COMP (frame1 frame2)
    (cond
        ; Base case: empty frames match
        ((and (null frame1) (null frame2)) t)
        ; Base case: frames with different preds do not match
        ((not (equal (f-pred frame1) (f-pred frame2))) NIL)
        ; Base case: frames of different lengths do not match
        ((not (= (f-length frame1) (f-length frame2))) NIL)
        ; Base case: bare predicates (or matching symbols) match
        ((<= (f-length frame1) 1) t)
        ; Base case: variables match iff they have the same name
        ; [!] Not required for HW1, will not be tested
        ((equal (first frame1) 'V) (equal frame1 frame2))
        
        ; Recursive case: check frame1's front slot, then remove and recurse to
        ; check the rest of the slots.
        (t (let ((front (front-slot frame1))) 
            (and (EQUAL-SF-COMP (GET-SF front frame1) (GET-SF front frame2))
                 (EQUAL-SF-COMP (pop-slot frame1) (rm-slot front frame2)))
            )
        )
    )
)

; FUNCTION: EQUAL-SF
; PURPOSE:  Boolean predicate which compares two frames
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: FRAME (first frame to compare)
;           frame2: FRAME (second frame to compare)
(defun EQUAL-SF (frame1 frame2)
    (let ((EX-frame1 (EXPAND frame1)) (EX-frame2 (EXPAND frame2)))
        (EQUAL-SF-COMP EX-frame1 EX-frame2)
    )
)

; -----------------------------------------------------------------------------


; Mark as included for examples file
; (this is not necessary for solution correctness, but avoids some warnings
; on the hw-1-examples.lsp)

(setq HW-1-SOLN-INCLUDED t)


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; FUNCTION: NEWATM
; PURPOSE:  Produces a fresh, unused, unbound, unique symbol with the given
;           symName prefix and (ostensibly) random numerical suffix
; INPUT:    symName: symbol prefix for the new symbol
; OUTPUT:   new unbound symbol with symName prefix and numeric suffix
(defun NEWATM (symName)
    ; Generate the new symbol using gensym
    (let* ((new-sym (gensym (string symName))))
        (intern (string new-sym))
    )
)



; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

; FUNCTION: ADD-TO-LM-EXEC
; PURPOSE:  Helper for ADD-TO-LM that simply checks for duplicate entries in LEX-MEM
;           before committing to a phrase addition
; INPUT:    phrase: a list of English words
;           frame: a frame associated with that phrase
;           demons: a list of 0 or more demon instantiations
;           lex-mem: current recursive state of input *LM
; OUTPUT:   current status of lexmem after execution
(defun ADD-TO-LM-EXEC (phrase frame demons lex-mem)
    (cond
        ; Base case: lex-mem is null so just insert
        ((null lex-mem) (list (list phrase frame demons)))
        ; Base case: we found a phrase match at the current front
        ; of lex-mem, so replace with our new definition
        ((equal (first (first lex-mem)) phrase) (cons (list phrase frame demons) (rest lex-mem)))
        ; Recursive case: haven't found our matching phrase in lex yet, so keep
        ; looking at the front and recursing
        (t (append (list (first lex-mem)) (ADD-TO-LM-EXEC phrase frame demons (rest lex-mem))))
    )
)

; FUNCTION: ADD-TO-LM
; PURPOSE:  Adds the given (phrase frame demon) triplet to the global LEX-MEM,
;           making sure to not add duplicate phrases
; INPUT:    phrase: a list of English words
;           frame: a frame associated with that phrase
;           demons: a list of 0 or more demon instantiations
; OUTPUT:   phrase-frame-demon triplet constructed
(defun ADD-TO-LM (phrase frame demons)
    ; Perform the insertion or modification of *LM
    (setq *LM (ADD-TO-LM-EXEC phrase frame demons *LM))
    ; Return the created triplet
    (list phrase frame demons)
)

; -----------------------------------------------------------------------------

; FUNCTION: MATCH-PH
; PURPOSE:  Helper for LOOKUP-PHRASE that returns length of lexical index 
;           (word/phrase) if a match with the start of word,
;           0 otherwise
; INPUT:    sent: a list of English words
;           phrase: a lexical entry phrase
; OUTPUT:   length of lexical index match
(defun MATCH-PH (sent phrase)
    (let* ((len (length phrase)))
        (cond
            ; Not enough words left, so no dice
            ((< (length sent) len) 0)
            ; First len entries of words indeed match, so return len
            ((equal (subseq sent 0 len) phrase) len)
            ; Otherwise, return 0
            (t 0)
        )
    )
)

; FUNCTION: LOOKUP-PHRASE
; PURPOSE:  Looks for the longest phrase at the start of sent that matches a
;           corresponding phrase in the input lex-mem, and if found, returns:
;           ((phrase frame demons) rest-sent)
;           ...for corresponding (phrase frame demons) triplet found in lex-mem
;           ...and rest-sent being the rest of the sentence minus the found phrase
;
;           If NOT found, returns:
;           ((phrase nil nil) rest-sent)
; INPUT:    sent: a list of English words
;           lex-mem: a lexical memory with ((phrase frame demon)*) triplets
; OUTPUT:   (see above in purpose)
(defun LOOKUP-PHRASE (sent lex-mem)
    (cond
        ; Base cases: return nil if either params null
        ((null sent) nil)
        ((null lex-mem) nil)
        
        ; Got to end of lex-mem, so return the best match, or special unknown
        ; word frame
        ((= (length lex-mem) 1)
            (let* (
                (lex (first lex-mem))
                (match-len (MATCH-PH sent (first lex)))
            )
            (if (> match-len 0)
                ; If match, build list of phrase, frame, demons as the first
                ; element, with the rest of the sentence tacked on after
                (list (list (subseq sent 0 match-len) (second lex) (third lex))
                      (nthcdr match-len sent))
                
                ; If no match, special return value
                (list (list
                    ; Unknown word goes as its own list in front, with NIL
                    ; frame and demons
                    (list (first sent)) NIL NIL)   
                    ; Remove first word for rest of sentence
                    (rest sent))
            ))
        )
        
        ; Recursive case: find best match between first two entries. Keep
        ; first entry if neither match.
        (t (let* (
                (match1 (MATCH-PH sent (first (first lex-mem))))
                (match2 (MATCH-PH sent (first (second lex-mem))))
            )
            ; Remove the worst match of the first 2 entries
            (if (>= match1 match2)
                (LOOKUP-PHRASE sent (cons (first lex-mem) (nthcdr 2 lex-mem)))
                (LOOKUP-PHRASE sent (rest lex-mem)))
            )
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: UNIQUE-GAPSLOTS (helper for NEWGAPS)
; PURPOSE:  Looks for gaps in a list of slot-filler pairs, dispatching
;           NEWGAPS on each filler.
; INPUTS:   sf: list of (slot filler) pairs
; OUTPUT:   List of slot-filler pairs with any gaps replaced by NEWATMs
(defun UNIQUE-GAPSLOTS (sf)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch TOKENIZE on our first filler
        (t (append (append (list (first sf))                  ; rebuild our first slot-filler pair
                           (list (NEWGAPS (second sf))))      ; dispatch NEWGAPS on the filler
                           (UNIQUE-GAPSLOTS (nthcdr 2 sf))))  ; recurse on rest of sf
    )
)

; FUNCTION: NEWGAPS
; PURPOSE:  Replaces all gaps in the input frames with unique, unbound gap names
;           and returns a copy of the resulting frame
; INPUT:    frame: a frame
; OUTPUT:   frame with now unique gap-names
(defun NEWGAPS (frame)
    (cond
        ; Base case: nil input -- just return nil
        ((null frame) nil)
        ; Base case: we got an atom, so uniquify it, and return it
        ((atom frame) (let* ((gap (NEWATM frame))) (set gap NIL) gap))
        ; Base case: empty, or single pred frame
        ((<= (length frame) 1) frame)
        ; Recursive case: dispatch UNIQUE-GAPSLOTS on our slot-filler list
        (t (cons (first frame) (UNIQUE-GAPSLOTS (rest frame))))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: IS-SUBCLASS
; PURPOSE:  Queries the global *TX for whether or not the given entity
;           ISA member of the given class
; INPUT:    entity, class: items in a class hierarchy
; OUTPUT:   boolean indicating hierarchical relationship
(defun IS-SUBCLASS (entity class &optional (sub-hier *TX))
    (cond
        ; Base case: same predicates
        ((equal entity class) t)
        ; Base case: "MEMB atm1 atm2" is in the hierarchy
        ((member (list 'MEMB entity class) sub-hier) t)
        
        ; Recursive case: "MEMB entity predi" where "MEMB predi class" for some i
        (t (let*
             ; Find a parent of entity (hierarchy entry with second entry class)
             ((parent (find-if (lambda (p) (equal (second p) entity)) sub-hier))
              
              ; Need to remove that entry to avoid infinite loop
              (next-hier (remove parent sub-hier)))
             ; If predi MEMB class, then success. Else look for a different predi
             ; (predi is the third element of parent, since parent = (MEMB entity predi))
             (cond 
                 ; If we didn't find such a parent, we're done
                 ((null parent) nil)
                 ; Case 1: predi MEMB class: success
                 ((IS-SUBCLASS (third parent) class next-hier) t)
                 ; Case 2: entity MEMB predi ... MEMB class for some different predi
                 ((IS-SUBCLASS entity class next-hier) t)
                 ; Else: failure
                 (t nil)
            ))
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: START-FIND-CON
; PURPOSE:  A helper for FIND-CON that locates a particular atom inside wm, 
;           and returns the remainder of wm in the given direction
; INPUT:    mycon: where to start searching in *WM
;           wm: current recursive state of the *WM we're searching in
;           dir: either 'BEF or 'AFT indicating search direction
; OUTPUT:   list of remaining CONatoms in a given direction
(defun START-FIND-CON (mycon wm dir)
    (cond
        ; Why have two cases when we can have one? Just do a forward search in
        ; the reversed list for a BEF search.
        ((equal dir 'BEF) (START-FIND-CON mycon (reverse *WM) 'AFT))
        ; Base case: searched everything
        ((null wm) nil)
        ; Base case: found what we were looking for, so just return the rest
        ((equal (first wm) mycon) (rest wm))
        ; Recursive case: no dice, so keep looking
        (t (START-FIND-CON mycon (rest wm) dir))
    )
)

; FUNCTION: FIND-CON
; PURPOSE:  Returns the CONatom found by searching the *WM for a CONatom with a
;           pred of the given class starting at mycon in direction dir within
;           the global *WM
; INPUT:    mycon: where to start searching in *WM
;           dir: either 'BEF or 'AFT indicating search direction
;           class: predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if not found
(defun FIND-CON (mycon dir class &optional found (wm *WM))
    (cond
        ; As a first step, find our starting CON. Flag that we found it
        ; by setting optional parameter "found"
        ((not found) (FIND-CON mycon dir class t (START-FIND-CON mycon *WM dir)))
        
        ; Base case: searched everything
        ((null wm) nil)
        
        ; We already found the start atom, so start looking at predicates
        (t
             ; See if pred matches (have to expand the first atm in atmlst
             ; into its frame representation first)
             (if (IS-SUBCLASS (first (eval (first wm))) class)
                 ; A match, so return it
                 (first wm)
                 ; Else, no match; remove and try again
                 (FIND-CON mycon dir class t (rest wm))
             )
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: ACT-WITHIN?
; PURPOSE:  Returns whether or not the given CONatom appears anywhere within
;           the given CONatom's frame
; INPUT:    candidate: CONatom to look for
;           frame: frame in which to look
; OUTPUT:   boolean designating whether or not it appears
(defun ACT-WITHIN? (candidate frame)
    (cond
        ; Base case: null frame
        ((null frame) nil)
        ; Complex case: we found an atom...
        ((atom frame) (cond
            ; Base case: frame is an atom? Check equivalence to candidate
            ; If it's equivalent, return t
            ((equal candidate frame) t)
            ; Recursive case: frame is an atom and bound? Check its eval
            ((and (boundp frame) (not (null (eval frame)))) (ACT-WITHIN? candidate (eval frame)))
            ; Default case: frame is an atom and unbound, so return nil since
            ; we already checked in the first case as to whether it was equal
            ; to the query conatm
            (t nil)
        ))
        ; Base case: frame is null, or single pred, so return nil
        ((<= (length frame) 1) nil)
        ; Recursive case: Check the front filler and then recurse on other
        ; slot-fillers in the current frame
        (t (or (ACT-WITHIN? candidate (front-filler frame)) (ACT-WITHIN? candidate (pop-slot frame))))
    )
)

; FUNCTION: UNUSED-ACT?
; PURPOSE:  Returns whether or not the given CONatom appears anywhere within
;           a CONatom's frame of the *WM
; INPUT:    candidate: CONatom to look for
; OUTPUT:   boolean designating whether or not it appears
(defun UNUSED-ACT? (candidate)
    ; Check if the candidate is within any of the CONatoms in the *WM
    ; We'll negate the output for nice semantic interpretation
    (not (loop for conatom in *WM do
        (if (ACT-WITHIN? candidate (eval conatom)) (return t))
    ))
)

; FUNCTION: MAIN-ACT
; PURPOSE:  Returns the first CONatom in *WM whose frame is an ACT, and is NOT
;           embedded in another *WM frame
; INPUT:    N/A
; OUTPUT:   CONatom
(defun MAIN-ACT (&optional (wk-mem *WM))
    (let* ((candidate (first wk-mem)))
        (cond
            ; Base case: ran out of candidates to check, so return nil
            ((null candidate) nil)
            ; Base case: first element is an ACT and is unused in the *WM,
            ; so we've found our MAIN-ACT
            ((and (IS-SUBCLASS (first (eval candidate)) 'ACT) (UNUSED-ACT? candidate)) candidate)
            ; Recursive case: Either the candidate wasn't an ACT or it was
            ; used somewhere in the *WM, so keep looking
            (t (MAIN-ACT (rest wk-mem)))
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: GEN-DEMS
; PURPOSE:  Inserts the given CONATM at the front of every partial-demon-
;           instance in the given demons and then adds those completed
;           demon instantiations to the global *DM
; INPUT:    demons: a list of partial-demon-instantiations of the format:
;           ((demon-name arg2 arg3 ...)*)
;           conatm: a CONatom indicating which frame the demons work for
; OUTPUT:   current state of *DM after insertion
(defun GEN-DEMS (demons conatm)
    (cond
        ((null demons) *DM)
        (t
            (let* (
                (dem (first demons))
                ; Construct new demon as ((first dem) conatm (rest dem))
                (newdemon (append (list (first dem)) (list conatm) (rest dem) )))
                ; Update global *DM
                (setq *DM (cons newdemon *DM))
                ; Recurse on any remaining demons
                (cons newdemon (GEN-DEMS (rest demons) conatm))
            )
        )
    )
)



; -----------------------------------------------------------------------------
; Here There Be Demons
; -----------------------------------------------------------------------------

; FUNCTION: DEM-SRCH
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then sets the top-level gap in the
;           myslot slot in mycon to the found CONatom. Returns the found
;           CONatom if found, NIL otherwise. 
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name of the gap in myslot to bind when found
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-SRCH (mycon myslot dir class)
    (let* ((frame (eval mycon))
           (gap (GET-SF myslot frame))
           (found (FIND-CON mycon dir class)))
        ; If we find a binding from our FIND-CON result, then set gap to found,
        ; and return found for having successfully completed; otherwise nil
        (if found (progn (set gap found) found) nil)
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: DEM-AMEND
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then inserts the top-level slot given
;           by myslot in the found CONatom to the given myfiller. 
;           Returns the found CONatom if found, NIL otherwise. 
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name to insert
;           myfiller: the filler of myslot to insert
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-AMEND (mycon myslot myfiller dir class)
    (let* ((found (FIND-CON mycon dir class)))
        (if (null found)
            ; If we didn't find anything, then just return nil
            nil
            ; Otherwise, we found something, so perform the insertion
            (progn
                (set found (AMEND-SF myslot myfiller (eval found)))
                ; Finally, return the found CONatom
                found
            )
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: DEM-REF
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then inserts the top-level slot given
;           by myslot in the found CONatom to the given mycon. 
;           Returns the found CONatom if found, NIL otherwise. 
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name to insert
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-REF (mycon myslot dir class)
    ; Functionally equivalent to calling DEM-AMEND with mycon for the filler
    (DEM-AMEND mycon myslot mycon dir class)
)



; -----------------------------------------------------------------------------
; Workhorse Functions
; -----------------------------------------------------------------------------

; FUNCTION: DEM-EXEC
; PURPOSE:  Repeatedly calls each active demon instantiation within the global
;           *DM until all active demons return nil. Whenever a demon
;           returns something non-nil, we remove it from the global *DM
;           and then call each remaining demon again
; INPUT:    N/A
; OUTPUT:   Status of *DM after executing all active demons (a list of all
;           remaining, active demon instantiations)
(defun DEM-EXEC (&optional no-change)
    (cond
        ; Base case: nothing to do, or all demons successful
        ((null *DM) nil)
        ; Flag for change based on last recursive call (no-change will
        ; be truthy when it is not null)
        (no-change *DM)
        ; Gather all unsuccessful demons
        (t (DEM-EXEC
               (let* (
                    ; old-dems stores the state before running demons
                    (old-dems *DM)
                    ; We'll collect all those demons that didn't die
                    (collected (loop for dem in *DM append
                        ; Call demon
                        (if (apply (first dem) (rest dem))
                             nil        ; If success, don't add back
                             (list dem) ; On failure, do add back
                    ))))
                    ; Update the *DM with remaining demons
                    (setq *DM collected)
                    ; We'll add as a param whether or not any change in *DM
                    ; occured based on its old value
                    (and (subsetp old-dems *DM) (subsetp *DM old-dems))
               ))
        )
        ; Recursive call will check whether or not we removed anything
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: PARSE-SENT
; PURPOSE:  Performs a conceptual anaylsis of the input SENT using the known
;           phrases and interpretations within the global *LM.
; INPUT:    sent: list of English words comprising a sentence
; OUTPUT:   frame consisting of: (EXPAND (MAIN-ACT))
(defun PARSE-SENT (sent)
    ; match-result = (((phrase) frame (demons*)) (remaining-sent))
    (let* ((match-result (LOOKUP-PHRASE sent *LM))
           (pfd (first match-result))
           (phrase (first pfd))
           (frame (second pfd))
           (demons (third pfd))
           (next-sent (second match-result)))
    
    ; When we've exhausted the sentence, return the EXPAND of MAIN-ACT
    (if (null sent)
        (EXPAND (MAIN-ACT))
        ; Else, process frame/demons, then recurse
        (progn
            ; Create a new CONatom to house the newly found frame
            (let* ((newcon (NEWATM 'CON)))
                ; Bind that to the found frame
                (set newcon (NEWGAPS frame))
                (setq *WM (append *WM (list newcon)))
                ; Generate demons that now work for this conatom
                (GEN-DEMS demons newcon)
            )
            ; Call the active demons, which will update the DEMON-MEM after
            ; reaching quiescence
            (DEM-EXEC)
            ; Finally, recurse: process next part of sentence after removing
            ; matched phrase
            (PARSE-SENT next-sent)
        )
    ))
)

; -----------------------------------------------------------------------------



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


 (setq LST1 '((INFORM SITU (S1) RECIP (V HUMX01) AGENT (HUMAN ROLE (ONCOLOGIST)) OBJECT (STATE AGENT (V HUMX01) OBJECT (V OBJX01))) (AFTER ANTE (S1) CONSEQ (V SXB1))))

 (setq LST2 '((INFORM AGENT (V HUMX02) RECIP (HUMAN F-NAME (GEORGE) GENDER (MALE)) OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE) GENDER (MALE)) OBJECT (CANCER TYPE (TERMINAL))) SITU (V SXA1)) (AFTER CONSEQ (S2) ANTE (V SXA1))))

 ;(print (UNIFY-FR LST1 LST2) )

; SHOULD RETURN:
; (T    ((V SXA1) (S1))
;       ((V HUMX01) (HUMAN F-NAME (GEORGE) GENDER (MALE)))
;       ((V HUMX02) (HUMAN ROLE (ONCOLOGIST))) ((V OBJX01) (CANCER TYPE (TERMINAL))) 
;       ((V SXB1) (S2))
; )


; -----------------------------------------------------------------------------


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
        )
        ((atom (car frm))
            (setq returnFrame (append returnFrame (list(car frm))))
            (RECURSIVELY-CHECKS-FRAME (cdr frm) bds returnFrame)
        )
        ( (and (listp (car frm)) (equal (length (car frm)) 1))
            (setq returnFrame (append returnFrame (list (car frm))))
            (RECURSIVELY-CHECKS-FRAME (cdr frm) bds returnFrame)
        )
        ( (and (listp (car frm)) (equal (length (car frm)) 2) (equal (CHECKS-AGAINST-BDS (car frm) bds) nil))
            (setq returnFrame (append returnFrame (list (car frm))))
            (RECURSIVELY-CHECKS-FRAME (cdr frm) bds returnFrame)
        )
        ( (and (listp (car frm)) (equal (length (car frm)) 2))
            (setq returnFrame (append returnFrame (CHECKS-AGAINST-BDS (car frm) bds)))
            (RECURSIVELY-CHECKS-FRAME (rest frm) bds returnFrame)
        )
        ( (and (listp (car frm)) (> (length (car frm)) 2))
             (setq returnFrame (append returnFrame (list (RECURSIVELY-CHECKS-FRAME (car frm) bds nil)))))
        (T 
            (RECURSIVELY-CHECKS-FRAME (cdr frm) bds returnFrame)
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

; -----------------------------------------------------------------------------


; FUNCTION: MP-INFER
; PURPOSE:  Attempts to unify the given rule's premises on the given facts,
;           and if successful, returns the conclusion of the rule with SUBST-FR
;           called on it using the successful binding list
; INPUT:    RULE: an if-then rule
;           O-FRAMES: a list of facts / concepts
; OUTPUT:   conclusion if successfully unified; nil otherwise
(defun MP-INFER (rule o-frames)
                    ;an extra cdr here because of the prem 
   (let* ((prem (cdr (car rule))) (conc  (first (cdr (first (cdr  rule)) ))) ) ;setting the premises and conclusion here

    (if (null prem  )  ;if prem is nil here, we return nil
       nil)
    (if (null conc) 
        nil) ;if conclusion is nil here we return nil
                   ;cdr here because we don't want no T in our list
    (let* ((oriList  (UNIFY-FR  prem o-frames  )))
        (print (SUBST-FR conc oriList))  ;return the result here
        (print "------------")
        (print conc)
        (print "------------")
        (print oriList)
        
    )
)
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
    (let* ((newC nil ))
        (loop for mRule in rules do 
            (setq newC (MP-INFER mRule epmem) )

        
        )

        (if (null newC) 
            (return-from FRW-CHAIN new-epmem)
            (FRW-CHAIN rules epmem (cons newC new-epmem))
        )
    )
)



;(setq RULE-51 '((PREMISES (OWNS AGENT (V a1) OBJECT (V o1)) (ISA OBJECT (V o1) TYPE (ICE-CREAM))) (CONCLU (HAPPY AGENT (V a1)) )))

;(setq RULE-52 '((PREMISES (HAPPY AGENT (V a1))) (CONCLU (AWESOME AGENT (V a1)) )))

;(print (FRW-CHAIN (list RULE-51 RULE-52) '((OWNS AGENT (ANDREW) OBJECT (DRUMSTICK)) (ISA OBJECT (DRUMSTICK) TYPE (ICE-CREAM)))) )

; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; Problem 4 Rule Definitions
; -----------------------------------------------------------------------------

(setq RULE-1 '((PREMISES
                    (TEACH AGENT(V x)
                        RECIP(HUMAN TYPE (STUDENTS))
                        OBJECT(CHEM)
                        LOC(HIGHSCHOOL)
                        SITU(V z))
               )
               (CONCLU
                    (STATE AGENT (V x)
                        TYPE(EMOTIONAL)
                        VALUE(HAPPY)
                        STIU(V z))
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
                    (KNOWS 
                    AGENT (V x)
                    OBJECT( STATE AGENT (V x)
                            OBJECT (CANCER TYPE (TERMINAL)))
                    SITU (V sa)
                 )   
               )
               (CONCLU
                 (
             STATE TYPE (EMOTIONAL)
                    AGENT(V x)
                    VALUE (SAD)
                    SITU (V sa)
                )
           ))
      
      RULE-4 '((PREMISES
                    (MARRIED AGENT (V x)
                        OBJECT (V y)
                        SITU (V sa))
                    (STATE TYPE (PHYSICAL)
                        AGENT (V y)
                        VALUE (PREGNANT)
                        SITU (V sb))
               )
               (CONCLU
                    (SEX_ACT AGENT (V x)
                        OBJECT (V y))
               ))
      
      RULE-5 '((PREMISES
                (TEACH AGENT (V x)
                    RECIP(HUMAN TYPE (STUDENTS))
                    OBJECT(CHEM)
                    LOC(HIGHSCHOOL)
                    SITU(V sa))
                (STATE TYPE (EMOTIONAL)
                    AGENT(V x)
                    VALUE(SAD)
                    SITU(V sb))
                (AFTER ANTE (V sa)
                    CONSEQ(V sb))
               )
               (CONCLU
                    (MAKES AGENT (V x)
                        OBJECT(COCAINE)
                        SITU(V sb))
               ))
      
      RULE-6 '((PREMISES
                (INGEST AGENT (V x)
                    OBJECT (COCAINE)
                    SITU(V sa))
                (STATE AGENT (V x)
                    OBJECT(LESIONS AREA (NOSE))
                    SITU (V sb)
                )
                (AFTER ANTE (V sa)
                    CONSEQ(V sb))
               )
               (CONCLU
                (CAUSE ANTE (INGEST AGENT (V x)
                    OBJECT (COCAINE)
                    SITU (V sa))
                   CONSEQ (STATE AGENT (V x)
                        OBJECT (LESIONS AREA (NOSE))
                        SITU (V sb)))
               ))
      
      RULE-7 '((PREMISES
                (MAKES AGENT (V x)
                    OBJECT (COCAINE)
                    SITU (V sa))
                (INGEST AGENT (V y)
                    OBJECT(COCAINE)
                    SITU(V sb))
                (AFTER ANTE (V sa)
                    CONSEQ(V sb))
               )
               (CONCLU
                (ACQUIRED AGENT (V y)
                    OBJECT (COCAINE)
                    FROM (V x)
                    SITU (V sb))
               ))
)

(setq FR1 '(ENABLES ANTE (HAVE AGENT (V XX01)
                               OBJECT (MONEY))
                    CONSEQ (TRAVEL AGENT (V XX01)
                                   TO (V QQ01)))
      
      FR2 '(ENABLES CONSEQ (TRAVEL TO (FRANCE)
                                   AGENT (V YY01))
                    ANTE (HAVE AGENT (JOE)
                               OBJECT (V ZZ01)))
      
      FR3 '(ENABLES ANTE (HAVE AGENT (V XX01)
                               OBJECT (MONEY))
            CONSEQ (TRAVEL AGENT (V XX01)
                           TO (FRANCE)))
      
      FR4 '(ENABLES ANTE (HAVE AGENT (JOE)
                               OBJECT (V ZZ01))
                    CONSEQ (TRAVEL AGENT (V YY01)
                                   TO (V YY01)))
      
      FR6 '(KNOWS AGENT (V HX1)
                  OBJECT (STATE TYPE (PHYSICAL)
                                AGENT (V HX1)
                                OBJECT (CANCER TYPE (V TY01)))
                  SITU (V SS01))
      
      FRM-A '(INFORM RECIP (HUMAN F-NAME (GEORGE))
                     OBJECT (STATE OBJECT (CANCER TYPE (TERMINAL)))                         
                     AGENT (V HUMX08))
      
      FRM-B '(INFORM SITU (S1)
                     RECIP (V HUMX09)
                     AGENT (HUMAN ROLE (ONCOLOGIST))
                     OBJECT (STATE OBJECT (V OBJX07)
                                   AGENT (HUMAN F-NAME (GEORGE))))
      
      
      
      LST1 '(
              (INFORM SITU (S1)
                      RECIP (V HUMX01)
                      AGENT (HUMAN ROLE (ONCOLOGIST))
                      OBJECT (STATE AGENT (V HUMX01)
                                    OBJECT (V OBJX01)))
              
              (AFTER ANTE (S1)
                     CONSEQ (V SXB1))
            )
      LST2 '(
              (INFORM AGENT (V HUMX02)
                      RECIP (HUMAN F-NAME (GEORGE)
                                   GENDER (MALE))
                      OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE)
                                                 GENDER (MALE))
                                    OBJECT (CANCER TYPE (TERMINAL)))
                      SITU (V SXA1))
              
              (AFTER CONSEQ (S2)
                     ANTE (V SXA1))
            )
      
      
      
      RULE-51 '((PREMISES
                  (OWNS AGENT (V a1)
                        OBJECT (V o1))
                  
                  (ISA OBJECT (V o1)
                       TYPE (ICE-CREAM))
                )
                (CONCLU
                  (HAPPY AGENT (V a1))
                ))
      
      RULE-52 '((PREMISES
                  (HAPPY AGENT (V a1))
                )
                (CONCLU
                  (AWESOME AGENT (V a1))
                ))
      
      
      
      EP0 '(AFTER ANTE (S0)
                  CONSEQ (S1))
      
      EP1 '(TEACH AGENT (HUMAN F-NAME (GEORGE)
                               GENDER (MALE))
                  RECIP (HUMAN TYPE (STUDENTS))
                  OBJECT (CHEM)
                  LOC (HIGHSCHOOL) 
                  SITU (S1))
      
      EP2 '(INFORM AGENT (HUMAN ROLE (ONCOLOGIST))
                   RECIP (HUMAN F-NAME (GEORGE)
                                GENDER (MALE))
                   OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE)
                                              GENDER (MALE))
                                 OBJECT (CANCER TYPE (TERMINAL)))
                   SITU (S1))
      
      EP3 '(STATE TYPE (PHYSICAL)
                  AGENT (HUMAN F-NAME (WINNIE)
                               GENDER (FEMALE))
                  VALUE (PREGNANT)
                  SITU (S2))
      
      EP4 '(MARRIED AGENT (HUMAN F-NAME (GEORGE)
                                 GENDER (MALE))
                    OBJECT (HUMAN F-NAME (WINNIE)
                                  GENDER (FEMALE))
                    SITU (S1))
      
      EP5 '(AFTER ANTE (S1)
                  CONSEQ (S2))
      
      EP6 '(AFTER ANTE (S2)
                  CONSEQ (S3))
      
      EP7 '(AFTER ANTE (S3)
                  CONSEQ (S4))
      
      EP8 '(INGEST AGENT (HUMAN F-NAME (RICK)
                                GENDER (MALE))
                   OBJECT (COCAINE)
                   SITU (S4))
      
      EP9 '(AFTER ANTE (S4)
                  CONSEQ (S5))
      
      EP10 '(STATE AGENT (HUMAN F-NAME (RICK)
                                GENDER (MALE))
                   OBJECT (LESIONS AREA (NOSE))
                   SITU (S5))
      
      EP11 '(AFTER ANTE (S2)
                   CONSEQ (S4))
      
      EPMEM (LIST EP0 EP1 EP2 EP3 EP4 EP5 EP6 EP7 EP8 EP9 EP10 EP11)
      
      
      INF1 '(STATE AGENT (HUMAN F-NAME (GEORGE)
                                GENDER (MALE))
                   TYPE (EMOTIONAL)
                   VALUE (HAPPY)
                   SITU (S1))
      
      INF2 '(KNOWS AGENT (HUMAN F-NAME (GEORGE)
                                GENDER (MALE))
                   OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE)
                                              GENDER (MALE))
                                 OBJECT (CANCER TYPE (TERMINAL)))
                   SITU (S2))
      
      INF3 '(STATE TYPE (EMOTIONAL)
                   AGENT (HUMAN F-NAME (GEORGE)
                                GENDER (MALE))
                   VALUE (SAD)
                   SITU (S2))
      
      INF4 '(SEX-ACT AGENT (HUMAN F-NAME (GEORGE)
                                  GENDER (MALE))
                     OBJECT (HUMAN F-NAME (WINNIE)
                                   GENDER (FEMALE))
                     SITU (S1))
      
      INF5 '(MAKES AGENT (HUMAN F-NAME (GEORGE)
                                GENDER (MALE))
                   OBJECT (COCAINE)
                   SITU (S2))
      
      INF6 '(CAUSE ANTE (INGEST AGENT (HUMAN F-NAME (RICK)
                                             GENDER (MALE))
                                OBJECT (COCAINE)
                                SITU (S4))
                   CONSEQ (STATE AGENT (HUMAN F-NAME (RICK)
                                              GENDER (MALE))
                                 OBJECT (LESIONS AREA (NOSE))
                                 SITU (S5)))
      
      INF7 '(ACQUIRED AGENT (HUMAN F-NAME (RICK)
                                   GENDER (MALE))
                      OBJECT (COCAINE)
                      FROM (HUMAN F-NAME (GEORGE)
                                  GENDER (MALE))
                      SITU (S4))
)


; -----------------------------------------------------------------------------

(print 
   (MP-INFER RULE-2 (LIST EP2 EP5))
)
(setq BD1 '(T ((V HX1) (HUMAN F-NAME (GEORGE) GENDER (MALE)))
         ((V SS01) (S2))
         ((V TY01) (TERMINAL))
      )
      )

;(print 
 ;   (SUBST-FR FR6 BD1)

;)