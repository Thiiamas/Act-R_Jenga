; Represents if the tower fell
(defvar *tower-fell* nil)               

; Display is visible or not
(defvar *visible* t)                    


(defvar *human* nil)

; The internal state of the jenga game, for each row, contains an array of three blocks and their state (t: block present, nil: block removed)
; ex: ((T T T) (T NIL T) (NIL T NIL) (T T T) ...)
(defvar *jenga_state* nil)             

; Keeps a reference to the buttons
(defvar *jenga_buttons* nil)   

; The number of total rows
(defvar *nb_blocks* 54)

; The index of the top row
(defvar *top_row* (- (/ *nb_blocks* 3) 1))

; Determines on which column the next removed block will be placed
(defvar *next_col_place* 0)

; The y position for the bottom row
(defvar *bottom_y* 850)

; The x position of the first block of side 1
(defvar *side1_x* 50)

; The x position of the first block of side 2
(defvar *side2_x* 130)

; The width of the face of a jenga block
(defvar *block_width* 15)

; The width of the window
(defvar *window_width* 300)

; The height of the window
(defvar *window_height* 900)

; The number of trials per blocks
(defvar *default-nb-trials-per-sets* 12)

; The number of trial blocks to be completed
(defvar *default_nb_sets* 4)

(defvar *nb_blocks_removed* 0)

(defvar *experiment-window* nil)

; Function that builds the display
(defun build-display ()
    (setf *experiment-window* (open-exp-window "Jenga"
                                             :visible *visible*
                                             :width *window_width*
                                             :height *window_height*
                                             :x 200
                                             :y 25))
  
    (allow-event-manager *experiment-window*)
)

; Sets the RESET button on the display
(defun build-reset-button ()
    ; Only show if human is playing
    (if (eq *human* t) 
        ; Reset button
        (add-button-to-exp-window :x (+ *block_width* (- *side2_x* *side1_x*)) :y 10 :height 15 :width 50 :text "RESET" :action (lambda (x) (reset-display)))
    )
)

; Clear the display and displays the jenga game
(defun display-jenga () 
    (setq *jenga_blocks_side_button* (make-array 18))
    
    (clear-exp-window) 

    (build-reset-button)

    ; Side labels
    ;; (add-text-to-exp-window :text "Side 1" :x (+ *side1_x* 0) :y (+  *bottom_y* 20))
    ;; (add-text-to-exp-window :text "Side 2" :x (+ *side2_x* 0) :y (+  *bottom_y* 20))

    ; For each rows, add the buttons reprensenting blocks
    (loop for row from 0 to *top_row*
        do (let ((r row) (x (if (equal (mod row 2) 0) *side1_x* *side2_x*))) 
            ; Side of the block on opposite side
            

            ; The three blocks we can remove
            (put-block-at-position r 0)
            (put-block-at-position r 1)
            (put-block-at-position r 2)
        )
    )

    (proc-display :clear t)
)

; Function that shows tower fall display
(defun display-fall ()
    ; Clear displays
    (clear-exp-window) 
    
    (build-reset-button)

    ; Tower fell label
    (add-text-to-exp-window :text "Tower fell!" :x (- *side2_x* *side1_x*) :y 100)

    ; Puts random blocks in a pile to look cool
    (loop for row from 0 to 54
        do (add-button-to-exp-window :x (random *window_width*) :y (- *bottom_y* (random 100)) :height *block_width* :width (+ *block_width* (* *block_width* (* (random 2) 3))) :text "" )
    )

    (proc-display :clear t)
)

; Resets the game
(defun reset-display ()
    ; Initialize the variables for a new game
    (setf *tower-fell* nil)
    (setq *jenga_state* (make-array '(54 3) :initial-element t))
    (setq *jenga_buttons* (make-array '(54 3) :initial-element nil))
    (setf *top_row* (- (/ *nb_blocks* 3) 1))
    (setf *next_col_place* 0)

    (display-jenga)
)

; Check if the row index in parameters is a good row (does not cause tower to fall)
(defun check-row-ok (row) 
    (if (and (not (equal row *top_row*)) 
                (or (and 
                    ; [ ] _ _ causes falling
                    (equal (aref *jenga_state* row 0) t)
                    (equal (aref *jenga_state* row 1) nil)
                    (equal (aref *jenga_state* row 2) nil)
                )
                (and
                    ; _ _ [ ] causes falling
                    (equal (aref *jenga_state* row 0) nil)
                    (equal (aref *jenga_state* row 1) nil)
                    (equal (aref *jenga_state* row 2) t)
                )
                (and 
                    ; _ _ _ causes falling
                    (equal (aref *jenga_state* row 0) nil)
                    (equal (aref *jenga_state* row 1) nil)
                    (equal (aref *jenga_state* row 2) nil)
                ))
        )
        nil ; Return not okay
        t   ; Return okay
    )   
)

; Puts a block at the position in parameters
(defun put-block-at-position (row col)
    ; Adds the block on the other side if it's the first block to be added on that row
    (if (not (aref *jenga_buttons* row col))
        (add-button-to-exp-window :x (if (equal (mod row 2) 0) *side2_x* *side1_x*) :y (- *bottom_y* (* row *block_width*)) :height *block_width* :width (* *block_width* 3) :text "" )
    )

    ; Adds the block and keep a reference of it in *jenga_buttons*
    (setf 
        (aref *jenga_buttons* row col) 
        (add-button-to-exp-window 
            :x      (+ (if (equal (mod row 2) 0) *side1_x* *side2_x*) (* *block_width* col)) 
            :y      (- *bottom_y* (* row *block_width*)) 
            :height *block_width* 
            :width  *block_width* 
            :text   ""  
            :color  (if (>= row *top_row*) 'gray 'blue) 
            :action (if (>= row *top_row*) nil (lambda (button) (block-removed button row col)))
        )
    )
)

; Put a the block removed on top of the tower
(defun put-block-on-top ()
    (let ((row (+ *top_row* 1)) (x (if (equal (mod (+ *top_row* 1) 2) 0) *side1_x* *side2_x*)))
        ; Put the block on the top
        (put-block-at-position row *next_col_place*)
        
        ; If it was the last block to be put on the top, we update the row so that its blocks
        ; are now able to be removed and also increments the index of the top row
        (if (equal *next_col_place* 2)
            (let ((row_below *top_row*))
                (incf *top_row*)
                (setf *next_col_place* 0)

                ; Remove the grey blocks from the row below
                (remove-items-from-exp-window (aref *jenga_buttons* row_below 0))
                (remove-items-from-exp-window (aref *jenga_buttons* row_below 1))
                (remove-items-from-exp-window (aref *jenga_buttons* row_below 2))

                ; Replace them with new blocks (the conditions in this function will make them blue because
                ; they are no longer on the top)
                (put-block-at-position row_below 0)
                (put-block-at-position row_below 1)
                (put-block-at-position row_below 2)

            )
            ; If its not the last block to be added, simply increment the column index for the next block to be added
            (incf *next_col_place*) 
        )
    )
)

; Event happening when the user or model clicks on a block to remove it
(defun block-removed (button row col)

    ; Remove the button (block) from the display
    (remove-items-from-exp-window button)

    ; Updates the internal state of jenga (set nil for the block)
    (setf (aref *jenga_state* row col) nil)
    (setf (aref *jenga_buttons* row col) nil)

    ; Test if the new state of the row causes tower to fall
    (if (not (check-row-ok row))
        ; If causes tower to fall, display the "Tower fell!" screen
        (progn
            (setf *tower-fell* t)                
            (display-fall)
        )
        ; If it doesn't cause the tower to fall, put the block back on top of the tower
        (progn
            (put-block-on-top)
            (incf *nb_blocks_removed*)
        )
    )

    (proc-display :clear t)
)


; Runs the model for 12 trials and return the average of HEADS
(defun do-trials (nb_trials) 
   (let ((block-result nil))

      ; For each trial in the block
      (dotimes (l nb_trials block-result)
         (setf *nb_blocks_removed* 0)

        
         (install-device *experiment-window*)   
         ; Run the model
         (reset-display)
         (run (* 60 30) :real-time *visible*)  

         ; Push 1 if model chose HEADS, 0 if TAILS
         (push *nb_blocks_removed* block-result)
      )
      
      ; Return the average of blocked removed for the experiment
      (/ (apply '+ block-result) nb_trials)
   )
)

; Runs the model for 4 blocks of 12 trials and return the average HEADS choosen for each blocks
; ex (0.50 0.65 0.75 0.81)
(defun do-sets (nb_sets nb_trials) 
   (let ((result nil))

      ; For each trial blocks
      (dotimes (i nb_sets result)
         ; Add the average for the trials of that block in the results
         (push (do-trials nb_trials) result)
      )

      ; Reverse because push prepends
      (reverse result)
   )
)

(defun model-jenga (nb_experiments &optional nb_sets nb_trials)
    (if (eq nb_sets nil) (setf nb_sets *default_nb_sets*))
    (if (eq nb_trials nil) (setf nb_trials *default-nb-trials-per-sets*))
    
    (let (  (result (make-list nb_sets :initial-element 0))
            (p-values (list '(try-another-row 0) '(try-random-block 0)))  )

        (format t "~d experiment(s) of ~d set(s) of ~d trial(s)" nb_experiments nb_sets nb_trials)

        (build-display)

        ; For N experiments
        (dotimes (i nb_experiments result)
            ; Resets the model
            (reset)

            ; Do an experiment (4 blocks of 12 trials) and add its results to sum of results from all experiments
            (setf result (mapcar '+ 
                        result 
                        (do-sets nb_sets nb_trials)))
         
            (setf p-values (mapcar (lambda (x) 
                                 (list (car x) (+ (second x) (production-u-value (car x)))))
                         p-values))
        )

        ; Calculate the average of each experiments (the % of heads for each blocks)
        (setf result (mapcar (lambda (x) (/ x nb_experiments)) result))

        ; Prints the results
        (format t "~%#Set      #Block removed ")
        (loop for a from 1 to nb_sets
            do (format t "~%~d         ~$" a (nth (- a 1) result))
        )
        
      (format t "~%")
      (dolist (x p-values)
        (format t "~%~12s: ~6,4f" (car x) (/ (second x) nb_experiments)))
    )
)

; Main function starting an experiment, can specify 'human to play yourself
(defun play-jenga (n &optional who ) 
    (build-display)
    (install-device *experiment-window*)   
    
    (setf *human* who)
    ; If it's a human playing, wait for human action
    (progn
        (reset-display)
        (wait-for-human)
    )
)

; Utility function to wait for user interaction
(defun wait-for-human ()
  (while (not *tower-fell*)
    (allow-event-manager *experiment-window*))
  (sleep 1))

; Utility function to return the utility of a production
(defun production-u-value (prod)
   (caar (no-output (spp-fct (list prod :u)))))

(clear-all)

(define-model jenga 
(sgp :v nil :esc t :egs 0.5 :show-focus t :trace-detail medium :ul t :ult t :ans .5 :mp 16 :rt 0)

(chunk-type goal 
    state 
    left-block          ; Keep track of the left block state (0 = not present, 1 = present)
    middle-block        ; Keep track of the middle block state (0 = not present, 1 = present)
    right-block         ; Keep track of the right block state (0 = not present, 1 = present)
    left-block-pos      ; Keep track of the left block position (for click position)
    middle-block-pos    ; Keep track of the middle block position (for click position)
    right-block-pos     ; Keep track of the right block position (for click position)
    block-to-remove     ; Keep track of the block that was decided to be removed
    block-to-remove-pos ; Keep track of the block that was decided to be removed
    block-y             ; Keep track of the row being looked at
    min-x               ; Keep track of the side being looked at
    max-x
    was-random)              ; Keep track of the side being looked at

(chunk-type good-row left-block middle-block right-block block-to-remove)
(chunk-type bad-row  left-block middle-block right-block block-removed)

(define-chunks 
    (start) (find-block) (looking) (find-other-blocks-same-row) (choose) (end) (check-row-choice) (try-remember-row) (remove-random-block) 
    (move-mouse) (wait-for-click) (try-again) (look-for-fall) (remove-block) (attend-failure) (check-failure-text) (look-for-failure)
    (wait-for-reset) (waiting-for-new-towers) (try-another-row-or-try-random)
    (goal isa goal state start left-block 0 middle-block 0 right-block 0 block-to-remove nil block-y nil min-x nil max-x nil was-random nil))

; Starts the trial, find a block to remove
(p start
   =goal>
      isa      goal
      state    start
   ?visual-location>
      buffer   empty
  ==>
   =goal>
      state    find-block)

; Find an unnattended block (blue button)
(p find-first-block
   =goal>
      isa       goal
      state     find-block
   ?visual-location>
      buffer   empty
  ==>
    +visual-location>
        isa       visual-location
        :attended  nil
        kind      oval            ; 'Oval' is kind 'button'
        color     blue
    =goal>
        state     looking
        left-block      0
        middle-block    0
        right-block     0
        block-to-remove nil
    -imaginal>)

; Attending left block of side 1
(p attend-left-block-side1 
   =goal>
      isa        goal
      state      looking
   =visual-location>                
      screen-x   =screen-x
      screen-y   =screen-y
      > screen-x 50             ; side1_x
      < screen-x 65             ; side1_x + block_width
   ?visual>                         
      state      free
  ==>
   =goal>
      state      find-other-blocks-same-row    
      left-block        1
      left-block-pos    =visual-location
      min-x      50
      max-x      95
      block-y    =screen-y
    +visual>
      isa        move-attention
      screen-pos =visual-location)

; Attending left block of side 2
(p attend-left-block-side2 
   =goal>
      isa        goal
      state      looking
   =visual-location>                
      screen-x   =screen-x
      screen-y   =screen-y
      > screen-x 130            ; side2_x
      < screen-x 145            ; side2_x + block_width

   ?visual>                         
      state      free
  ==>
   =goal>
      state             find-other-blocks-same-row    
      left-block        t
      left-block-pos    =visual-location
      min-x      130
      max-x      175
      block-y    =screen-y
    +visual>
      isa        move-attention
      screen-pos =visual-location)

; Attending middle block of side 1
(p attend-middle-block-side1 
   =goal>
      isa        goal
      state      looking
   =visual-location>                
      screen-x   =screen-x
      screen-y   =screen-y
      > screen-x 65                 ; side1_x + block_width
      < screen-x 80                 ; side1_x + (block_width * 2)

   ?visual>                         
      state      free
  ==>
   =goal>
      state      find-other-blocks-same-row    
      middle-block        t
      middle-block-pos    =visual-location
      min-x      50
      max-x      95
      block-y    =screen-y
      
    +visual>
      isa        move-attention
      screen-pos =visual-location)

; Attending middle block of side2
(p attend-middle-block-side2             
   =goal>
      isa        goal
      state      looking
   =visual-location>              
      screen-x   =screen-x
      screen-y   =screen-y
      > screen-x 145                ; side2_x + block_width
      < screen-x 160                ; side1_x + (block_width * 2)
   ?visual>                         
      state      free
  ==>
   =goal>
      state      find-other-blocks-same-row    
      middle-block        t
      middle-block-pos    =visual-location
      min-x      130
      max-x      175
      block-y    =screen-y
    +visual>
      isa        move-attention
      screen-pos =visual-location)
      
; Attending right block of side 1
(p attend-right-block-side1 
   =goal>
      isa        goal
      state      looking
   =visual-location>                
      screen-x   =screen-x
      screen-y   =screen-y
      > screen-x 80                 ; side1_x + (block_width * 2)
      < screen-x 95                 ; side1_x + (block_width * 2)
   ?visual>                         
      state      free
  ==>
   =goal>
      state      find-other-blocks-same-row    
      right-block        t
      right-block-pos    =visual-location
      min-x      50
      max-x      95
      block-y    =screen-y
    +visual>
      isa        move-attention
      screen-pos =visual-location)
      
; Attending right block of side 1
(p attend-right-block-side2
   =goal>
      isa        goal
      state      looking
   =visual-location>                
      screen-x   =screen-x
      screen-y   =screen-y
      > screen-x 160                ; side2_x + (block_width * 2)
   ?visual>                         
      state      free
  ==>
   =goal>
      state      find-other-blocks-same-row
      right-block        t
      right-block-pos    =visual-location
      min-x      130
      max-x      175
      block-y    =screen-y
    +visual>
      isa        move-attention
      screen-pos =visual-location)

; Find other blocks of the same row same side
(p find-other-blocks-same-row
   =goal>
      isa        goal
      state      find-other-blocks-same-row
      min-x      =min-x
      max-x      =max-x
      block-y    =screen-y
   ?visual-location>
      buffer    empty
  ==>
    +visual-location>
      isa           visual-location
      :attended     nil
      kind          oval            ; 'Oval' is kind 'button'
      color         blue
      > screen-x    =min-x
      < screen-x    =max-x
      screen-y      =screen-y
   =goal>
      state      looking
    !output!    =screen-y)

(p no-more-blocks-same-row
   =goal>
      isa           goal
      state         looking
   ?visual-location>
      buffer        failure
  ==>
   =goal>
      state         check-row-choice
    !output!        "no more block")

(p check-row-choice
   =goal>
      isa               goal
      state             check-row-choice
      left-block        =left-block
      middle-block      =middle-block
      right-block       =right-block
  ==>
   =goal>
      isa               goal
      state             try-remember-row
   +retrieval>
      ISA               good-row
      left-block        =left-block
      middle-block      =middle-block
      right-block       =right-block)

(p do-not-remember-row-configuration
    =goal>
        isa        goal
        state      try-remember-row
    ?retrieval>
        buffer  failure
    ==>
    =goal>
        isa        goal
        state      try-another-row-or-try-random)

(p try-another-row 
    =goal>
        isa         goal
        state       try-another-row-or-try-random
    ==>
    =goal>
        isa         goal
        state       find-block
)

(p try-random-block
    =goal>
        isa         goal
        state       try-another-row-or-try-random
    ==>
    =goal>
        isa         goal
        state       remove-random-block
        was-random  t
)

(p remembered-left-block-to-remove
    =goal>
        isa                 goal
        state               try-remember-row
        left-block-pos      =visual-location
    =retrieval>
        block-to-remove   left
    ==>
    =goal>
        isa                     goal
        state                   remove-block
        was-random  nil
        block-to-remove         left
        block-to-remove-pos     =visual-location)

(p remembered-middle-block-to-remove
    =goal>
        isa                     goal
        state                   try-remember-row
        middle-block-pos        =visual-location
    =retrieval>
        block-to-remove         middle
    ==>
    =goal>
        isa                     goal
        state                   remove-block
        was-random  nil
        block-to-remove         middle
        block-to-remove-pos     =visual-location)

(p remembered-right-block-to-remove
    =goal>
        isa                 goal
        state               try-remember-row
        right-block-pos      =visual-location
    =retrieval>
        block-to-remove     right
    ==>
    =goal>
        isa                     goal
        state                   remove-block
        was-random  nil
        block-to-remove         right
        block-to-remove-pos     =visual-location)

(p try-left-block
    =goal>
        isa                 goal
        state               remove-random-block
        left-block          t
        left-block-pos      =visual-location
    ==>
    =goal>
        isa                     goal
        state                   remove-block
        block-to-remove         left
        block-to-remove-pos     =visual-location)

(p try-middle-block
    =goal>
        isa                 goal
        state               remove-random-block
        middle-block        t
        middle-block-pos    =visual-location
    ==>
    =goal>
        isa                     goal
        state                   remove-block
        block-to-remove         middle
        block-to-remove-pos     =visual-location)

(p try-right-block
    =goal>
        isa                     goal
        state                   remove-random-block
        right-block             t
        right-block-pos         =visual-location
    ==>
    =goal>
        isa                     goal
        state                   remove-block
        block-to-remove         right
        block-to-remove-pos      =visual-location)

(p remove-block
    =goal>
        state                   remove-block
        block-to-remove-pos     =visual-location
    ?manual>
        state  free
    ==>
    =goal>
        state   move-mouse
    +manual>
        isa     move-cursor
        loc     =visual-location
)

(p click-mouse
   =goal>
      isa    goal
      state  move-mouse
   ?manual>
      state  free
  ==>
   =goal>
      state  wait-for-click
   +manual>
      isa    click-mouse)

(p check-block-removal-result
    =goal>
        isa    goal
        state  wait-for-click
   ?manual>
      state     free
    ==>
    =goal>
        state  look-for-fall
    !output! "Clicked")

(p look-for-failure
    =goal>
        isa         goal
        state       look-for-fall
    ==>
    =goal>
        state       attend-failure
    +visual-location>
        isa         visual-location
        :attended   nil
        > screen-y  80
        < screen-y  120
        kind        text)

; No text found, meaning
(p action-was-good-with-random
    =goal>
        isa             goal
        state           attend-failure
        left-block      =left-block
        middle-block    =middle-block
        right-block     =right-block
        block-to-remove =block-to-remove
        was-random      t
    ?imaginal>
        state   free
    ?visual-location>
        buffer  failure
    ==>
    =goal>
        state           find-block
    +imaginal>
        isa             good-row
        left-block      =left-block
        middle-block    =middle-block
        right-block     =right-block
        block-to-remove =block-to-remove)

; No text found, meaning
(p action-was-good-with-remembering
    =goal>
        isa             goal
        state           attend-failure
        left-block      =left-block
        middle-block    =middle-block
        right-block     =right-block
        block-to-remove =block-to-remove
        was-random      nil
    ?imaginal>
        state   free
    ?visual-location>
        buffer  failure
    ==>
    =goal>
        state           find-block
    +imaginal>
        isa             good-row
        left-block      =left-block
        middle-block    =middle-block
        right-block     =right-block
        block-to-remove =block-to-remove)

(p attend-failure-text
    =goal>
        isa         goal
        state       attend-failure
    =visual-location>
    ?visual>
      state      free
    ==>
    =goal>
        state       check-failure-text
    +visual>
        isa         move-attention
        screen-pos  =visual-location)

(p failed
    =goal>
        isa         goal
        state       check-failure-text
    =visual>
    ==>
    =goal>
        state       wait-for-reset
    !output!        "Failure!")

; Find an unnattended block (blue button)
(p wait-for-reset
   =goal>
      isa       goal
      state     wait-for-reset
   ?visual-location>
      buffer   empty
  ==>
    +visual-location>
        isa         visual-location
        :attended   nil
        kind        oval            ; 'Oval' is kind 'button'
        color       blue
    =goal>
        state           waiting-for-new-towers
        block-to-remove nil)

(p waiting-for-new-towers
   =goal>
      isa       goal
      state     waiting-for-new-towers
    =visual-location>
  ==>
    =visual-location>
    =goal>
        state     looking
        left-block      0
        middle-block    0
        right-block     0
        block-to-remove nil)
        
(start-hand-at-mouse)

(spp try-another-row :u 5)
(spp try-random-block :u 5)

(spp action-was-good-with-remembering :reward 10)
(spp action-was-good-with-random :reward 1)
(spp failed :reward 0)

(goal-focus goal)

)
