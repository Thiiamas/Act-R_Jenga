; Represents if the tower fell
(defvar *tower-fell* nil)               

; Display is visible or not
(defvar *visible* t)                    

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

; The y position for the bottom row
(defvar *left_x* 50)

; The y position for the bottom row
(defvar *right_x* 110)

; The width of the face of a jenga block
(defvar *block_width* 15)

; The width of the window
(defvar *window_width* 210)

; The height of the window
(defvar *window_height* 900)

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
    ; Reset button
    (add-button-to-exp-window :x (+ *block_width* (- *right_x* *left_x*)) :y 10 :height 15 :width 50 :text "RESET" :action (lambda (button) (reset)))
)

; Clear the display and displays the jenga game
(defun display-jenga () 
    (setq *jenga_blocks_side_button* (make-array 18))
    
    (clear-exp-window) 

    (build-reset-button)

    ; Side labels
    (add-text-to-exp-window :text "Side 1" :x (+ *left_x* 0) :y (+  *bottom_y* 20))
    (add-text-to-exp-window :text "Side 2" :x (+ *right_x* 0) :y (+  *bottom_y* 20))

    ; For each rows, add the buttons reprensenting blocks
    (loop for row from 0 to *top_row*
        do (let ((r row) (x (if (equal (mod row 2) 0) *left_x* *right_x*))) 
            ; Side of the block on opposite side
            

            ; The three blocks we can remove
            (put-block-at-position r 0)
            (put-block-at-position r 1)
            (put-block-at-position r 2)
        )
    )
)

; Function that shows tower fall display
(defun display-fall ()
    ; Clear displays
    (clear-exp-window) 
    
    (build-reset-button)

    ; Tower fell label
    (add-text-to-exp-window :text "Tower fell!" :x (- *right_x* *left_x*) :y 100)

    ; Puts random blocks in a pile to look cool
    (loop for row from 0 to 54
        do (add-button-to-exp-window :x (random *window_width*) :y (- *bottom_y* (random 100)) :height *block_width* :width (+ *block_width* (* *block_width* (* (random 2) 3))) :text "" )
    )
)

; Resets the game
(defun reset ()
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
                    ; [ ] _ _ cause falling
                    (equal (aref *jenga_state* row 0) t)
                    (equal (aref *jenga_state* row 1) nil)
                    (equal (aref *jenga_state* row 2) nil)
                )
                (and
                    ; _ _ [ ] cause falling
                    (equal (aref *jenga_state* row 0) nil)
                    (equal (aref *jenga_state* row 1) nil)
                    (equal (aref *jenga_state* row 2) t)
                )
                (and 
                    ; _ _ _ cause falling
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
        (add-button-to-exp-window :x (if (equal (mod row 2) 0) *right_x* *left_x*) :y (- *bottom_y* (* row *block_width*)) :height *block_width* :width (* *block_width* 3) :text "" )
    )

    ; Adds the block and keep a reference of it in *jenga_buttons*
    (setf 
        (aref *jenga_buttons* row col) 
        (add-button-to-exp-window 
            :x      (+ (if (equal (mod row 2) 0) *left_x* *right_x*) (* *block_width* col)) 
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
    (let ((row (+ *top_row* 1)) (x (if (equal (mod (+ *top_row* 1) 2) 0) *left_x* *right_x*)))
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
            (setf *tower-fall* t)                
            (display-fall)
        )
        ; If it doesn't cause the tower to fall, put the block back on top of the tower
        (put-block-on-top)
    )
)

; Main function starting an experiment, can specify 'human to play yourself
(defun play-jenga (who)
    (build-display)
    (install-device *experiment-window*)    
    (reset)

    (if (eq who 'human)
        ; If it's a human playing, wait for human action
        (wait-for-human)
        ; If it's the model playing, runs the model for 60 seconds
        (progn
            (proc-display :clear t)
            (run 60 :real-time *visible*))
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

(sgp :v nil :esc t :egs 0.5 :show-focus t :trace-detail medium :ul t :ult t)

(chunk-type goal state)

(define-chunks 
    (start) (find-text)
    (goal isa goal state start))

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

(goal-focus goal)
)
