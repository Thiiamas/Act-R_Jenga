; Represents if the tower fell
(defvar *tower-fell* nil)               

; Display is visible or not
(defvar *visible* t)                    

; The internal state of the jenga game, for each row, contains an array of three blocks and their state (t: block present, nil: block removed)
; ex: ((T T T) (T NIL T) (NIL T NIL) (T T T) ...)
(defvar *jenga_state* nil)             

; The number of total rows
(defvar *nb_rows* 18)

; The index of the top row
(defvar *top_row* (- *nb_rows* 1))

; Function that builds the display
(defun build-display ()
    (setf *experiment-window* (open-exp-window "Jenga"
                                             :visible *visible*
                                             :width 800
                                             :height 600))
  
    (allow-event-manager *experiment-window*)
)

; Clear the display and displays the jenga game
(defun display-jenga () 
    (setq *jenga_blocks_side_button* (make-array 18))

    (clear-exp-window) 

    ; Reset button
    (add-button-to-exp-window :x 350 :y 20 :height 35 :width 50 :text "RESET" :action (lambda (button) (reset)))

    ; Side labels
    (add-text-to-exp-window :text "Side 1" :x 310 :y 140)
    (add-text-to-exp-window :text "Side 2" :x 410 :y 140)

    ; For each rows, add the buttons reprensenting blocks
    (loop for row from 0 to 17
        do (let ((r row) (x (if (equal (mod row 2) 0) 300 400))) 
            ; Side of the block on opposite side
            (add-button-to-exp-window :x (if (equal (mod r 2) 0) 400 300) :y (- 500 (* r 20)) :height 20 :width 60 :text "" )

            ; The three blocks we can remove
            (add-button-to-exp-window :x x :y (- 500 (* r 20)) :height 20 :width 20 :text "" :color (if (equal r 17) 'gray 'blue) :action (if (equal r 17) nil (lambda (button) (block-removed button r 0))))  
            (add-button-to-exp-window :x (+ x 20) :y (- 500 (* r 20)) :height 20 :width 20 :text "" :color (if (equal r 17) 'gray 'blue) :action (if (equal r 17) nil (lambda (button) (block-removed button r 1))))  
            (add-button-to-exp-window :x (+ x 40) :y (- 500 (* r 20)) :height 20 :width 20 :text "" :color (if (equal r 17) 'gray 'blue) :action (if (equal r 17) nil (lambda (button) (block-removed button r 2))))         
        )
    )
)

; Function that shows tower fall display
(defun display-fall ()
    (setf *experiment-window* (open-exp-window "Jenga"
                                             :visible *visible*
                                              :width 800
                                             :height 600))
    ; Clear displays
    (clear-exp-window) 
    
    ; Reset button
    (add-button-to-exp-window :x 350 :y 20 :height 35 :width 50 :text "RESET" :action (lambda (button) (reset)))

    ; Tower fell label
    (add-text-to-exp-window :text "Tower fell!" :x 350 :y 200)

    ; Puts random blocks in a pile to look cool
    (loop for row from 0 to 54
        do (add-button-to-exp-window :x (+ 200 (random 300)) :y (+ 450 (random 100)) :height 20 :width (+ 20 (* 20 (* (random 2) 3))) :text "" )
    )
          
)

; Resets the game
(defun reset ()
    (setf *tower-fell* nil)
    (setq *jenga_state* (make-array '(18 3) :initial-element t))
    (setf *top_row* (- *nb_rows* 1))

    (display-jenga)
)

; Check if the row index in parameters is a good row (does not cause tower to fall)
(defun check-row-ok (row) 
    (if (and (not (equal row *top_row*)) 
                (or (and 
                    (equal (aref *jenga_state* row 0) t)
                    (equal (aref *jenga_state* row 1) nil)
                    (equal (aref *jenga_state* row 2) nil)
                )
                (and 
                    (equal (aref *jenga_state* row 0) nil)
                    (equal (aref *jenga_state* row 1) nil)
                    (equal (aref *jenga_state* row 2) t)
                )
                (and 
                    (equal (aref *jenga_state* row 0) nil)
                    (equal (aref *jenga_state* row 1) nil)
                    (equal (aref *jenga_state* row 2) nil)
                ))
        )
        nil ; Return not okay
        t   ; Return okay
    )   
)

; Event happening when the user or model clicks on a block to remove it
(defun block-removed (button row col)

    ; Remove the button (block) from the display
    (remove-items-from-exp-window button)

    ; Updates the internal state of jenga (set nil for the block)
    (setf (aref *jenga_state* row col) nil)

    ; Test if the new state of the row causes tower to fall
    (if (not (check-row-ok row))
        ; If causes tower to fall, display the "Tower fell!" screen
        (progn
            (setf *tower-fall* t)                
            (display-fall)
        )
    )
)

; Main function starting an experiment, can specify 'human to play yourself
(defun play-jenga (who)
    (build-display)
    (install-device *experiment-window*)    
    (reset)

    (if (eq who 'human)
        (progn (print *jenga_state*)
        (wait-for-human)
    )
    (progn
        (proc-display :clear t)
        (run 60 :real-time *visible*)))
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
