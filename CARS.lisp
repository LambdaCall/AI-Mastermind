(defvar *MAX_POP_SIZE* 100)
(defvar *MAX_GENERATIONS* 150)

(defvar *CROSSOVER_PROBABILITY* 0.5)
(defvar *CROSSOVER_THEN_MUTATION_PROBABILITY* 0.03)
(defvar *PERMUTATION_PROBABILITY* 0.03)
(defvar *INVERSION_PROBABILITY* 0.02)

(defvar *ELITE_RATIO* 0.4)

(defvar *WEIGHT_BLACK* 5) ; weight of well placed colors we give them a slightly better weight
(defvar *WEIGHT_WHITE* 3) ; weight of bad placed colors

(defvar *guesses* nil)
(defvar *last_eligibles* nil)


;Instead of passing same arguments to different functions (many times), we created 2 more global variables
(defvar *pegs* nil)
(defvar *all-colors* nil)
(defvar *trial* nil)


(defun genetic-algorithm (board colors SCSA last-response)
	(setf code nil)
	(COND
		((equal last-response nil)
			(setf *guesses* nil)
			(setf *pegs* board)
			(setf *all-colors* colors)
			(setf code '(A A C C))) ;OUR INITIAL GUESS
		((not (equal last-response nil)) ;Condition such that this is not the first guess
			;We get a result, and append it's result to it
			(setf *trial* (third last-response))
			(push last-response (first *guesses*))

			(setf eligibles (genetic_evolution *MAX_POP_SIZE* *MAX_GENERATIONS*)) ;Generate a list of new eligiable guesses. ;scoref is fitness function, needs implementation?
			(loop while (= (length eligibles) 0) ;If we somehow get a empty list, try to populate again using different parameters
				do (setf eligibles (genetic_evolution (* *MAX_POP_SIZE* 2) (/ *MAX_GENERATIONS* 2)))) ;What i said above

      		;check for duplicate guesses and remove from our eligibles lists
      		(setf code (first (pop eligibles))) ;Take the first item in the list
      		(setf i 0)
      		(loop while (< i (length *guesses*))  ;Loop through the list of guesses to check if the item we 'poped' is in the list
        		do (COND
        				((equal code (rest (nth i *guesses*)));Check if we have guessed the eligiable guess already
        					(setf code (first (pop eligibles))) (setf i 0))
        				(T (setf i (+ i 1))))))) ;Reset the counter

	(push code *guesses*)
	(print 'OURGUESS)
	(print code)
	(print (first (rest (rest last-response))))
 	code)


;Since this function is seperate function, We use global variable pegs to represent board size
(defun crossover (code1 code2)
	(setf newcode nil)
	(loop for i from 0 to (- *pegs* 1)
		do (COND 
			((> (/ (RANDOM 100) 100) *CROSSOVER_PROBABILITY*) (push (nth i code1) newcode ))
			(T (push (nth i code2) newcode))))
	newcode)

;90% this function is working proparly, we just need to make sure that variable 'v' is able to get all possible random numbers
(defun mutate (code)
	(setf i (RANDOM (- *pegs* 1)))
    (setf v (RANDOM (- (length *all-colors*) 1)))
    (setf (nth i code) (nth v *all-colors*))
    code)

;95% this function is working properly, we just need to check when to return 'code'
(defun permute (code)
	(loop for i from 0 to (- *pegs* 1)
		do (COND
			((<= (/ (RANDOM 100) 100) *PERMUTATION_PROBABILITY*)
				(setf position_a (+ 0 (RANDOM (- *pegs* 1))))
				(setf position_b (+ 0 (RANDOM (- *pegs* 1))))
				(setf color_a (nth position_a code))
				(setf (nth position_a code) (nth position_b code))
				(setf (nth position_b code) color_a ))))
	code)




(defun genetic_evolution (popsize generations)
;We generate the first population of chromosomes, in a randomized way in order to reduce probability of duplicates
	(setf population nil)
	(loop for j from 0 to (- popsize 1)
		do (setf person nil)
		do (loop for i from 0 to (- *pegs* 1)
			do (setf rcolor (nth (RANDOM (length *all-colors*)) *all-colors*))
			do (push rcolor person))
		do (push person population))


;Set of our favorite choices for the next play (Elite Group Ei)
	(setf chosen-ones nil)
	(setf h 1)
	(setf k 0)



	(loop while (and (<= (length chosen-ones) popsize) (<= h generations))
		do (setf sons nil)
		
		do (loop for i from 0 to (- (length population) 1)
			do (if (= i (- (length population) 1))
				(push (nth i population) sons))
			never (= i (- (length population) 1))

			do (setf son (crossover (nth i population) (nth (+ i 1) population)))
			do (COND
				((<= (/ (RANDOM 100) 100) *CROSSOVER_THEN_MUTATION_PROBABILITY*) (setf son (mutate son))))

			do (setf son (permute son))

			do (push son sons)) ;End of population loop


		do (setf pop_score nil)
		do (loop for c in sons
			;We should have one list that has a list and other stuff
			do (setf entry (fitness_score c))
			do (push (list entry c) pop_score))

		do (sort pop_score 'compare)

		do (setf eligibles nil)
		do (loop for i from 0 to (- (length pop_score) 1)
			do (COND
				((= (first (nth i pop_score)) 0) (push (nth i pop_score) eligibles)))) ;We need rest on the first paranthases

		do (COND
			((= (length eligibles) 0) (setf h (+ h 1)) CONTINUE))	;NEEDS CHECKING

		do (setf new_eligibles nil)
		do (loop for i from 0 to (- (length eligibles) 1)
			do (push (rest (nth i eligibles)) new_eligibles))
		do (setf eligibles new_eligibles)

		do (loop for i from 0 to (- (length eligibles) 1)
			do (COND
          	;CHECK IF ELIGIBLE OF I IS IN THE CHOSEN LIST, IF SO REMOVE IT FROM THE CHOSEN SET AND THE ONE WE REMOVED FROM CHOSEN ONE WITH A NEW RANDOM "PERSON"
          		((find (nth i eligibles) chosen-ones)
          			(remove (nth i eligibles) chosen-ones)
          			(setf person nil)
          			(loop for j from 0 to (- *pegs* 1)
          				do (setf rcolor (nth (RANDOM (- (length *all-colors*) 1)) *all-colors*))
          				do (push rcolor person))
          			(push person chosen-ones))))

		do (loop for eligible in eligibles until (= (length chosen-ones) popsize)
        	do (COND
        		((not (find eligible chosen-ones)) (push eligible chosen-ones))))

		do (setf population nil)
		do (setf population eligibles)
		do (setf j (length eligibles))

		do (loop while (< j popsize)
			do (setf person nil)
			do (loop for i from 0 to (- *pegs* 1)
				do (setf rcolor (nth (RANDOM (- (length *all-colors*) 1)) *all-colors*))
				do (push rcolor person))
          	do (push person population)
        	do (setf j (+ j 1)))
		do (setf h (+ h 1))
		return chosen-ones))

(defun compare (a b)
	(> (first a) (first b)))

;90% this function works, only problem is with trial_result, needs cheking
(defun get_difference (guess)
	(setf guess_result (first guess))
	(setf guess_code (rest guess))
	(setf trial_result (process-guess *mastermind* guess_code))
	(setf dif '(0 0))

	(setf (first dif) (abs (- (first trial_result) (first guess_result))))
	(setf (second dif) (abs (- (second trial_result) (second guess_result))))
	dif)

;This function is 99% done, just run time check is needed
;code candidate guess we are trying to have its fitness score
(defun fitness_score (code)
	(setf differences nil)
	(loop for i from 0 to (- (length *guesses*) 1)
      do (push (get_difference (nth i *guesses*)) differences))

    (setf black 0)
    (setf white 0)
    (loop for i from 0 to (- (length differences) 1)
      do (setf black (+ black (first (nth i differences))))
      do (setf white (+ white (second (nth i differences)))))
    (setf score (+ black white)))