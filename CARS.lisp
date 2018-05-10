;
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

;Instead of passing same arguments to different functions (many times), we created 2 more global variables
(defvar *pegs* nil)
(defvar *all-colors* nil)


(defun random-range-for () ;Generates two random numbers such that the first is smaller than the second
  (let
    (
      (index1 (random *pegs*))
      (index2 (random *pegs*)))
    (loop while (= index1 index2)
    	do (setf index1 (random *pegs*)))
    (if (< index1 index2)
        (list index1 index2)
        (list index2 index1))))

(defun crossover (code1 code2) ;Our cross over function
	(setf newcode nil) ;Variable to hold our return guess
	(setf crosses (random-range-for)) 
	(COND 
		((< *CROSSOVER_PROBABILITY* (random 1.0)) ;Check if we should do 1 or 2 crossover
			(setf newcode (append 
				(subseq code1 0 (first crosses))
				(subseq code2 (first crosses) (second crosses))
				(subseq code1 (second crosses) *pegs*))))
		(T
			(setf newcode (append
			(subseq code1 0 (first crosses))
			(subseq code2 (first crosses) *pegs*)))))
	newcode)

(defun mutate (code) ;Randomly mutate a guess
	(COND
		((<= (random 1.0) *CROSSOVER_THEN_MUTATION_PROBABILITY*) ;Check if we should mutate
			(setf i (RANDOM *pegs*)) ;Pick a random position
			(setf v (RANDOM-CHOOSER *all-colors*)) ;Pick a random color
			(setf (nth i code) v))) ;Assign the random position with the random color
    code) ;Return the code

(defun permutate (code) ;Generates a permutation of the code
	(COND
		((<= (Random 1.0) *PERMUTATION_PROBABILITY*) ;Chance to permutate
			(rotatef (nth (random *pegs*) code) (nth (random *pegs*) code)))) ;Swap two positions
	code) ;Give code

(defun compare (a b) ;Sorts by looking at fitness score 
	(> (first a) (first b)))

;Modified version of your (color-counter) function
(defun my-color-counter (colors list)
  (loop with tally = (make-array (length colors) :initial-element 0)
     for peg in list
     for index = (spot peg)
     do (incf (aref tally index))
     finally (return tally)))

;Modified version of your function (process-guess)
(defun my-process-guess (answer guess)
  (loop
     with guess-color-count = (my-color-counter *all-colors* guess)
     with true-color-count = (my-color-counter *all-colors* answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot entry)))
     and do (decf (aref true-color-count (spot entry)))
     finally
      (return
        (list
          exact-counter
          (loop
            for i from 0 to (1- (length *all-colors*))
            for guessed = (aref true-color-count i)
            for true = (aref guess-color-count i)
            when (<= true guessed)
            sum true
            else sum guessed)))))

;Helper function to fitness_score to calculate fitness
;Goes through the previous guesses we already did
(defun get_difference (answer guess)
	(setf guess_result (my-process-guess (rest answer) guess))
	(+
		(abs (- (first (first answer)) (first guess_result)))
		(abs (- (second (first answer)) (second guess_result)))))

;Calculates the fitness score of the candidate guess 'code'
(defun fitness_score (code)
	(setf differences nil)
	(loop for i from 0 to (- (length *guesses*) 1)
		sum (get_difference (nth i *guesses*) code)))


(defun genetic_evolution (popsize generations) ;Generate a population 
;We generate the first population of chromosomes, in a randomized way in order to reduce probability of duplicates
	(setf population nil) ;A list to hold our generated population
	(loop for j from 0 to (- popsize 1) ;Generate popsize-1 random "guesses" and stores them into populatiobn
		do (setf person nil)
		do (loop for i from 0 to (- *pegs* 1)
			do (setf rcolor (RANDOM-CHOOSER *all-colors*))
			do (push rcolor person))
		do (push person population))


;Set of our favorite choices for the next play (Elite Group Ei)
	(setf chosen-ones nil)
	(setf h 1)
	(setf k 0)



	(loop while (and (<= (length chosen-ones) popsize) (<= h generations)) ;Keep looping until we get max popsize or hit the generation limit
		do (setf sons nil)  ;The list that will hold all the "sons"/dererived guesses from our initail/parent guess
		
		do (loop for i from 0 to (- (length population) 1)  ;Simply take the last element in the list and append it into sons
			do (if (= i (- (length population) 1))
				(push (nth i population) sons))
			never (= i (- (length population) 1))

			do (setf son (crossover (nth i population) (nth (+ i 1) population))) ;Run crossover with the guess
			do (COND
				((<= (random 1.0) *CROSSOVER_THEN_MUTATION_PROBABILITY*) (setf son (mutate son)))) ;Run mutation with the guess

			do (setf son (permutate son)) ;Apply permutation

			do (push son sons)) ;Add the son/derived guess to the list
			;End of population loop


		do (setf pop_score nil) ;A list of population scores 
		do (loop for c in sons
			;We should have one list that has a list and other stuff
			do (setf entry (fitness_score c))  ;Find the fitness of current guess (wherever we are at in sons)
			do (push (list entry c) pop_score)) ;Add to the pop_score

		do (setf pop_score (sort pop_score 'compare)) ;sort the list so that when we pop we get the best guess to guess

		do (setf eligibles nil) 
		do (loop for i from 0 to (- (length pop_score) 1) ;If we get a fitness score of zero push it into eligibles
			do (COND
				((= (first (nth i pop_score)) 0) (push (nth i pop_score) eligibles)))) ;We need rest on the first paranthases

		do (COND
			((= (length eligibles) 0) (setf h (+ h 1)) (CONTINUE)))	;if the list (eligibles is empty) just leave and go to the next generation

		do (setf new_eligibles nil) 
		do (loop for i from 0 to (- (length eligibles) 1) ;Copy the eligible list into new_eligibles
			do (push (rest (nth i eligibles)) new_eligibles))
		do (setf eligibles new_eligibles)

		do (loop for i from 0 to (- (length eligibles) 1) ;Go through the eligibles items and see if we can pick it 
			do (COND
          	;CHECK IF ELIGIBLE OF I IS IN THE CHOSEN LIST, IF SO REMOVE IT FROM THE CHOSEN SET AND THE ONE WE REMOVED FROM CHOSEN ONE WITH A NEW RANDOM "PERSON"
          		((find (nth i eligibles) chosen-ones)
          			(remove (nth i eligibles) chosen-ones)
          			(setf person nil)
          			(loop for j from 0 to (- *pegs* 1) 
          				do (setf rcolor (RANDOM-CHOOSER *all-colors*))
          				do (push rcolor person))
          			(push person chosen-ones))))

		do (loop for eligible in eligibles until (= (length chosen-ones) popsize) ;If the element isn't in our chosen list add it to our chosen list
        	do (COND
        		((not (find eligible chosen-ones)) (push eligible chosen-ones))))


		do (setf population nil)
		do (setf population eligibles)
		do (setf j (length eligibles))


		;Keep adding random guesses to the population till we hit the cap
		do (loop while (< j popsize)
			do (setf person nil)
			do (loop for i from 0 to (- *pegs* 1)
				do (setf rcolor (RANDOM-CHOOSER *all-colors*))
				do (push rcolor person))
          	do (push person population)
        	do (setf j (+ j 1)))
		do (setf h (+ h 1)) ;Increase generation count
		return chosen-ones))

(defun CARS (board colors SCSA last-response) ;Our Player
	(declare (ignore SCSA)) 
	(setq code nil) ;Code is going to the variable that we guess
	(COND
		((equal last-response nil) ;Check if this is our initial guess, if so we set values to global 
			(setf *guesses* nil)
			(setf *pegs* board)
			(setf *all-colors* colors)
			(setq code (choose-n-random board colors))) 
		((not (equal last-response nil)) ;Condition such that this is not the first guess
			;We get a result, and append it's result to it
			(push last-response (first *guesses*))

			(setf eligibles (genetic_evolution *MAX_POP_SIZE* *MAX_GENERATIONS*)) ;Generate a list of new eligiable guesses. ;scoref is fitness function, needs implementation?
			(loop while (= (length eligibles) 0) ;If we somehow get a empty list, try to populate again using different parameters
				do (setf eligibles (genetic_evolution (* *MAX_POP_SIZE* 2) (/ *MAX_GENERATIONS* 2)))) ;What i said above

      		;check for duplicate guesses and remove from our eligibles lists
      		(setq code (first (pop eligibles))) ;Take the first item in the list
      		(setf i 0)
      		(loop while (< i (length *guesses*))  ;Loop through the list of guesses to check if the item we 'poped' is in the list
        		do (COND
        				((equal code (rest (nth i *guesses*)));Check if we have guessed the eligiable guess already
        					(setq code (first (pop eligibles))) (setf i 0))
        				(T (setf i (+ i 1))))))) ;Reset the counter

	(push code *guesses*) ;Add the guess we're going to submit into our global guess list
 	code ;Return our guess
 	)
