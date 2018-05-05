(defvar *TOGUESS* '(A A A A))

(defvar *MAX_POP_SIZE* 20)
(defvar *MAX_GENERATIONS* 50)

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


(defun genetic-algorithm (board colors SCSA last-response)
  (COND
    ((equal last-response nil)
    	(setf *pegs* board)
    	(setf *all-colors* colors)
      (push *TOGUESS* *guesses*) ;To scale we should loop through the pegs and insert colors 
      (setf *TOGUESS* '(A A A A))) ;OUR INITIAL GUESS
    ((not (equal last-response nil)) ;Condition such that this is not the first guess
      ;We get a result, and append it's result to it
      (push last-response (first (last *guesses*)))

      (setf eligibles (genetic_evolution MAX_POP_SIZE MAX_GENERATIONS)) ;Generate a list of new eligiable guesses. ;scoref is fitness function, needs implementation?
      (loop while (= (length eligibles) 0) ;If we somehow get a empty list, try to populate again using different parameters
        do (setf eligibles (genetic_evolution (* MAX_POP_SIZE 2) (/ MAX_GENERATIONS 2)))) ;What i said above

      ;check for duplicate guesses and remove from our eligibles lists
      (setf code (pop eligibles)) ;Take the first item in the list
      (setf i 0)
      (loop while (< i (length *guesses*))  ;Loop through the list of guesses to check if the item we 'poped' is in the list
        do (COND
        ((equal code (nth i *guesses*));Check if we have guessed the eligiable guess already
                (t (setf code (pop eligibles)) ;Pop again
                (setf i 1))))) ;Reset the counter
      ))

  (push code *guesses*)
  (print code))


;Since this function is seperate function, We use global variable pegs to represent board size
(defun crossover (code1, code2)
  (setf newcode nil)
  (loop for i from 1 to *pegs*
    (COND 
      ((> (RANDOM 100) *CROSSOVER_PROBABILITY*) (push newcode (nth i code1)))
      (T ((push newcode (nth i code2))))))
  (newcode))

;90% this function is working proparly, we just need to make sure that variable 'v' is able to get all possible random numbers
(defun mutate (code)
	(setf i (RANDOM (- *pegs* 1)))
    (setf v (RANDOM (- (length *all-colors*) 1)))
    (setf (nth i code) (nth v *all-colors*))
    (code))

;95% this function is working properly, we just need to check when to return 'code'
(defun permute (code)
	(loop for i from 0 to (- *pegs* 1)
		(COND
			((<= (RANDOM 100) *PERMUTATION_PROBABILITY*)
				(setf position_a (RANDOM (- *pegs* 1)))
				(setf position_b (RANDOM (- *pegs* 1)))
				(setf color_a (nth position_a code))
				(setf (nth position_a code) (nth position_b code))
				(setf (nth position_b code) color_a ))))
	(code))




(defun genetic_evolution (popsize, generations, costfitness)
;REPLACE *MAX_POP WITH WHATEVER A IS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;We generate the first population of chromosomes, in a randomized way in order to reduce probability of duplicates
	(setf population nil)
	(loop for j from 0 to (- popsize 1)
		(setf person nil)
		(loop for i from 0 to (- (length *pegs*) 1)
			(setf rcolor (nth (RANDOM (length *all-colors*)) *all-colors*))
			(setf (nth i person) rcolor))
		(setf (nth j population) person))



;Set of our favorite choices for the next play (Elite Group Ei)
	(setf chosen-ones nil)
	(setf h 1)
	(setf k 0)

	(loop while (and (<= (length chosen-ones) popsize) (<= h generations))
		(setf sons nil)
		
		(loop for i from 0 to (- (length population) 1)
			do (if (= i (- (length population) 1))
				(push (nth i population) sons))
			never (= i (- (length population) 1))

			(setf son (crossover (nth i population) (nth (+ i 1) population)))
			(COND
				((<= (RANDOM 100) *CROSSOVER_THEN_MUTATION_PROBABILITY*) (setf son (mutate son))))

			(setf son (permute son))

			(push son sons)) ;End of population loop



		(setf pop_score nil)
		(loop for c from 0 to (- (length sons) 1)
			;We should have one list that has a list and other stuff
			(setf entry (costfitness c))
			(push c (first entry))
			(push entry pop_score))

		(sort pop-score 'compare)



		(setf eligible nil)
		(loop for i from 0 to (- (length pop_score) 1)
			(COND
				(= (first (nth i pop_score) 0))
				(push eligible (nth i pop_score))))

		(COND 
          (= (length eligible) 0)
          (setf h (+ h 1))
          ;CONTINUE
          )

      (setf new_eligibles nil)
      (loop for i from 0 to (length eligible)
        (push new_eligibles (rest (nth i eligible)))
      )

      (setf eligibles new_eligibles)


      (loop for i from 0 to (length eligible)
        (COND ;CHECK
          ;CHECK IF ELIGIBLE OF I IS IN THE CHOSEN LIST, IF SO REMOVE IT FROM THE CHOSEN SET AND THE ONE WE REMOVED FROM CHOSEN ONE WITH A NEW RANDOM "PERSON"
          ( (nth i eligible) ) ;check/ NOT DONE
        )
        )

      (loop for e in (length eligibles)
        (COND ;CHECK
            (= (length chosen) a)
            ;BREAK
          )

        ;if not eligible in chosen_ones:
            ;            chosen_ones.append(eligible)
        ;TODO:
        ;Check if nth e eligible is not in the list chosen, append it to the chosen list
        )

      (setf population nil)
      ;CHECK
      (setf population eligibles)

      (setf j (length eligibles))

      (loop while (< j popsize) ;a
        ;Append to population a random "person"
        (setf j (+ j 1))
        )

      (setf h (+ h 1))

      (chosen)
  ))

(defun compare (a b)
       (> (first (first a)) (first (first b))))

;90% this function works, only problem is with trial_result, needs cheking
(defun get_difference (trial, guess)
	(setf guess_result (first guess))
	(setf guess_code (nth 0 guess))
	(setf trial_result (process-guess *mastermind* guess))
	(setf dif '(0,0))

	(setf (first dif) (abs (- (first trial_result) (first guess_result))))
	(setf (last dif) (abs (- (second trial_result) (second guess_result))))
	(dif))

;This function is 99% done, just run time check is needed
;trial is the guess index
;code candidate guess we are trying to have its fitness score
(defun fitness_score (trial, code)
	(setf differences nil)
	(loop for i from 0 to (- (length guesses) 1)
      (push differences (get_difference trial (nth i *guesses*))))

    (setf black 0)
    (setf white 0)

    (loop for i from 0 to (- (length differences) 1)
      (setf black (+ black (nth i differences)))
      (setf white (+ white (nth i differences))))

    (setf score (+ black white)))