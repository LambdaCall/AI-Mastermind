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

(defun genetic-algorithm (board colors SCSA last-response)
  (COND
    ((equal last-response nil)
      (push *TOGUESS* *guesses*) ;To scale we should loop through the pegs and insert colors 
      (setf *TOGUESS* '(A A A A))) ;OUR INITIAL GUESS
    ((not (equal last-response nil)) ;Condition such that this is not the first guess
      ;We get a result, and append it's result to it
      (push last-response (first (last *guesses*)))

      (setf eligibles (genetic_evolution MAX_POP_SIZE MAX_GENERATIONS scoref board)) ;Generate a list of new eligiable guesses. ;scoref is fitness function, needs implementation?
      (loop while (= (length eligibles) 0) ;If we somehow get a empty list, try to populate again using different parameters
        do (setf eligibles (genetic_evolution (* MAX_POP_SIZE 2) (/ MAX_GENERATIONS 2) scoref board))) ;What i said above

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

(defun genetic_evolution (a b c d)
  '(A B C D))
