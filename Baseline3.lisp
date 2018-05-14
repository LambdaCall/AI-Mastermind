(defvar *valid-colors* nil) ;contains a list of valid colors
(defvar *list-of-colors* nil) ;contains a list on n-1 colors
(defvar *our-guess* nil) ;current guess
(defvar *current-color* nil);current color which is being tested
(defvar *position* -2) ;index of p-list set to -2 intially
(defvar *p-list* nil); p-list of permutations

;initalizes all necessary global variables
(defun init (colors2)
	(setf *list-of-colors* (reverse (rest (reverse colors2))))
	(setf *position* -2)
	(setf *p-list* nil)
	(setf *valid-colors* nil)
	(setf *our-guess* nil)
	(setf *current-color* nil))

;This method creates monochromatic guess according to board size and deletes the color from list-of-colors
(defun create-guess (board)
	(cond ((> (length *list-of-colors*) 0)
		(loop for i from 1 to board
			do (setf *current-color* (first *list-of-colors*))
			collect (first *list-of-colors*) into my-list
			do (setf *our-guess* my-list))
	(setf *list-of-colors* (rest *list-of-colors*))))
	*our-guess*)

;This method returns the next untested guess stored in p-list
(defun generate-guesses ()
	(setf *position* (+ *position* 1))
	(nth *position* *p-list*))

;This method was taken from: https://stackoverflow.com/questions/2087693/how-can-i-get-all-possible-permutations-of-a-list-with-common-lisp?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
;It creates all possible permutations of valid-colors and stores the result into p-list
;It method allows for duplicate elements
(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))

(defun BASELINE3 (board colors SCSA last-response)
	(declare (ignore SCSA))
	;if last-response is nil, this is the first call to this function. In such case, call init which will initialize all necessary variables.
	(if (= (length last-response) 0)
		(init colors)
	;otherwise
	;if number of exact pegs is at least one,push as many colors as exact pegs into valid-colors
	(cond ((>= (nth 0 last-response) 1)
		(loop for i from 1 to (nth 0 last-response)
			do (push *current-color* *valid-colors*)
			do (setf *valid-colors* (reverse *valid-colors*))))))
	;if valid-colors is less than board size and list-of-colors is empty fill the rest of valid colors with the last color in the colors list and create permutations to generate next guess.
	;else if, valid-colors is less than board, create the next monochromatic guess
	;else if, position of p-list is -2, generate perumations
	;else, return a guess from p-list
	(cond
		((and (< (length *valid-colors*) board) (= (length *list-of-colors*) 0))
			(loop for i from 1 to (- board (length *valid-colors*))
				do (push (nth (- (length colors) 1) colors) *valid-colors*))
			(setf *p-list* (all-permutations *valid-colors*))
			(setf *position* -1)
			(generate-guesses))
		((< (length *valid-colors*) board) (create-guess board))
		((= *position* -2) (setf *p-list* (all-permutations *valid-colors*)) (setf *position* -1) (generate-guesses))
		(T (generate-guesses))))