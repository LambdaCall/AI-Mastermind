(defvar *avoid-colors* nil) ;Stores list of colors to be avoided
(defvar *p-list* nil) ;Holds list of all possible quesses which are generated through permutation
(defvar *our-guess* nil) ;our guess stored at global variable
(defvar *position* 1) ;our position while we are parsing through permutations

;This functions gives us our next guess
(defun get-next-guess ()
	(let 
		((newcode (nth *position* *p-list*))
		(i 0))
		(incf *position*)
		(loop while (< i (length *avoid-colors*))
			do (COND
				((member (nth i *avoid-colors*) newcode)
					(setq newcode (nth *position* *p-list*))
					(incf *position*)
					(setq i 0))
				(T (incf i))))
		newcode))

;This function updates colors which we should be avoiding
(defun update-avoids ()
	(setf *our-guess* (remove-duplicates *our-guess*))
	(loop for i from 0 to (- (length *our-guess*) 1)
		do (push (nth i *our-guess*) *avoid-colors*)))

;Found this permutation function at https://gist.github.com/capablemonkey/f824aeed72efe007078abb235ac8d22a
;But modified in a way that it creates all possible k lenght permutations
(defun k-permutations (n-list k)
	(cond ((equal 0 k) (list nil))
		((null n-list) nil)
		((null (cdr n-list)) (list n-list))
		(t (loop for element in n-list
			append (mapcar (lambda (l) (cons element l))
				(k-permutations n-list (- k 1)))))))

;BASELINE 2 PLAYER
(defun Baseline2 (board colors SCSA last-response)
	(declare (ignore SCSA))
	(COND
		((equal last-response nil)
			(setf *avoid-colors* nil)
			(setf *position* 1)
			(setf *p-list* (k-permutations colors board))
			(setf *our-guess* (first *p-list*)))
		((= (+ (first last-response) (second last-response)) 0)
			(update-avoids)
			(setf *our-guess* (get-next-guess)))
		(T (setf *our-guess* (get-next-guess))))
	*our-guess*)