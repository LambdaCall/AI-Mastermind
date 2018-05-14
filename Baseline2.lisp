(defvar *avoid-colors* nil)
(setf *p-list* nil)
(defvar *our-guess* nil)
(setf *position* 0)

(defun Baseline2 (board colors SCSA last-response)
	(declare (ignore SCSA last-response))
	(print "new guessing starts with last response")
	(print last-response)
	(COND
		((equal last-response nil)
			(setf *avoid-colors* nil)
			(setf *p-list* (k-permutations colors board))
			(setf *our-guess* (get-next-guess))
			(setf *position* 0)
			(print 'done))
		((= (+ (first last-response) (second last-response)) 0)
			(update-avoids)
			(setf *our-guess* (get-next-guess)))
		(T 
			(setf *our-guess* (get-next-guess))
			(print 'done2)))
	(print "our new guess is ")
	(print *our-guess*))

(defun get-next-guess ()
	(setf my-list (nth *position* *p-list*))
	(if (= 0 (length *avoid-colors*))
		(print my-list)
		(loop for i from (setf *position* (+ *position* 1)) to (length *p-list*) until (eval
			(cons 
				'and
				(loop for j in *avoid-colors* 
					collect 
					(cond
						((and (> (length (find j my-list)) 1) )
						((> (length (find j my-list)) 1) (print nil))
						))))
		; (loop for i from (setf *position* (+ *position* 1)) to (length *p-list*) until (eval
		; 	(not (equal nil (member
		; 		nil
		; 		(loop for j in *avoid-colors* 
		; 			collect (find j my-list))))))
		do (setf my-list (nth *position* *p-list*))
		))
	(print my-list))

(defun update-avoids ()
	(print 'SACIT)
	(remove-duplicates *our-guess*)
	(print *our-guess*)
	(loop for i from 0 to (- (length *our-guess*) 1)
		do (push (nth i *our-guess*) *avoid-colors*))
	(print *avoid-colors*))

(defun k-permutations (n-list k)
  (cond ((equal 0 k) (list nil))
        ((null n-list) nil)
        ((null (cdr n-list)) (list n-list))
        (t (loop for element in n-list
             append (mapcar (lambda (l) (cons element l))
                            (k-permutations (remove element n-list) (- k 1)))))))