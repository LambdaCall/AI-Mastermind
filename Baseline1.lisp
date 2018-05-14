(defvar *p-list* nil)
(defvar *new-list* nil)


; returns all possible combinations without duplicates, but without all variations in each position
; code from, https://stackoverflow.com/questions/2087693/how-can-i-get-all-possible-permutations-of-a-list-with-common-lisp
(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
(all-permutations (append (rest lst) (list (first lst))) (rest remain))))))


;gets all possible variations of variables , 
;code found : https://rosettacode.org/wiki/Combinations_with_repetitions#Common_Lisp
(defun combinations (xs k)  
  (let ((x (car xs)))
    (cond
     ((null xs) nil)
     ((= k 1) (mapcar #'list xs))
     (t (append (mapcar (lambda (ys) (cons x ys))
            (combinations xs (1- k)))
        (combinations (cdr xs) k))))))

; permutations of permutation, creates a new list with all possible variations
(defun permofperm (plist)
(loop for i in plist
  do (loop for j in (all-permutations i)
    do(push j *new-list*))))  



;Exhaustively enumerates all possibilities, paying no attention to the system’s responses.
(defun baseline1 (board colors SCSA last-response)
        (declare (ignore SCSA))
        (cond (last-response nil)
          ((setf *p-list* (combinations colors board)); creates a list of combinations
          (permofperm *p-list*)
          (setf *new-list* (remove-duplicates *new-list* :test #'equal))))
      (pop *new-list*))