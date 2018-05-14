(defvar *p-list* nil)
(defvar *new-list* nil)

(defun baseline1 (board colors SCSA last-response)
        (declare (ignore SCSA last-response))
        (print(setf *p-list* (combinations colors board))); creates a list of combinations
        (permofperm *p-list*)

        (pop *new-list*)
    )

(defun all-permutations (lst &optional (remain lst)); returns all possible combinations without duplicates
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
(all-permutations (append (rest lst) (list (first lst))) (rest remain))))))

(defun combinations (xs k) ; gets all possible variations of variables
  (let ((x (car xs)))
    (cond
     ((null xs) nil)
     ((= k 1) (mapcar #'list xs))
     (t (append (mapcar (lambda (ys) (cons x ys))
            (combinations xs (1- k)))
        (combinations (cdr xs) k))))))


(defun permofperm (plist); permutations of permutation
(loop for i in plist
  do (loop for j in (all-permutations i)
    do (push j *new-list*))))
