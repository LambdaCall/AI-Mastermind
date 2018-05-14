;Implementation of Rao's algorithms

;global list which is updated on every round
;it stores the possible possitions for each color
(defvar *inferences* nil)
(defvar *being-fixed* 0)
(defvar *being-considered* 0)
(defvar *positions* nil)
(defvar *color* 0)
(defvar *guess* nil)
(defvar *bool* nil)
(defvar *bool2* nil)

;set exact
;set almost
;if exact + almost == equal board, reset inferences
;call update with exact and almost
(defun CARS (board colors SCSA last-response)
  (declare (ignore SCSA))
  (cond ((= (length last-response) 0) (initialize-variables colors board) 
    (create-initial-guess board))
  (t (update-knowledge-base board colors (nth 0 last-response) (nth 1 last-response))
    (get-next-guess board)))
  *guess*)

;updates the inference list
(defun update-knowledge-base (board colors exact almost)
  (cond ((< (length *inferences*) board)
  (cond ((numberp *being-fixed*) (add-to-inferences (- (+ exact almost) (number-of-colors-tied-to-positions))))
    (t (add-to-inferences (- (- (+ exact almost) (number-of-colors-tied-to-positions)) 1))))))
  (cond 
    ((= 0 almost) (fix-being-fixed-in-position *being-fixed*) (bump-being-fixed))
    ((= 1 almost)
      (cond ((symbolp *being-fixed*) (delete-position *being-fixed* *being-considered*)))
        (delete-position *being-fixed* *being-fixed*))
    ((= 2 almost) (fix-color-i-in-color-j *being-considered* *being-fixed*))
    (t nil))

  (clean-up-inferences (length *inferences*))

  ;next color for being considered
  (cond ((< (length *inferences*) board)
  (cond ((<= *color* (length colors)) (setf *being-considered* (nth *color* colors))(setf *color* (+ *color* 1)))
    (t (setf *being-considered* 0))))))


(defun number-of-colors-tied-to-positions ()
  (cond ((> (length *inferences*) 0)
    (loop for item in (reverse *inferences*)
      sum (cond ((= 1 (length (first (rest item)))) 1)
                (t 0))))
  (t 0)))

;adds color to inference list n times in the form ((C) p1 p2 ...)
(defun add-to-inferences (n)
  (loop for i from 1 to n
    do (push (list *being-considered* *positions*) *inferences*)))

;color being-fixed has a fixed position
(defun fix-being-fixed-in-position (being-fixed)
  (setf *bool* t)
  (loop for i from 0 to (- (length *inferences*) 1)
    do (cond ((and (equal being-fixed (first (nth i (reverse *inferences*)))) (> (length (first (rest (nth i (reverse *inferences*))))) 1) *bool*)
      (setf (first (rest (nth i (reverse *inferences*)))) (list (first (first (rest (nth i (reverse *inferences*)))))))
      (clean-up-inferences (length *inferences*))
      (setf *bool* nil)))))

;bump being fixed to the next color in the list which needs fixing 
(defun bump-being-fixed ()
  (loop for i from 0 to (- (length *inferences*) 1)
    do (cond ((> (length (first (rest (nth i (reverse *inferences*))))) 1)
      (setf *being-fixed* (first (nth i (reverse *inferences*))))
      (return))
    (t (setf *being-fixed* 0)))))

;delete position of color i from color j
(defun delete-position (color-i color-j)
  (setf *bool* t)
  (loop for i from 0 to (- (length *inferences*) 1)
    do (cond ((and (equal color-i (first (nth i (reverse *inferences*)))) (> (length (first (rest (nth i (reverse *inferences*))))) 1) *bool*)
      (setf var (first (first (rest (nth i (reverse *inferences*))))))
        (loop for j from 0 to (- (length *inferences*) 1)
          do (cond ((and (equal color-j (first (nth j (reverse *inferences*)))) (> (length (first (rest (nth j (reverse *inferences*))))) 1))
            (setf (first (rest (nth j (reverse *inferences*)))) (remove var (first (rest (nth j (reverse *inferences*)))))))))
        (setf *bool* nil)))))

;fix position of color-j to color-i
(defun fix-color-i-in-color-j (color-i color-j)
  (loop for i from 0 to (- (length *inferences*) 1)
    do (setf *bool* t)
    do (cond ((and (equal color-j (first (nth i (reverse *inferences*)))) (> (length (first (rest (nth i (reverse *inferences*))))) 1) *bool*)
      (setf var (first (first (rest (nth i (reverse *inferences*))))))
      (setf *bool2* t)
        (loop for j from 0 to (- (length *inferences*) 1)
          do (cond ((and (equal color-i (first (nth j (reverse *inferences*)))) (> (length (first (rest (nth j (reverse *inferences*))))) 1) *bool2*)
            (setf (first (rest (nth j (reverse *inferences*)))) (list var))
          (setf *bool2* nil))))
    (clean-up-inferences (length *inferences*))
    (setf *bool* nil)))))

;clean-up the inferences list 
(defun clean-up-inferences (inference-length)
  (loop for item in (reverse *inferences*)
    do (cond ((= 1 (length (first (rest item))))
      (clean-up (first (first (rest item))) inference-length)))))

;clean-up inferences list by removing an tied position of color from other colors
(defun clean-up (var inference-length)
  (loop for i from 0 to (- inference-length 1)
    do (cond ((> (length (first (rest (nth i (reverse *inferences*))))) 1)
      (setf (first (rest (nth i (reverse *inferences*)))) (remove var (first (rest (nth i (reverse *inferences*))))))))))

;loops through board number and creates a list
;returns list which will be the next guess
(defun get-next-guess (board)
  (setf *guess* nil)
  (loop for i from 1 to board
    do (COND ((is-position-taken i) (push (return-taken-color i) *guess*))
      ((= i (next-possible-position-for-color *being-fixed*)) (push *being-fixed* *guess*))
      ((= (length *inferences*) board) (push (next-unfixed-color) *guess*))
      (t (push *being-considered* *guess*))))
    (setf *guess* (reverse *guess*)))

;loop through inference list
;if position p has a color, then return true
;otherwise, return false
(defun is-position-taken (position)
  (cond ((> (length *inferences*) 0)
    (loop for item in (reverse *inferences*)
      do (cond ((and (= (length (first (rest item))) 1) (= (first (first (rest item))) position))
        (return t)))))
  (t nil)))

;return the color that is associated with the position from the inferences list
(defun return-taken-color (position)
  (cond ((> (length *inferences*) 0)
    (loop for item in (reverse *inferences*)
      do (cond ((and (= (length (first (rest item))) 1) (= (first (first (rest item))) position))
        (return (first item))))))
  (t 0)))

;return the next possible position which might work for color
(defun next-possible-position-for-color (color)
  (cond ((> (length *inferences*) 0)
    (loop for i from 0 to (- (length *inferences*) 1)
      do (cond ((and (equal (first (nth i (reverse *inferences*))) color) (> (length (first (rest (nth i (reverse *inferences*))))) 1)) (return (first (first (rest (nth i (reverse *inferences*)))))))
        ((= i (- (length *inferences*) 1)) (setf *being-fixed* 0) (return 0)))))
  (t 0)))

;return the next color which might be in the secret code
(defun next-unfixed-color()
  (cond ((> (length *inferences*) 0)
    (loop for i from 0 to (- (length *inferences*) 1)
      do (cond ((> (length (first (rest (nth i (reverse *inferences*))))) 1) (return (first (nth i (reverse *inferences*))))))))
  (t 0)))

;initialize all necessary variables on the first call to CARS
(defun initialize-variables (colors board)
  (setf *inferences*  nil)
  (setq *being-fixed* 0)
  (setf *being-considered* 0)
  (setf *positions* nil)
  (setq *guess* nil)
  (setq *being-considered* (nth 0 colors))
  (setq *color* 1)
  (loop for i from 1 to board
    do (push i *positions*))
  (setf *positions* (reverse *positions*)))
 
(defun create-initial-guess (board)
  (loop for i from 1 to board
    do (push *being-considered* *guess*)))

;(play-tournament *mastermind* 'CARS 'insert-colors 25)

;CARS 
;(SCORE 42.305634) 
;(8.461126 5 0)


