(defun handleRotationsLargerThanTheNumberOfElements (elements rotationCount)
  (if  (> rotationCount (length elements))
      (% rotationCount (length elements))
    ; else, return the original value
    rotationCount
    )
  )

(defun handleNegativeRotations (elements rotationCount)
  (if (< rotationCount 0)
      (+ (length elements) rotationCount) ; -1 rotations now means length - 1 left rotations
    ; else we don't need to change the rotation
    rotationCount
    )
  )

(defun handleEdgeCases (elements rotationCount)
  (handleNegativeRotations elements
			   (handleRotationsLargerThanTheNumberOfElements elements rotationCount)
			   )
  )

(defun rotateOnce (elements)
  (append (cdr elements) (list (car elements)))
  )

(defun rotateList (elements rotationCount)
  "Perform left rotations on the list of elements"
  ; base case
  (let ((numberOfLeftRotations (handleEdgeCases elements rotationCount)))
  (if (= numberOfLeftRotations 0) elements
    (rotateList (rotateOnce elements) (- numberOfLeftRotations 1))
    )
  ))

; 5 / 3 = 2
(handleRotationsLargerThanTheNumberOfElements (list 1 2 3) 5)
; -1 is length of elements - 1 rotations
(handleNegativeRotations (list 1 2 3) -1)
(handleEdgeCases (list 1 2 3 4) 1)
(rotateOnce (rotateOnce (list 1 2 3)))
(rotateList (list 1 2 3) 1)
