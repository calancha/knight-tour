(defvar *board-size*)
(defvar *board*)

(defun in-range-and-empty?(ty tx)
  "Check if coordinates are within the board."
  (and (>= ty 0) (>= tx 0) (< ty *board-size*) (< tx *board-size*)
       (= 0 (aref *board* ty tx))))

(defun print-board()
  (let* ((ncells (* *board-size* *board-size*))
         (scale (length (format nil "~d" ncells)))
         (str+ (string #\+))
         (str- (make-string scale :initial-element #\-))
         (line-fmt (format nil "|~~~dd" scale))
         (line-sep
          (format nil "~{~a~}~a~%"
                  (loop :repeat *board-size* :collect (concatenate 'string str+ str-)) 
                  str+)))
    (princ line-sep)
    (loop :for i :from 0 :below ncells :with idx := 0
          :for elem := (row-major-aref *board* i)
          :do (format t line-fmt elem)
              (incf idx)
              :if (zerop (mod idx *board-size*)) :do
                (setq idx 0)
                (format t "|~%~a" line-sep))))

(defun fill-board (y x counter)
  "Fill recursively the board."
  (block nil
    (labels ((recur-fn (yy xx cc)
               (assert (= 0 (aref *board* yy xx)))
               (setf (aref *board* yy xx) cc) ; fill the square
               (if (= cc (* *board-size* *board-size*)) ; was this the last empty square?
                   (progn (print-board) (return nil))) ; yes, print the board and exit
               (let ((jumps '((-2 1) (-1 2) (1 2) (2 1) (2 -1) (1 -2) (-1 -2) (-2 -1))))
                 (loop :for jump :in jumps
                    :for ty := (+ yy (car jump)) :and tx := (+ xx (cadr jump))
                    :if (in-range-and-empty? ty tx)
                    :do (recur-fn ty tx (1+ cc)))
                 (setf (aref *board* yy xx) 0))))
      (recur-fn y x counter))))

(let* ((*board-size* (if (cdr *posix-argv*) (parse-integer (cadr *posix-argv*)) 8))
       (*board* (make-array (list *board-size* *board-size*) :initial-element 0)))
  (and (fill-board 0 0 1) (princ "No solution found")))
