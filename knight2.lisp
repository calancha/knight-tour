(defvar *board-size*)
(defvar *board*)

(defun in-range-and-empty?(ty tx)
  "Check if coordinates are within the board."
  (and (>= ty 0) (>= tx 0) (< ty *board-size*) (< tx *board-size*)
       (= 0 (aref *board* ty tx)))) ;;  and the square is empty

(defun print-board()
  (let* ((ncells (* *board-size* *board-size*))
         (scale (length (format nil "~d" ncells)))
         (str+ (string #\+))
         (str- (make-string scale :initial-element #\-))
         (line-fmt (format nil "|~~~dd" scale))
         (line-sep (format nil "~{~a~}~a~%"
                           (loop :repeat *board-size*
                                 :collect (concatenate 'string str+ str-))
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
               (if (= cc (* *board-size* *board-size*)) ; last empty square?
                   (progn (print-board) (return nil))) ; then print the board and return
               ;; otherwise find our empty neighbours for the recursion
               (let* ((jumps '((-2 1) (-1 2) (1 2) (2 1) (2 -1) (1 -2) (-1 -2) (-2 -1)))
                      (empty-neighbours
                       (loop :for jump :in jumps
                          :for ty := (+ yy (car jump)) :and tx := (+ xx (cadr jump))
                          :if (in-range-and-empty? ty tx) :collect `(,ty ,tx))))
                 ;; recurse using our neighbours, trying first the ones with the 
                 ;; least amount of free neighbours, i.e. the "loners"
                 (flet ((fn (c)
                          (loop :for j :in jumps
                             :sum (or (and (in-range-and-empty? (+ (car c) (car j))
                                                                (+ (cadr c) (cadr j)))
                                           1)
                                      0))))
                   (loop :for (ty tx) :in (sort empty-neighbours
                                                (lambda (c d) (<= (fn c) (fn d))))
                      :do (recur-fn ty tx (1+ cc)))
                   ;; All the neighbours failed, reset the square
                   (setf (aref *board* yy xx) 0) nil))))
      (recur-fn y x counter))))

  (let* ((*board-size* (if (cdr *posix-argv*) (parse-integer (cadr *posix-argv*)) 8))
         (*board* (make-array (list *board-size* *board-size*) :initial-element 0)))
    (and (fill-board 0 0 1) (princ "No solution found")))
