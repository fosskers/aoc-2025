(in-package :aoc)

(defparameter *sample* '("L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"))

(defun rotation (offset)
  (funcall (p:ap (lambda (dir num)
                   (case dir
                     (:left (- num))
                     (:right num)))
                 (p:alt (p:<$ :left (p:char #\L))
                        (p:<$ :right (p:char #\R)))
                 #'p:unsigned)
           offset))

#+nil
(t:transduce (t:map (lambda (line) (p:parse #'rotation line)))
             #'t:cons *sample*)

;; --- Part 1 Solution --- ;;

#+nil
(t:transduce (t:comp (t:map (lambda (line) (p:parse #'rotation line)))
                     (t:scan (lambda (acc rot) (mod (+ acc rot) 100))
                             50)
                     (t:filter #'zerop))
             #'t:count #p"01-input.txt")

;; --- Part 2 Solution --- ;;

#+nil
(t:transduce (t:comp (t:map (lambda (line) (p:parse #'rotation line)))
                     (t:scan (lambda (acc rot)
                               (let* ((prev (car acc))
                                      (sum (+ prev rot))
                                      (pos (mod sum 100))
                                      (passed (cond
                                                ;; Edge case for when you land on 0 in a single move.
                                                ((zerop sum) 1)
                                                ;; Edge case for when you start from 0 and go left.
                                                ((zerop prev) (floor (/ (abs sum) 100)))
                                                ((< sum 0) (1+ (floor (/ (abs sum) 100))))
                                                (t (floor (/ sum 100))))))
                                 (cons pos passed)))
                             (cons 50 0))
                     (t:map #'cdr))
             #'+ #p"01-input.txt")

