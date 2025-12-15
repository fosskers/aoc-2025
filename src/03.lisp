(in-package :aoc)

(defparameter *sample* '("987654321111111" "811111111111119" "234234234234278" "818181911112111"))

(defun char->int (c)
  (- (char-code c) 48))

;; --- Part 1 Solution --- ;;

#+nil
(t:transduce
 (t:comp (t:map (lambda (line) (t:transduce (t:map #'char->int) #'t:cons line)))
         (t:map (lambda (ns)
                  (let ((1st  (nth 0 ns))
                        (2nd  (nth 1 ns))
                        (rest (cddr ns)))
                    (t:transduce #'t:pass
                                 (t:fold (lambda (acc n)
                                           (destructuring-bind (high . low) acc
                                             (let ((new (max (+ (* high 10) low)
                                                             (+ (* high 10) n)
                                                             (+ (* low  10) n))))
                                               (cons (floor (/ new 10)) (mod new 10)))))
                                         (cons 1st 2nd))
                                 rest))))
         (t:map (lambda (pair) (+ (* 10 (car pair)) (cdr pair)))))
 #'+
 #p"03-input.txt")

;; --- Part 2 Solution --- ;;

(defun vector->int (v)
  "Fold a vector of individual ints into the whole number it would be."
  (t:transduce #'t:pass (t:fold (lambda (acc n) (+ (* 10 acc) n))) v))

#+nil
(vector->int #(1 2 3))

(defun index-of-lowest (v)
  "Find the index of the left-most lowest number in a vector."
  (->> v
       (t:transduce #'t:enumerate
                    (t:fold (lambda (acc pair)
                              (cond ((< (cdr acc) (cdr pair)) (t:reduced acc))
                                    ((< (cdr pair) (cdr acc)) pair)
                                    (t acc)))))
       (car)))

#+nil
(index-of-lowest #(7 4 3 2 6 1))

(defun remove-at (v n)
  "Remove a vector element at index N. Allocates a fresh vector."
  (t:transduce (t:comp #'t:enumerate
                       (t:filter (lambda (pair) (not (= n (car pair)))))
                       (t:map #'cdr))
               #'t:vector v))

#+nil
(remove-at #(1 2 3) 1)

(defun pushv (item v)
  (t:transduce (t:once item) #'t:vector v))

#+nil
(pushv 0 #(1 2 3))

;; Also works for Part 1.
(defun greatest-of-digits (digits source)
  (t:transduce
   (t:comp (t:map (lambda (line) (t:transduce (t:map #'char->int) #'t:vector line)))
           (t:map (lambda (ns)
                    (let ((running (->> (t:reversed ns)
                                        (t:transduce (t:take digits) #'t:vector)
                                        (reverse))))
                      (->> (t:reversed ns)
                           (t:transduce (t:drop digits)
                                        (t:fold (lambda (acc n)
                                                  (let* ((acc-n (vector->int acc))
                                                         (pot-v (->> acc index-of-lowest (remove-at acc) (pushv n)))
                                                         (pot-n (vector->int pot-v)))
                                                    (cond ((> pot-n acc-n) pot-v)
                                                          (t acc))))
                                                running))))))
           (t:map #'vector->int))
   #'+ source))

#+nil
(greatest-of-digits 2 #p"03-input.txt")
#+nil
(greatest-of-digits 12 #p"03-input.txt")

