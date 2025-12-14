(in-package :aoc)

(defparameter *sample* "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defun ranges (offset)
  (funcall (p:sep (p:char #\,)
                  (p:ap (lambda (1st 2nd)
                          (cons (string->simple 1st)
                                (string->simple 2nd)))
                        (p:recognize #'p:unsigned)
                        (p:*> (p:char #\-) (p:recognize #'p:unsigned))))
           offset))

#+nil
(p:parse #'ranges *sample*)

;; --- Part 1 Solution --- ;;

;; It's enough to split the number in half (adjusting for odd-lengthed numbers)
;; and increment the first half only, looking for matches less than the range's
;; top end.

(defun nearest-even-up (start)
  (let ((len (length start)))
    (cond ((zerop (mod len 2)) (p:parse #'p:unsigned start))
          (t (expt 10 len)))))

#+nil
(nearest-even-up "1234")
#+nil
(nearest-even-up "7")

(fn half-split (-> fixnum fixnum))
(defun half-split (n)
  "Truncate the right half digits, being mindful that if the left half is already
lower than the right, we need to bump it in order to be useful."
  (let* ((s (format nil "~a" n))
         (len (/ (length s) 2))
         (left  (p:parse #'p:unsigned (t:transduce (t:take len) #'t:string s)))
         (right (t:transduce (t:comp (t:drop len)
                                     (t:drop-while (lambda (c) (char= #\0 c))))
                             #'t:string s))
         (right (cond ((string= "" right) 0)
                      (t (p:parse #'p:unsigned right)))))
    (cond ((< left right) (1+ left))
          (t left))))

#+nil
(half-split 1234)

#+nil
(p:parse #'p:unsigned "00")

(fn mirror (-> fixnum fixnum))
(defun mirror (n)
  "Extend a number by its mirror image."
  (declare (optimize (speed 3)))
  (let ((s (format nil "~a" n)))
    (+ n (* n (expt 10 (length s))))))

#+nil
(mirror 12)

(defun mirrors-between (start end)
  "Source: Yields numbers of even length that have a mirrored left and right side."
  (let* ((curr  (half-split (nearest-even-up start)))
         (end-n (p:parse #'p:unsigned end)))
    (t::make-generator
     :func (lambda ()
             (let ((mirror (mirror curr)))
               (cond ((< end-n mirror) t::*done*)
                     (t (incf curr)
                        mirror)))))))

#+nil
(t:transduce #'t:pass #'t:cons (mirrors-between "565653" "565659"))

#+nil
(t:transduce (t:map (lambda (pair) (t:transduce #'t:pass #'+ (mirrors-between (car pair) (cdr pair)))))
             #'+
             (p:parse #'ranges *sample*))

#+nil
(->> (read-string #p"02-input.txt")
     (p:parse #'ranges)
     (t:transduce (t:map (lambda (pair) (t:transduce #'t:pass #'+ (mirrors-between (car pair) (cdr pair)))))
                  #'+))

;; --- Part 2 Solution --- ;;

;; It feels more closed-form, but you do still need to produce the actual
;; numbers since you require the final sum of the IDs in the end.

;; The longest numbers are only 10 digits, so I only have to calculate mirror
;; lengths of up to 5.

;; Got it, just generate all repeats of a certain length. Further, I know that
;; there are no overlapping ranges, so I only have to generate each number once
;; total, instead of once per each range.

(defparameter +ceiling+ 9999999999
  "The highest possible number in all ranges.")

(defun repeating (start)
  "Source: The general case for repeating digits."
  (let* ((start start)
         (bump  (* 10 start))
         (curr  (+ start (* bump start)))
         (limit (1- bump)))
    (t::make-generator
     :func
     (lambda ()
       (cond ((> start limit) t::*done*)
             (t (let ((prev curr))
                  (setf curr (+ start (* bump prev)))
                  (when (> curr +ceiling+)
                    (incf start)
                    (setf curr (+ start (* bump start))))
                  prev)))))))

#+nil
(t:transduce #'t:pass #'t:cons (repeating 10))

#+nil
(let ((ranges (-<>> (read-string #p"02-input.txt")
                    (p:parse #'ranges)
                    (t:transduce (t:map (lambda (pair)
                                          (cons (p:parse #'p:unsigned (car pair))
                                                (p:parse #'p:unsigned (cdr pair)))))
                                 #'t:cons)
                    (sort <> #'< :key #'car))))
  (t:transduce (t:comp #'t:unique
                       (t:filter (lambda (n) (t:transduce #'t:pass (t:any? (lambda (range) (<= (car range) n (cdr range)))) ranges))))
               #'+ (t:chain (repeating 1)
                            (repeating 10)
                            (repeating 100)
                            (repeating 1000)
                            (repeating 10000))))
