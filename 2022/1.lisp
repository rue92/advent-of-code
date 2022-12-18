(ql:quickload "asdf")
(ql:quickload :cl-ppcre)
(ql:quickload "iterate")
(ql:quickload "split-sequence")

(defclass elf ()
  ((idx
    :initarg :idx
    :accessor idx)
   (food
    :initform 0
    :accessor food)))
(defmethod feed ((obj elf) amount)
           (setf 
                (food obj) 
                (+ (slot-value obj 'food) amount)))
(defmethod print-object ((obj elf) stream)
  (print-unreadable-object (obj stream :type t)
                           (with-accessors ((food food))
                                           obj
                                           (format stream "Food: ~a" food))))


(defun load-elves (filename)
  (mapcar (lambda (x) 
    (let* ((elf (make-instance 'elf :idx 1)))
      (loop for item in x do
            (feed elf (parse-integer item)))
      elf))
    (split-sequence:split-sequence "" (uiop:read-file-lines filename)
         :test #'equal)))

;; Part 1
(first (sort (load-elves "./input_1.txt") #'> :key #'food))

;; Part 2
(reduce (lambda (acc elf) (+ acc (food elf))) (subseq (sort (load-elves "./input_1.txt") #'> :key #'food) 0 3) :initial-value 0)
