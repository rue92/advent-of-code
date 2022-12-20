(ql:quickload "alexandria")

(defvar radio-filename "./input_6.txt")
(defvar unique-characters 4)

(defun radio-load-string ()
  (uiop:read-file-string radio-filename))

(defun string-to-list (string)
  (loop for char across string collect char))

(defun iterate-through-string (radio-string)
  (loop for i from 1 to (- (length radio-string) unique-characters)
        if (alexandria:setp (string-to-list (subseq radio-string i (+ i unique-characters))) :test #'eq)
          collect (+ i unique-characters)))

(first (iterate-through-string (radio-load-string)))

;; Part 2
(setf unique-characters 14)
(first (iterate-through-string (radio-load-string)))
