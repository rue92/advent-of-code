(ql:quickload "asdf")
(ql:quickload :cl-ppcre)
(ql:quickload "iterate")
(ql:quickload "split-sequence")

(defvar cleanup-filename "./input_4.txt")

(defun load-pairs ()
  (mapcar #'parse-pair
          (uiop:read-file-lines cleanup-filename)))

(defun parse-range (range)
  (let ((split-range (cl-ppcre:split "-" range)))
    (list (parse-integer (first split-range))
          (parse-integer (second split-range)))))

(defun parse-pair (pair)
  (mapcar #'parse-range (cl-ppcre:split "," pair)))

(defun range-begin (range)
  (first range))
(defun range-end (range)
  (second range))

(defun range-proper-subset (first second)
  (and (>= (range-begin second) (range-begin first))
           (<= (range-end second) (range-end first))))

(defun count-proper-subsets ()
  (let ((pairs (load-pairs)))
    (loop
      for pair in pairs sum
                        (cond
                          ((range-proper-subset (first pair) (second pair)) 1)
                          ((range-proper-subset (second pair) (first pair)) 1)
                          (t 0)))))

(print (count-proper-subsets))

;; Part 2
(defun range-overlaps (first second)
  (cond ((or (range-proper-subset first second)
             (range-proper-subset second first)) t)
        ((and (>= (range-end first) (range-begin second))
              (<= (range-end first) (range-end second))) t)
        ((and (>= (range-begin first) (range-begin second))
              (<= (range-begin first) (range-end second))) t)))

(defun count-overlapping-subsets ()
  (let ((pairs (load-pairs)))
    (loop
      for pair in pairs sum
                        (cond
                          ((range-overlaps (first pair) (second pair)) 1)
                          ((range-overlaps (second pair) (first pair)) 1)
                          (t 0)))))

(print (count-overlapping-subsets))
