(ql:quickload "asdf")
(ql:quickload :cl-ppcre)
(ql:quickload "iterate")
(ql:quickload "split-sequence")


(defvar backpack-filename "./input_3.txt")

;; Part 1
(defun load-backpacks ()
  (uiop:read-file-lines backpack-filename))

(defun string-to-list (string)
  (loop for char across string collect char))

(defun backpack-compartments ()
  (mapcar
   (lambda (backpack)
     (let ((boundary (/ (length backpack) 2)))
       (list (string-to-list (subseq backpack 0 boundary))
             (string-to-list (subseq backpack boundary)))))
   (load-backpacks)))

(defun wrong-supplies ()
  (mapcar
   (lambda (backpack)
     (first (remove-duplicates (intersection (first backpack) (second backpack)))))
   (backpack-compartments)))

(defun symbol-to-priority (symbol)
  (let* ((lower-a (char-int #\a))
         (upper-a (char-int #\A))
         (sym-int (char-int symbol))
         (upper-math (+ 27 (- sym-int upper-a)))
         (lower-math (+ 1 (- sym-int lower-a))))
    (cond
      ((>= sym-int lower-a) lower-math)
      ((>= sym-int upper-a) upper-math))))

(reduce #'+ (mapcar #'symbol-to-priority (wrong-supplies)))

;; Part 2
(defun group-backpacks ()
  (let ((backpacks (load-backpacks)))
    (loop for i from 0 to (- (length backpacks) 1) by 3 collect (subseq backpacks i (+ i 3)))))

(defun group-badges ()
  (mapcar
   (lambda (group)
     (first (remove-duplicates
             (intersection
              (intersection
               (string-to-list (first group))
               (string-to-list (second group)))
               (string-to-list (third group))))))
   (group-backpacks)))

(reduce #'+ (mapcar #'symbol-to-priority (group-badges)))
