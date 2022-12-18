(ql:quickload "asdf")
(ql:quickload :cl-ppcre)
(ql:quickload "split-sequence")

(defun rps-remap-move (char)
  (cond
    ((or (equal "A" char) (equal "X" char)) "Rock")
    ((or (equal "B" char) (equal "Y" char)) "Paper")
    ((or (equal "C" char) (equal "Z" char)) "Scissors")
    (t (error "Invalid character"))))

(defun rps-get-move (rps-round)
  (rps-remap-move (second rps-round)))

(defun rps-get-opponent (rps-round)
  (rps-remap-move (first rps-round)))

(defun rps-move-value (rps-round)
  (let
      ((rps-move (rps-get-move rps-round)))
    (cond
      ((equal "Rock" rps-move) 1)
      ((equal "Paper" rps-move) 2)
      ((equal "Scissors" rps-move) 3)
      (t (error "Incorrect mapping value")))))

(defun rps-outcome-value (rps-round)
  (let 
      ((move (rps-get-move rps-round))
       (opponent (rps-get-opponent rps-round)))
    (cond
      ;; tie
      ((equal move opponent) 3)
      ;; rock wins
      ((and (equal move "Rock") (equal opponent "Scissors")) 6)
      ;; paper wins
      ((and (equal move "Paper") (equal opponent "Rock")) 6)
      ;; scissors wins
      ((and (equal move "Scissors") (equal opponent "Paper")) 6)
      (t 0))))

(defun rps-round-value (rps-round)
  (+ (rps-move-value rps-round) (rps-outcome-value rps-round)))

(defvar rps-file "./input_2.txt"
  "Path to the Rock Paper Scissors Strategy")

(defun load-rounds (filename)
  (mapcar
   (lambda (line)
     (cl-ppcre:split "\\s" line))
  (uiop:read-file-lines filename)))

;; Part 1
(reduce #'+ (mapcar
 (lambda (round)
   (rps-round-value round))
 (load-rounds rps-file)))

;; Part 2
;; Redefine some things
(defun rps-opponent (rps-round)
  (rps-remap-move (first rps-round)))

(defun rps-expected-outcome (rps-round)
  (let ((outcome (second rps-round)))
  (cond
    ((equal outcome "X") 0)
    ((equal outcome "Y") 3)
    ((equal outcome "Z") 6)
    (t (error "Invalid outcome")))))

(defun rps-our-move (rps-round)
  (let
      ((opponent (rps-opponent rps-round))
       (outcome (second rps-round)))
    (cond
      ((equal outcome "X")
       (cond
         ((equal opponent "Rock") "Scissors")
         ((equal opponent "Scissors") "Paper")
         ((equal opponent "Paper") "Rock")))
      ((equal outcome "Y") opponent)
      ((equal outcome "Z")
       (cond ((equal opponent "Rock") "Paper")
             ((equal opponent "Scissors") "Rock")
             ((equal opponent "Paper") "Scissors"))))))

(defun rps-move-value (rps-round)
  (let
      ((rps-move (rps-our-move rps-round)))
    (cond
      ((equal "Rock" rps-move) 1)
      ((equal "Paper" rps-move) 2)
      ((equal "Scissors" rps-move) 3)
      (t (error "Incorrect mapping value")))))

(defun rps-round-value (rps-round)
  (+ (rps-move-value rps-round) (rps-expected-outcome rps-round)))

(reduce #'+ (mapcar
 (lambda (round)
   (rps-round-value round))
 (load-rounds rps-file)))
