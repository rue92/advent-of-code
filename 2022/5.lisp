(ql:quickload :cl-ppcre)
(ql:quickload "split-sequence")

(defvar crane-filename "./input_5.txt")

(defun parse-instruction-line (line)
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" line)))

(defun remove-last-line (l)
  (reverse (cdr (reverse l))))

(defun parse-initial-state (lines)
  (let ((slices (remove-last-line lines)))
    (mapcar
     (lambda (line)
       (loop for i from 0 to (length line) by 4 collect (subseq line (+ i 1) (+ i 2)))) slices)))

(defun assemble-stacks (slices)
  (loop for i from 0 to (- (length (first slices)) 1) collect
        (loop for slice in slices if (not (equal (nth i slice) " ")) collect (nth i slice))))

(defun crane-load-state-and-instructions ()
  (destructuring-bind (initial-state instructions)
      (split-sequence:split-sequence "" (uiop:read-file-lines crane-filename) :test #'equal)
    (list (assemble-stacks (parse-initial-state initial-state)) (mapcar #'parse-instruction-line instructions))))

(defun move-count (instr)
  (first instr))
(defun move-source (instr)
  (- (second instr) 1))
(defun move-dest (instr)
  (- (third instr) 1))

(defun execute-instruction (state instruction)
  (loop for i from 1 to (move-count instruction) do
    (push (pop (nth (move-source instruction) state)) (nth (move-dest instruction) state))
        finally (return state)))

(defun execute-instructions ()
  (let* ((crane-state-and-instructions (crane-load-state-and-instructions))
         (initial-state (first crane-state-and-instructions))
         (instructions (second crane-state-and-instructions)))
    (reduce #'execute-instruction instructions :initial-value initial-state)))

(loop for stack in (execute-instructions) collect (first stack))

;; part 2
(defun execute-instruction (state instruction)
  (setf (nth (move-dest instruction) state) (append
   (subseq (nth (move-source instruction) state) 0 (move-count instruction))
   (nth (move-dest instruction) state)))
  (loop for i from 1 to (move-count instruction) do
    (pop (nth (move-source instruction) state))
        finally (return state)))

(loop for stack in (execute-instructions) collect (first stack))
