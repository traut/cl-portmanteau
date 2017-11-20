(defpackage #:portmanteau
  (:use :cl)
  (:export :portmanteau))

(in-package #:portmanteau)

(defvar *min-word-size* 3)
(defvar *margin* 2)

(defun run-moving-window (word size)
  (loop for x from 0 to (- (length word) size)
        collect (subseq word x (+ x size))))


(defun is-long-enough (word &key (size *min-word-size*))
  (> (length word) size))


(defun contains-vowel (word)
  (remove nil
          (loop for x from 0 below (length word)
                collect (find (char word x) "aeiou" :test #'char-equal))))


(defun is-far-enough-from-start (idx elem)
  "Check if the end of an element is placed no closer than
  *margin* from the beginning of the string"
  (>= (+ idx (length elem)) *margin*))


(defun is-far-enough-from-end (idx elem word-len)
  "Check if the beginning of an element is placed no closer than
  *margin* from the end of the string"
  (<= (+ idx (length elem)) (- word-len *margin*)))


(defun find-matches (seq-a seq-b word-b-size)
  (let* ((len-seq-b (length seq-b))
         ; leave out first elem of seq-a
         (elems-a (subseq seq-a 1 (length seq-a)))
         ; leave out last 2 elems of seq-b
         (elems-b (subseq seq-b 0 (- len-seq-b 2))))
    (if (and (> (length elems-a) 0)
             (> (length elems-b) 0))
      (loop for elem in elems-a and idx from 0
            if (and (contains-vowel elem)  ; element must contain vowel
                    (is-far-enough-from-start idx elem))
            collect (let ((matched-idx (position elem elems-b :test #'equal)))
                      (vom:debug
                        "matching ~a (idx=~a) in ~a (matched=~a)"
                        elem idx elems-b matched-idx)
                      (if (and matched-idx
                               (is-far-enough-from-end matched-idx elem word-b-size))
                        ; idx + 1 because first elem in seq-a was skipped
                        (list (+ idx 1) matched-idx)))
            into matches
            finally (return (remove nil matches))))))


(defun filter-out-without-vowels (seq)
  (remove-if-not #'contains-vowel seq))


(defun merge-words (word-a index-a word-b index-b)
  (vom:debug "matched indices: ~a (~a) ~a (~a)"
            index-a (subseq word-a 0 index-a)
            index-b (subseq word-b index-b (length word-b)))
  (concatenate
    'string
    (subseq word-a 0 index-a)
    (subseq word-b index-b (length word-b))))


(defun valid-result (result word-a word-b)
  "Validate result word.
  Criteria:
    - result must not be smaller than one of the input words"
  (let ((result-len (length result))
        (word-a-len (length word-a))
        (word-b-len (length word-b)))
    (or (>= result-len word-a-len)
        (>= result-len word-b-len))))


(defun calculate-max-window-size (word-a word-b)
  "Calulate max matching window size from the size
  of input words by dividing length of the longest word by 3.
  The result must not be less than 3"
  (max 3
       (floor (/ (max (length word-a)
                      (length word-b))
                 3))))


(defun portmanteau (word-a
                    word-b
                    &key (max-win-size (calculate-max-window-size word-a word-b)))
  "Calculate portmanteau of 2 input words"
  (if (and (is-long-enough word-a)
           (is-long-enough word-b))
    ; Walk down from window in 3 characters to 1 character
    (loop for win-size from max-win-size downto 1
      do (let* ((blocks-a (run-moving-window word-a win-size))
                (blocks-b (run-moving-window word-b win-size))
                (word-b-len (length word-b))
                (matched-indices (find-matches blocks-a blocks-b word-b-len)))
           (loop for (idx-a idx-b) in matched-indices
                 do (let* ((result (merge-words word-a idx-a word-b idx-b))
                           (is-valid (valid-result result word-a word-b)))
                      (vom:debug "result ~a, valid = ~a" result is-valid)
                      (if is-valid (return-from portmanteau result))))))
    (progn
      (vom:error "Both words must be longer than ~a" *min-word-size*)
      (return-from portmanteau nil))))

;;; vim: set ft=lisp lisp:
