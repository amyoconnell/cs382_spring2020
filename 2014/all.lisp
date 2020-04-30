(load "morris2014.lisp")
(load "sandbox.lisp")
(load "parser.lisp")
(load "../lib/min-heap.lisp")

(setf test-file
  (open "../test_output/big-test-output.txt"
  :direction :output :if-exists :append))

(defun test-dir (dir-name)
(let ((results-acc nil))
(dotimes (n 10)
  (let* ((filename (concatenate 'string
    "../sample_stnus/" dir-name
    "/00" (write-to-string n) ".plainStnu"))
    (stnu (parse-file filename))
    (results (determine-dc stnu)))

    ; (format t "~A file ~A dc results: ~A~%"
    ;     dir-name n results)
    (push (list n results) results-acc)
    ))

(dolist (result results-acc)
  (format test-file "~A file ~A dc results: ~A~%"
      dir-name (first result) (second result)))))

(test-dir "dc-10")
(test-dir "dc-20")
(test-dir "dc-30")
(test-dir "dc-40")
(test-dir "ndc-10")
(test-dir "ndc-20")
(test-dir "ndc-30")
(test-dir "ndc-40")


(close test-file)


; (setf dc-2-stnu (parse-file "../sample_stnus/dc-2.stnu"))
; (setf dc-3-stnu (parse-file "../sample_stnus/dc-3.stnu"))
; (setf dc-5-stnu (parse-file "../sample_stnus/dc-5.stnu"))
; (setf ml-2-stnu (parse-file "../sample_stnus/magic-loop-2.stnu"))
; (setf ml-3-stnu (parse-file "../sample_stnus/magic-loop-3.stnu"))
; (setf ml-5-stnu (parse-file "../sample_stnus/magic-loop-5.stnu"))
; (setf dc-big-stnu (parse-file "../sample_stnus/big_test.stnu"))
;; turn array of cl lists into cl structs

; (setf dc-2-results (determine-dc dc-2-stnu))
; (setf dc-3-results (determine-dc dc-3-stnu))
; (setf dc-5-results (determine-dc dc-5-stnu))
; (setf ml-2-results (determine-dc ml-2-stnu))
; (setf ml-3-results (determine-dc ml-3-stnu))
; (setf ml-5-results (determine-dc ml-5-stnu))
; (setf dc-big-results (determine-dc dc-big-stnu))


;(print-stnu-short test_stnu t)
;(format t "~A~%" (negative-tp-vec dc-2-stnu))
; (format t "dc-2 dc results: ~A~%" dc-2-results)
; (format t "dc-3 dc results: ~A~%" dc-3-results)
; (format t "dc-5 dc results: ~A~%" dc-5-results)
; (format t "ml-2 dc results: ~A~%" ml-2-results)
; (format t "ml-3 dc results: ~A~%" ml-3-results)
; (format t "ml-5 dc results: ~A~%" ml-3-results)
; (format t "dc-big dc results: ~A~%" dc-big-results)
;(cl-list-to-struct test_stnu)
;(print-stnu-short test_stnu t)
