;; ====================================
;;  FILE:    all.lisp
;;  AUTHOR:  amy o'connell
;; ====================================
;;  This file loads all files needed to run Morris's
;;  2006 DC-checking algorithm and tests a sample STNU
;; =========================================================

(load "../lib/dc-fixnum-arrays.lisp")
(load "../lib/dc-inf-arith.lisp")
(load "../lib/new-new-stnu.lisp")
(load "../lib/params.lisp")
(load "../lib/vectors.lisp")
(load "../lib/min-heap.lisp")
(load "parser.lisp")
(load "morris2006-min-heap.lisp")

(defun test-stnu-to-file (stnu_file output)
  (let ((stnu (parse-file stnu_file)))
    (format output "~%Before Propagation:~%~%")
    (print-stnu-short stnu output)
    (if (is-dc stnu)
      (format output "~%Test STNU is dynamically controllable~%~%")
      (format output "~%Test STNU is not dynamically controllable~%~%"))
    (format output "~%After Propagation:~%~%")
    (print-stnu-short stnu output)))

(defun test-stnu (stnu_file)
  (test-stnu-to-file stnu_file t))

;/home/aoconnell/Desktop/cs382
(let ((output (open "../test_output/output.txt" :direction :output
        :if-exists :supersede
        :if-does-not-exist :create)))

    (format output "~%========================================================================~%")
  (format output "  Test 1: dc-3.stnu - should be dynamically controllable~%")
      (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/dc-3.stnu" output)
      (format output "~%========================================================================~%")
  (format output "  Test 2: magic-loop-2.stnu - should not be dynamically controllable~%")
      (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/magic-loop-2.stnu" output)
      (format output "~%========================================================================~%")
  (format output "  Test 3: magic-loop-3.stnu - should not be dynamically controllable~%")
      (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/magic-loop-3.stnu" output)
  (format output "~%========================================================================~%")
  (format output "  Test 4: magic-loop-5.stnu - should not be dynamically controllable~%")
      (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/magic-loop-5.stnu" output)
  (format output "~%========================================================================~%")
  (format output "  Test 5: dc-2.stnu - should be dynamically controllable~%")
      (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/dc-2.stnu" output)
  (format output "~%========================================================================~%")
  (format output "  Test 6: dc-5.stnu - should be dynamically controllable~%")
      (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/dc-5.stnu" output)
  (format output "~%========================================================================~%")
  (format output "  Test 7: morris2006.stnu (from Morris Paper) - should not be dynamically controllable~%")
      (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/morris2006.stnu" output)
  (close output)
)
