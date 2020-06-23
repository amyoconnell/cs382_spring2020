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

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;  HELPFUL TESTING FUNCTIONS
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;  TEST-STNU-TO-FILE
;; ---------------------------------------
;;  function to dc test an .stnu file
;; ---------------------------------------
;;  INPUT:  STNU_FILE: a .stnu file
;;          OUTPUT: output file to print results to
;;  OUTPUT: meh
;;  SIDE EFFECT: writes results of dc tests in OUTPUT

(defun test-stnu-to-file (stnu_file output)
  (let (; STNU: stnu instance from STNU_FILE
        (stnu (parse-file stnu_file)))

    ;; pretty print STNU contents before DC testing propagation to OUTPUT
    (format output "~%Before Propagation:~%~%")
    (print-stnu-short stnu output)

    ;; print DC test results
    (if (is-dc stnu)
        (format output "~%Test STNU is dynamically controllable~%~%")
        (format output "~%Test STNU is not dynamically controllable~%~%"))

    ;; pretty print updated STNU contents after DC testing
    ;; propagation to OUTPUT
    (format output "~%After Propagation:~%~%")
    (print-stnu-short stnu output)))

;;  TEST-STNU
;; ---------------------------------------
;;  function to dc test an .stnu file
;; ---------------------------------------
;;  INPUT:  STNU_FILE: a .stnu file
;;  OUTPUT: meh
;;  SIDE EFFECT: prints results of dc test to terminal

(defun test-stnu (stnu_file)
  (test-stnu-to-file stnu_file t))

;;  TEST-DIR
;; ---------------------------------------
;;  function to dc test a directory of .stnu files
;; ---------------------------------------
;;  INPUT:  DIR-NAME: a directory name
;;          TEST-FILE: output file to print results to
;;  OUTPUT: meh
;;  SIDE EFFECT: writes results of dc tests in TEST-FILE

(defun test-dir (dir-name test-file)
  ;; for each file in DIR-NAME
  (dotimes (n 10)
           (let* (;; FILE-NAME: string -> name of nth file in DIR
                  (filename (concatenate 'string
                                         "../sample_stnus/" dir-name
                                         "/00" (write-to-string n) ".plainStnu"))
                  (stnu (parse-file filename)) ; STNU: stnu instance from filename
                  (result (is-dc stnu))) ; RESULT: bool -> is STNU in filename DC?

             ;; pretty print result to test-file
             (format test-file "~A file ~A dc result: ~A~%"
                     dir-name n result)
             )))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;  ACTUAL TESTS
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; ---------------------------------------
;;  200 TP Tests
;; ---------------------------------------

;; 200 TP test output file: BIG-TEST-OUTPUT2006.TXT
;; TEST-FILE: 200 TP test file stream

; (let ((test-file
;        (open "../test_output/big-test-output2006.txt"
;              :direction :output :if-exists :append :if-does-not-exist :create)))
;
;   ;; *** These tests take several hours to run, and ~might~ get you kicked
;   ;;  out of mote indefinitely. Seriously would not recommend running ***
;
;   ;; Pre-generated results can be found in
;   ;; ../test_output/big-test-output2006.txt
;
;   (test-dir "dc-10" test-file)
;   (test-dir "dc-20" test-file)
;   (test-dir "dc-30" test-file)
;   (test-dir "dc-40" test-file)
;   (test-dir "ndc-10" test-file)
;   (test-dir "ndc-20" test-file)
;   (test-dir "ndc-30" test-file)
;   (test-dir "ndc-40" test-file)
;
;   (close test-file))

;; ---------------------------------------
;;  Small Tests
;; ---------------------------------------

;; small test output file: output.txt
(let (; OUTPUT: small test file stream
      (output (open "../test_output/output.txt" :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)))

  (format output "~%========================================================================~%")
  (format output "  dc-2.stnu - should be dynamically controllable~%")
  (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/dc-2.stnu" output)

  (format output "~%========================================================================~%")
  (format output "  dc-3.stnu - should be dynamically controllable~%")
  (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/dc-3.stnu" output)

  (format output "~%========================================================================~%")
  (format output "  dc-5.stnu - should be dynamically controllable~%")
  (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/dc-5.stnu" output)

  (format output "~%========================================================================~%")
  (format output "  magic-loop-2.stnu - should not be dynamically controllable~%")
  (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/magic-loop-2.stnu" output)

  (format output "~%========================================================================~%")
  (format output "  magic-loop-3.stnu - should not be dynamically controllable~%")
  (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/magic-loop-3.stnu" output)

  (format output "~%========================================================================~%")
  (format output "  magic-loop-5.stnu - should not be dynamically controllable~%")
  (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/magic-loop-5.stnu" output)

  (format output "~%========================================================================~%")
  (format output "  morris2006.stnu (from Morris Paper) - should not be dynamically controllable~%")
  (format output "========================================================================~%")
  (test-stnu-to-file "../sample_stnus/morris2006.stnu" output)

  (close output))
