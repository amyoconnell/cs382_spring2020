;;; ========================================================================
;;;      FILE:  hunsberger-fixnum-arrays.lisp  
;;;    AUTHOR:  luke hunsberger
;;;      DATE:  dec. 2006 
;;; ========================================================================
;;;
;;; ========================================================================
;;;    THIS CODE WAS DEVELOPED BY LUKE HUNSBERGER, VASSAR COLLEGE.
;;;    THIS CODE IS COPYRIGHT 2006 BY LUKE HUNSBERGER, VASSAR COLLEGE.
;;;    THIS CODE MAY BE USED FOR RESEARCH PURPOSES UNDER THE CONDITION
;;;    THAT THIS NOTICE REMAINS IN THE CODE.  ALL OTHER USES OF THIS
;;;    CODE ARE PROHIBITED.
;;; ========================================================================
;;;
;;;   Implementation of Arrays of fixnums for STNs.

(in-package user)

(defun make-nil-matrix (&optional (size *init-size*))
  (make-array size :initial-element nil))

;;; ====================================================================
;;;   MAKE-FIXNUM-ARRAY
;;; ====================================================================
 
(defun make-fixnum-array (dim1 dim2 &key (initial-element 0))
  (make-array (list dim1 dim2)
	      :adjustable nil
	      :initial-element initial-element
	      :element-type 'fixnum))

(defun make-dist-mat (num)
  (let ((matty (make-array (list num num)
			   :initial-element +infinity+)))
    (dotimes (i num)
      (setf (aref matty i i) 0))
    matty))

(defun make-fixnum-vec (dim &key (initial-element 0))
  (make-array dim
	      :adjustable nil
	      :initial-element initial-element
	      :element-type 'fixnum))
  
(defmacro num-rows (arr) `(array-dimension ,arr 0))
(defmacro num-cols (arr) `(array-dimension ,arr 1))

;;; ====================================================================
;;;   COPY-FIXNUM-ARRAY-CONTENTS
;;; ====================================================================

(defun copy-fix-array (from-array to-array limit)
  (dotimes (i limit)
    (dotimes (j limit)
      (setf (aref to-array i j) (aref from-array i j))))
  ;; return the TO-ARRAY
  to-array)


(defun copy-fix-array-2 (from-array to-array lim1 lim2)
  (dotimes (i lim1)
    (dotimes (j lim2)
      (setf (aref to-array i j) (aref from-array i j))))
  ;; return the TO-ARRAY
  to-array)


(defun print-mat (mat numrows numcols)
    (format t "         | ")
    (dotimes (c numcols) (format t "~8D " c))
    (newline t)
    (dotimes (i (+ (* (+ 1 numcols) 9) 2)) (format t "-"))
    (newline t)
    (dotimes (r numrows)
      (format t "~8D | " r)
      (dotimes (c numcols)
	(if (finite? (aref mat r c))
	    (format t "~8D " (aref mat r c))
	  (format t "      -- ")))
      (newline t)))
