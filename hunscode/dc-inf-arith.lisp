;;; ========================================================================
;;;      FILE:  hunsberger-inf-arith.lisp  
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
;;;   Constants and Functions for dealing with pos-infinity
;;;   ---used in distance-matrix computations.

;;;   NOTE:  Infinity is represented by a very large FIXNUM.

;;; ***********>  WARNING!  Only REPORTS errors, doesn't stop them.

;;;   MACROS:   FINITE?  INFINITE?
;;;             ADD2    ADD3    ADD4

(in-package user)

(defconstant +positive+ 1)
(defconstant +negative+ -1)

(defconstant +infinity+ 100000000)

(defmacro infinite? (x) `(>= ,x +infinity+))
(defmacro finite? (x) `(< ,x +infinity+))

(defmacro sign (x) `(signum ,x))

(defmacro add2 (x y)
  `(let* ((xx ,x)
	  (yy ,y)
	  (raw-sum (+ xx yy)))
     (cond
      ((or (infinite? xx) (infinite? yy))
       +infinity+)
      (T
       raw-sum))))

(defmacro add2-simple-input (x y)
  `(if (or (infinite? ,x) (infinite? ,y))
       +infinity+
     (+ ,x ,y)))

(defmacro add3 (x y z)
  `(let* ((xx ,x)
	  (yy ,y)
	  (zz ,z)
	  (raw-sum (+ xx yy zz)))
     (cond
      ((or (infinite? xx) (infinite? yy) (infinite? zz))
       +infinity+)
      (T 
       raw-sum))))

(defmacro add3-simple-input (x y z)
  `(if (or (infinite? ,x) (infinite? ,y) (infinite? ,z))
       +infinity+
     (+ ,x ,y ,z)))


(defmacro add4 (w x y z)
  `(let* ((ww ,w)
	  (xx ,x)
	  (yy ,y)
	  (zz ,z)
	  (raw-sum (+ ww xx yy zz)))
     (cond 
      ((or (infinite? xx) (infinite? yy)
	   (infinite? zz) (infinite? ww))
       +infinity+)
      (T 
       raw-sum))))

