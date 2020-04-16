;; ===========================================
;;  FILE:    params.lisp
;;  AUTHOR:  luke hunsberger
;;  DATE:    may 2018
;; ===========================================
;;  Constants and parameters for various STNU functions


;;  *N-INCR* / *K-INCR*
;; --------------------------------------------------------
;;  Amount by which vectors and matrices are incremented 
;;  in the N or K dimensions when the numbers of TPs or CONT LINKS
;;  can't be accommodated by current vectors/matrices.

(defparameter *n-incr* 100)
(defparameter *k-incr* 30)

(defparameter *init-size* (list *n-incr* *n-incr*))
(defparameter *init-n-k-size* (list *n-incr* *k-incr*))

