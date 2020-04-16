;; ===============================================
;;  FILE:    vectors.lisp
;;  AUTHOR:  luke hunsberger
;;  DATE:    may 2018
;; ===============================================

(in-package user)

;;  Functions defined herein
;; -----------------------------------------------------------------------
;;  MAKE-VECTOR, COPY-VECTOR-CONTENTS, COPY-VECTOR
;;  VECTOR-TO-N, VECTOR->LIST, VECTOR-SWAP!, DEAL!  


(defun make-vector (length &key (initial-element 0)) 
  (make-array length :initial-element initial-element))

;;;  MAKE functions for creating new matrices and vectors of various kinds

(defun make-a-vector (&optional (n *n-incr*) (elt nil))
  (make-vector n :initial-element elt))

(defun make-num-vector (&optional (n *n-incr*))
  (make-vector n :initial-element 0))


(defun copy-vector-contents (oldvec newvec limit)
  (dotimes (i limit)
    (setf (svref newvec i) (svref oldvec i)))
  ;; return the NEW vector
  newvec)

(defun copy-vector
    (harry)
  (let* ((len (length harry))
	 (new-harry (make-array len :initial-element nil)))
    (dotimes (i len)
      (setf (aref new-harry i) (aref harry i)))
    new-harry))

;;  VECTOR-TO-N
;; ------------------------------------------
;;  INPUT:   N, non-negative integer
;;  OUTPUT:  A vector of the form #(0 1 2 ... N-1)

(defun vector-to-n (n)
  (let ((vecky (make-fixnum-vec n)))
    (dotimes (i n)
      (setf (aref vecky i) i))
    vecky))

;;  VECTOR->LIST
;; ------------------------------------------
;;  INPUT:   VECK, a vector
;;  OUTPUT:  Corresponding list.

(defun vector->list (veck)
  (let ((listy nil)
	(n (length veck)))
    (dotimes (i n)
      (push (aref veck i) listy))
    (nreverse listy)))

;;  VECTOR-SWAP!
;; ---------------------------------------
;;  INPUTS:  VECKY, a vector
;;           I, J, legal indices for VECKY
;;  OUTPUT:  none
;;  SIDE EFFECT:  Swaps elements of VECKY in slots I and J.

(defun vector-swap!
    (vecky i j)
  (let ((tmp (svref vecky i)))
    (setf (svref vecky i) (svref vecky j))
    (setf (svref vecky j) tmp)))

;;  DEAL!
;; ------------------------------------
;;  INPUTS:  VECKY, a vector
;;           NUM-LEFT, an integer:  0 < num-left <= (length VECKY)
;;  OUTPUT:  Randomly selected element from first NUM-LEFT elts of VECKY

(defun deal!
    (vecky num-left)
  (vector-swap! vecky (1- num-left) (random num-left)))

;;; ---------------------
;;;  SCRAMBLE
;;; ---------------------
;;;  INPUT:  NUM, a positive integer
;;;  OUPUT:  A vector of length NUM that contains the integers from 0 to NUM-1
;;;           in a random order

(defun scramble (num)
  (let ((vecko (make-fixnum-vec num))
	tmp)
    ;; init contents to 0, 1, 2, ..., num-1.
    (dotimes (i num) (setf (aref vecko i) i))
    ;; randomly pick one element to move to the "discard" pile
    (dotimes (i num)
      (let ((randy (random (- num i))))
	  (setf tmp (aref vecko (- num i 1)))
	  (setf (aref vecko (- num i 1)) (aref vecko randy))
	  (setf (aref vecko randy) tmp)))
    ;; return the scrambled vector
    vecko))

(defun veck-invert (veck)
  (let* ((num (length veck))
	 (inverto (make-vector num)))
    (dotimes (i num)
      (setf (aref inverto (aref veck i)) i))
    inverto))
	 
