;; ====================================
;;  FILE:    new-stnu-2014.lisp
;;  AUTHOR:  amy oconnell
;; ====================================
;;  This file contains a CLOS-based implementation of STNUs
;;  modified for Morris's 2014 DC-Checking algorithm
;; =========================================================

(defconstant  *unprocessed* NIL)
(defconstant  *ancestor* 1)
(defconstant  *terminated* 2)
(defconstant *infinity* 100000000)

;;  The CL struct (contingent link)
;; -------------------------------------

(defstruct cont-link
  a ;; activation time-point (ordinary tp index)
  x ;; lower bound
  y ;; upper bound
  c ;; contingent time-point (ordinary tp index)
  )

;;  EDGE struct:  from to wt succ-index prec-index
;; -----------------------------------------------------------------------
;;  Represents an ordinary edge from FROM to TO with weight WT.
;;  PRED-INDEX is an index into the TOth col of PREDS matrix (in EDGES-ETC).

(defstruct edge
  from
  to
  wt
  pred-index
  )

;;  MAKE-ORD-EDGE - wraper function to make an edge struct
;; ---------------------------------------
;;  INPUT:  FROM, origin TP symbol/name (NOT index)
;;          TO, destination TP
;;          WT, edge weight
;;          SUCC-I, index into the FROMth row of SUCCS matrix in EDGES-ETC
;;          PRED-I, index into the TOth col of PREDS matrix in EDGES-ETC
;;  OUTPUT: edge struct with values set to INPUTS
;;  SIDE EFFECT: none

(defun make-ord-edge (from to wt pred-i)
  (make-edge :from from :to to :wt wt :pred-index pred-i))


;; --------------------------------------------------
;;  The NU-STNU class (for "new" STNU)
;; --------------------------------------------------

(defclass nu-stnu ()

  (
   ;; -----------------------------
   ;;  TIME-POINTS
   ;; -----------------------------
   ;;  If there are N time-points, then they will be represented
   ;;  by the integers, 0, 1, 2, ..., N-1.  However, the user-interface
   ;;  allows time-points to be represented by symbols---hence, the
   ;;  following hash table

   (num-tps
    :accessor num-tps :initform 0)

   ;;  TP-HASH
   ;;    KEY is a symbol (name), VALUE is the corresponding number

   (tp-hash
    :accessor tp-hash :initform (make-hash-table))

   ;;  TP-NAMES-VEC
   ;;    each ith TP index holds the corresponding TP SYMBOL

   (tp-names-vec
    :accessor tp-names-vec)

   ;;  NEGATIVE-TP-VEC - N-item array of bools
   ;;   index i holds t if ith TP is negative
   ;;   negative TP == any TP with a negative incoming edge

   (negative-tp-vec
    :accessor negative-tp-vec)

   ;; TP-STATUS-VEC - N-item vector of ints
   ;;   indicates status of TP during recurcive back-propagation
   ;;   *unprocessed* *ancestor* or *terminated*

   (tp-status-vec
    :accessor tp-status-vec)

   ;; -------------------------
   ;;  CONTINGENT LINKS
   ;; -------------------------
   ;;  If there are K contingent links, they will be indexed from 0 to K-1.

   (num-cls
    :accessor num-cls :initform 0)

   ;;  CL-VEC, a vector containing contingent link structs

   (cl-vec
    :accessor cl-vec)

   ;; CL-INDEX-VEC, vector for accessing CL index from TP index of cont TP
   ;;   INDEX = Ordinary TP index of a contingent time-point, C
   ;;   VAL = CL index for the corresponding contingent link, (A x y C)
   ;;         (or NIL if C is not a contingent time-point)

   (cl-index-vec
    :accessor cl-index-vec)

   ;; ===============
   ;;  ORD-EDGES-ETC (including ordinary distance matrix)
   ;; ===============

   (num-ord-edges
    :accessor num-ord-edges :initform 0)

   ;; ORD-EDGES ... an N-by-N array of ORD-EDGE structs (or NIL)

   (ord-edges :accessor ord-edges)

   ;;  NUM-PREDS: N item vector of ints

   (num-preds :accessor num-preds)

   ;;  PREDS: an N-by-N array of ORD-EDGE structs

   (preds :accessor preds)

   )) ;; end DEFCLASS NU-STNU
