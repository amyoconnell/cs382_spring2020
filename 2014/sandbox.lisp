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
     :accessor negative-tp-vec :initform NIL)

   ;; TP-STATUS-VEC - N-item vector of ints
   ;;   indicates status of TP during recurcive back-propagation
   ;;   *

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
