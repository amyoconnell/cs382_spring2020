;; ====================================
;;  FILE:    new-stnu.lisp
;;  AUTHOR:  luke hunsberger
;; ====================================
;;  This file contains a CLOS-based implementation of STNUs
;;  and the DC-checking algorithm discussed in my ICAART-2014
;;  conference paper.
;; =========================================================
;;  A major difference from preceding implementations:
;;   -- uses vectors and arrays instead of hash tables
;;        for representing edges (should be faster)
;;   -- uses the "alternating Dijkstra" technique first
;;        discussed in my TIME-2013 paper
;;   -- enables the order in which the lower-case edges
;;        are processed in Morris' algorithm to be
;;        specified in advance.  Also, uses alternating
;;        Dijkstra to enable new core edges to be integrated
;;        as soon as they are created, not waiting until the
;;        end of the round as in Morris 2006.
;; =========================================================

#|

RAW STNU:  An STNU is a triple (T, C, L) where:

  T is a set of time-points (i.e., var names):
    only need to give NON-CONTINGENT tps since CONTINGENT tps are
    implicit in the cont links.
  C is a set of ordinary edges: (e.g., ((X1 3 X2) (X2 -5 X3) ...))
    note:  ordinary edges can involve contingent tps
  L is a set of cont links:  (e.g., ((A1 3 9 C1) (A2 4 8 C2) ...))

  Example:
    ((X1 X2 X3 X4)
     ((X1 3 X2) (C1 -5 X2) (C2 10 C1))
     ((X1 2 9 C1) (X2 3 8 C2)))


COMMON INTERNAL REPRESENTATIONS:  NU-STNU

  Need different ways of initializing...

    num-tps, num-cls initially 0

    cl-vec (initially a vector with *k-incr* slots)
    cl-index-vec (initally a vector with *n-incr* slots)
    ord-edges-etc (init:  (make-edges-etc)
                             edges:            (make-nil-matrix)
			     num-succs/preds:  (make-num-vector)
			     succs/preds:      (make-nil-matrix)
			     dist:             (make-dist-mat *n-incr*)
    )
    dmax-edges-etc (init:  (make-edges-etc))
    uc-edges-etc (init:  (make-uc-edges-etc)
                             uc-edges:  (make-nil-matrix *init-n-k-size*)
			     num-uc-succs:  (make-num-vector)
			     uc-succs:  (make-nil-matrix *init-n-k-size*)
    )

    where:
      make-nil-matrix takes optional size = *init-size*
      make-a-vector takes an optional n = *n-incr*
      make-num-vector takes an optional n = *n-incr*

  SUMMARY of INITIALIZATIONS...
    NUMBERS:  num-tps, num-cls: 0 is fine

    VECTORS:  cl-vec (*k-incr*)
              cl-index-vec (*n-incr*)
	      ord-edges-etc!num-succs/preds:  (make-num-vector) (*n-incr*)
	      dmax-edges-etc!num-succs/preds:  (make-num-vector) (*n-incr*)
	      uc-edges-etc!num-uc-succs: (make-num-vector) (*n-incr*)

    MATRICES:  ord-edges-etc!edges:  (make-nil-matrix) (*init-size*)
               ord-edges-etc!succs/preds:  (make-nil-matrix) (*init-size*)
	       ord-edges-etc!dist:  (make-dist-mat *n-incr*)
	       dmax-edges-etc (same)
	       uc-edges-etc!uc-edges:  (make-nil-matrix *init-n-k-size*)
	       uc-edges-etc!uc-succs:  (make-nil-matrix *init-n-k-size*)
	          NOTE:  *init-size* = (*n-incr* *n-incr*)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    --- Note to Amy:  You don't need to use the expandable stuff ---
    So, the vectors and matrices are all expandable and are initialized
    to some arbitrary values based on *n-incr* and *k-incr*.
    Better to give constructor method the initial values of these parameters
    and then create the initial vectors and matrices...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  T (the time-points):
    num-tps (number)
    tp-hash (numbers <--> symbols)
  C is a set of edges:
    ORD-EDGES-ETC (an EDGES-ETC):
      EDGES (NxN array of EDGEs {from,to,wt,succ-index,pred-index})
      NUM-SUCCS (vector: tp-index ==> num-succs-for-that-tp)
      SUCCS (NxN array: (from-tp, i) ==> ith successor of from-tp:
                which is an EDGE {from,to,wt,succ-index,pred-index})
      NUM-PREDS
      PREDS
      ;; Not everyone uses DIST... but i guess it doesn't hurt
      ;; to include it..??

      UC-EDGES-ETC:  {uc-edges(n-by-k matrix), num-uc-succs(n vector),
                      uc-succs(n-by-k matrix)}

  L is a set of contingent links:
    num-cls (number)
    cl-vec (vector of CONT-LINKs:  {a,x,y,c}
    cl-index-vec (tp-index-of-c ==> cl-index-for-{a,x,y,c}-in-cl-vec)

|#

(defun newline (x)
    (format t " "))


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
;;  SUCC-INDEX is an index into the FROMth row of SUCCS matrix (in EDGES-ETC).
;;  PRED-INDEX is an index into the TOth col of PREDS matrix (in EDGES-ETC).

(defstruct edge
  from
  to
  wt
  succ-index
  pred-index
  )

;;  UC-EDGE -- should always have negative weight
;; -----------------------------------------------------------------
;;  CLI specifies the contingent link corresponding to an outgoing
;;  upper-case edge.  CLI is the CL-INDEX that gives access to the
;;  cont link, and hence the destination time point (i.e., the
;;  activation time-point for the cont link).  WT is the weight of
;;  this UC edge.

(defstruct uc-edge
  from
      ;; don't need "to" since the destination *must* be the activation
      ;; time-point for cont link
  cli ;; the CL index specifying the corresponding cont link
  wt  ;; the weight of this UC edge
  )


;; ------------------------------------------
;;  EDGES
;; ------------------------------------------
;;  OKAY... here's where things get different!
;;  Going to use vectors and arrays instead of hash-tables to
;;  represent the ordinary and upper-case edges in the STNU.
;;  (Lower-case edges don't need to be represented since we
;;  have the contingent link structs.)

;; =======================
;;  EDGES-ETC
;; =======================
;;  Represents the UNLABELED edges that can appear in any STN, or as
;;  ordinary edges in an STNU, or as unlabeled edges in a DMAX graph
;;  (an STN).

(defclass edges-etc ()
  (
   ;; EDGES ... an N-by-N array of ORD-EDGE structs (or NIL)

   (edges :accessor edges
	  ; :initform (make-nil-matrix)
	  :initarg :edges)

   ;;  NUM-SUCCS ... an N-element vector, where the ith element
   ;;    tells how many ordinary edges are going out from ith TP

   (num-succs :accessor num-succs
	      ; :initform (make-num-vector)
	      :initarg :num-succs)

   ;;  SUCCS ... an N-by-N array, where the ith row is an
   ;;               N-element vector whose first NUM-SUCCS[i]
   ;;               elements contain SUCC structs specifying the
   ;;               successor/destination time-point and the weight of
   ;;               an edge emanating from Ith time-point

   (succs :accessor succs
	  ; :initform (make-nil-matrix)
	  :initarg :succs)

   ;;  NUM-PREDS, PREDS --- analogous

   (num-preds :accessor num-preds
	      ; :initform (make-num-vector)
	      :initarg :num-preds)

   (preds :accessor preds
	  ; :initform (make-nil-matrix)
	  :initarg :preds)

   ;; DIST -- Distance Matrix

   (dist :accessor dist
	 ; :initform (make-dist-mat *n-incr*)
	 :initarg :dist)

   )) ;; end DEFCLASS EDGES-ETC


(defmethod print-edges ((etc edges-etc) numrows numcols)
  (format t "         | ")
  (dotimes (c numcols) (format t "~8D " c))
  (newline t)
  (dotimes (i (+ (* (+ 1 numcols) 9) 2)) (format t "-"))
  (newline t)
  (dotimes (r numrows)
    (format t "~8D | " r)
    (dotimes (c numcols)
      (if (get-edge etc r c)
	  (format t "~8D " (get-weight etc r c))
	(format t "      -- ")))
    (newline t)))

(defmethod printy ((etc edges-etc))
  (format t "---- EDGES: ~A~%" (edges etc))
  (format t "---- NUM-SUCCS:  ~A~%" (num-succs etc))
  (format t "---- SUCCS:  ~A~%" (succs etc))
  (format t "---- NUM-PREDS:  ~A~%" (num-preds etc))
  (format t "---- PREDS:  ~A~%" (preds etc))
  (format t "---- DIST: ~A~%" (dist etc)))

; (defun make-edges-etc (&key (init-max-n *n-incr*)
; 			    (include-dist t))

(defun make-edges-etc (init-max-n)
  (let ((init-size (list init-max-n init-max-n)))
    (make-instance 'edges-etc
      :edges (make-nil-matrix init-size)
      :num-succs (make-num-vector init-max-n)
      :succs (make-nil-matrix init-size)
      :num-preds (make-num-vector init-max-n)
      :preds (make-nil-matrix init-size)
      :dist (make-dist-mat init-max-n)
	      nil)))

; (defun make-empty-edges-etc ()
;   (make-instance 'edges-etc
;     :edges (make-array '(0 0))
;     :num-succs (make-array 0)
;     :succs (make-array '(0 0))
;     :num-preds (make-array 0)
;     :preds (make-array '(0 0))
;     :dist (make-array '(0 0))
;       nil))

(defmethod get-weight ((etc edges-etc) (from fixnum) (to fixnum))
  (edge-wt (aref (edges etc) from to)))

(defmethod get-edge ((etc edges-etc) (from fixnum) (to fixnum))
  (aref (edges etc) from to))


;;  UC-EDGES-ETC
;; ------------------------
;;  No distance matrix for UC edges.  (See DMAX-EDGES-ETC for dmax matrix.)
;;  Only SUCCS followed during DC-checking algorithms, not PREDS.
;;  So, no PREDS-type stuff in here.

(defclass uc-edges-etc ()
  (
   ;;  UC-EDGES ... a N-by-K array of UC-EDGE structs representing UC edges
   ;;    NOTE:  UC-EDGES[i][cli] specifies a UC edge corresponding to
   ;;             the cont link identified by CLI (CL-INDEX).
   ;;             the edge goes from the Ith time-point to the activation
   ;;             time-point of the corresponding cont link

   (uc-edges :accessor uc-edges
	     ; :initform (make-nil-matrix *init-n-k-size*)
	     :initarg :uc-edges)

   ;;  NUM-UC-SUCCS ... an N-element vector where the Ith element
   ;;      specifies the number of UC edges that are outgoing from
   ;;      the Ith time-point

   (num-uc-succs :accessor num-uc-succs
		 ; :initform (make-num-vector)
		 :initarg :num-uc-succs)

   ;;  UC-SUCCS ... an N-by-K array where the [i][j] element is
   ;;      a UC-EDGE struct representing an upper-case edge that
   ;;      is outgoing from the ith TP.  The UC-EDGE struct
   ;;      specfies FROM, CLI and WT, where CLI tells which contingent
   ;;      link is relevant (uc label and destination time point)
   ;;      and WT specifies the weight.

   (uc-succs :accessor uc-succs
	     ; :initform (make-nil-matrix *init-n-k-size*)
	     :initarg :uc-succs)

   ))

(defun make-uc-edges-etc (init-max-n init-max-k)
  (let ((init-n-k-size (list init-max-n init-max-k)))
    (make-instance 'uc-edges-etc
      :uc-edges (make-nil-matrix init-n-k-size)
      :num-uc-succs (make-num-vector init-max-n)
      :uc-succs (make-nil-matrix init-n-k-size))))

; (defun make-empty-uc-edges-etc ()
;   (make-instance 'uc-edges-etc
;     :uc-edges (make-array '(0 0))
;     :num-uc-succs (make-array 0)
;     :uc-succs (make-array '(0 0))))

(defmethod get-uc-edge ((uc-etc uc-edges-etc) from cli)
  (aref (uc-edges uc-etc) from cli))

(defmethod get-uc-weight ((uc-etc uc-edges-etc) from cli)
  (let ((uc-edgie (get-uc-edge uc-etc from cli)))
    (if uc-edgie
	(uc-edge-wt uc-edgie)
      nil)))

(defmethod print-uc-edges ((uc-etc uc-edges-etc) numrows numcols)
  (format t "         | ")
  (dotimes (c numcols) (format t "~8D " c))
  (newline t)
  (dotimes (i (+ (* (+ 1 numcols) 9) 2)) (format t "-"))
  (newline t)
  (dotimes (r numrows)
    (format t "~8D | " r)
    (dotimes (c numcols)
      (if (get-edge uc-etc r c)
	  (format t "~8D " (get-uc-weight uc-etc r c))
	(format t "      -- ")))
    (newline t)))


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

   ;; -------------------------
   ;;  CONTINGENT LINKS
   ;; -------------------------
   ;;  If there are K contingent links, they will be indexed from 0 to K-1.

   (num-cls
   :accessor num-cls :initform 0)

   ;;  CL-VEC, a vector containing contingent link structs

   (cl-vec
   :accessor cl-vec
	;   :initform (make-array 0)
	   :initarg :cl-vec)

   ;; CL-INDEX-VEC, vector for accessing CL index from TP index of cont TP
   ;;   INDEX = Ordinary TP index of a contingent time-point, C
   ;;   VAL = CL index for the corresponding contingent link, (A x y C)
   ;;         (or NIL if C is not a contingent time-point)

   (cl-index-vec
     :accessor cl-index-vec
	;	 :initform (make-array 0)
		 :initarg :cl-index-vec)

   ;; ===============
   ;;  ORD-EDGES-ETC (including ordinary distance matrix)
   ;; ===============

   (num-ord-edges
   :accessor num-ord-edges :initform 0)

   (ord-edges-etc
   :accessor ord-edges-etc
		  ;;:initform (make-empty-edges-etc)
		  :initarg :ord-edges-etc)

   ;; ===============
   ;;  DMAX-EDGES-ETC (including dmax matrix)
   ;; ===============

   (dmax-edges-etc
   :accessor dmax-edges-etc
		   ; :initform (make-edges-etc)
		   :initarg :dmax-edges-etc)

   ;; ===============
   ;;  UPPER-CASE EDGES
   ;; ===============

   (uc-edges-etc
   :accessor uc-edges-etc
		;;:initform (make-empty-uc-edges-etc)
		 :initarg :uc-edges-etc)

   )) ;; end DEFCLASS NU-STNU


; TODO: Finish updating file to static implementation, starting here:
(defun make-nu-stnu (init-max-n init-max-k)
  (let () ;; ((init-size (list init-max-n init-max-n)))
    (make-instance 'nu-stnu
      ;; num-tps = 0; tp-hash = empty hash table; num-cls = 0
      :cl-vec (make-a-vector init-max-k)
      :cl-index-vec (make-a-vector init-max-n)
      :ord-edges-etc (make-edges-etc init-max-n
				     ; :include-dist include-dist
             )
      :dmax-edges-etc ; (if include-dmax
			  (make-edges-etc init-max-n
					  ; :include-dist include-dist )
			; nil
      )
      :uc-edges-etc (make-uc-edges-etc init-max-n init-max-k)

      )
  ))


(defmethod printy ((s nu-stnu))
  (let ((num (num-tps s))
	(num-links (num-cls s)))
    (format t "NU_STNU!~%")
    (format t ".. ~A time-points; ~A cont links~%" num num-links)
    (newline t)
    (format t "CONT-LINKS: ~%")
    (dotimes (i num-links)
      (format t "~A~%" (svref (cl-vec s) i)))
    (format t ".. ORD-EDGES-ETC:~%")
    ; (print-edges (ord-edges-etc s) num num)
    (printy (ord-edges-etc s))
    (newline t)
    (when (dist (ord-edges-etc s))
      (format t ".. DIST: ~%")
      (print-mat (dist (ord-edges-etc s)) num num)
      (newline t))
    (when (dmax-edges-etc s)
      (format t ".. DMAX-EDGES-ETC:~%")
      (print-edges (dmax-edges-etc s) num num)
      (when (dist (dmax-edges-etc s))
	(format t ".. DMAX: ~%")
	(print-mat (dist (dmax-edges-etc s)) num num)))))

(defun make-stnu ()
  (make-instance 'nu-stnu))


;; =============================

;;  EXPAND-MATRICES-NUM-TPS
;; ---------------------------------
;;  expands the following vectors and matrices in an STNU object
;;  to accommodate addtional time-points (not additional cont tps).
;;    matrices:  ORD-EDGES, ORD-SUCCS, ORD-PREDS,
;;                 UC-EDGES**, UC-SUCCS**, MY-CLIS**, UC-PREDS**,
;;                 DIST, DMAX
;;    vectors:   NUM-ORD-SUCCS, NUM-ORD-PREDS, NUM-UC-SUCCS,
;;                 NUM-CLS-ACTIVE-FOR,
;;  NOTE:  For matrices tagged by **, only one dimension of the matrix
;;           is expanded (the dimension that, when initialized, contains
;;           *n-incr* many entries).  The other dimension in
;;           such cases is only expanded when ADD-CONT-LINK is called.

;;  NOTE:  This is only ever called when (NUM-TPS s) == length of one side of
;;           distance matrices  (i.e., current matrices are FULL!)


;;  EXPAND-EDGES-ETC
;;  EXPAND-UC-EDGES-ETC
;;  EXPAND-MATRICES-NUM-TPS


;;  EXPAND-EDGES-ETC
;; -----------------------------------------------
;;  INPUTS:  EDIGES an EDGES-ETC struct
;;           NUM -- current number of TPS in the STNU
;;           NEW-NUM -- new size in NUM-TPS dimension
;;           NEW-SIZE -- new size of arrays in NUM-TPS dimensions

(defmethod expand-edges-etc ((edgies edges-etc) num new-num new-size)
  (let* ((new-edges (make-nil-matrix new-size))
	 (new-num-succs (make-num-vector new-num))
	 (new-succs (make-nil-matrix new-size))
	 (new-num-preds (make-num-vector new-num))
	 (new-preds (make-nil-matrix new-size))
	 (disty (dist edgies))
	 (new-dist (if disty
		       (make-dist-mat new-num)
		     nil)))
    (setf (edges edgies)
      (copy-fix-array (edges edgies) new-edges num))
    (setf (num-succs edgies)
      (copy-vector-contents (num-succs edgies) new-num-succs num))
    (setf (succs edgies)
      (copy-fix-array (succs edgies) new-succs num))
    (setf (num-preds edgies)
      (copy-vector-contents (num-preds edgies) new-num-preds num))
    (setf (preds edgies)
      (copy-fix-array (preds edgies) new-preds num))
    (when disty
      (setf (dist edgies)
	(copy-fix-array (dist edgies) new-dist num))))
  T)

;; EXPAND-UC-EDGES-ETC
;;  Used whenever the number of tps or number of cont links exceeds capacity

;;  NEW-NUM = new length in NUM-TPS dimension
;;  NEW-NUM-LINKS = new length in NUM-CLS dimension

(defmethod expand-uc-edges-etc
    ((s nu-stnu) new-num new-num-links)
  ;;(format t "Expand UC edges etc:  new-num = ~A, new-num-links = ~A!~%"
;;	  new-num new-num-links)
  (let* ((ucs (uc-edges-etc s))
	 (num (num-tps s))
	 (num-links (num-cls s))
	 (new-n-k-size (list new-num new-num-links))
	 (new-uc-edges (make-nil-matrix new-n-k-size))
	 (new-num-uc-succs (make-num-vector new-num))
	 (new-uc-succs (make-nil-matrix new-n-k-size))
	 (new-cl-vec (make-a-vector new-num-links))
	 )
    (setf (uc-edges ucs)
      (copy-fix-array-2 (uc-edges ucs) new-uc-edges num num-links))
    (setf (num-uc-succs ucs)
      (copy-vector-contents (num-uc-succs ucs) new-num-uc-succs num))
    (setf (uc-succs ucs)
      (copy-fix-array-2 (uc-succs ucs) new-uc-succs num num-links))
    (setf (cl-vec s)
      (copy-vector-contents (cl-vec s) new-cl-vec num-links))
    )

;;  (format t "Done with expand-uc-edges-etc!~%")
  ;; return T for fun
  T)


;;  EXPAND-MATRICES-NUM-TPS
;; ---------------------------------------
;;  INPUT:  S, an STNU instance
;;  OUTPUT:  who cares!
;;  SIDE EFFECT:  The matrices and vectors of S that
;;   have one or both dimensions equal to the number of
;;   TPs in S are expanded in that dimension!

(defmethod expand-matrices-num-tps ((s nu-stnu) &key (new-size nil))
  (let* (;; The current length of structs in the NUM-TPs dimension
	 (curr (length (cl-index-vec s)))
	 ;; The current length of structs in the NUM-CLs dimension
	 (curr-size-cls (length (cl-vec s)))
	 ;; Expanding in the NUM-TPs dimension...
	 (new-len (if new-size new-size (+ curr *n-incr*)))
	 (new-array-size (list new-len new-len))
	 ;; New vector for CL-INDICES...
	 (new-cl-index-vec (make-a-vector new-len))
	 ;; numby:  number of tps in the stnu
	 (numby (num-tps s))
	 )
    ;; Copy contents of existing matrices/vectors
    (setf (cl-index-vec s)
      (copy-vector-contents (cl-index-vec s) new-cl-index-vec numby))
    ;; Expand other structures...
    (expand-edges-etc (ord-edges-etc s) numby new-len new-array-size)
    ;; Only expand DMAX-EDGES-ETC if it exists!!
    (when (dmax-edges-etc s)
      (expand-edges-etc (dmax-edges-etc s) numby new-len new-array-size))
    ;; Expand UC-EDGES in the dimension of num-tps
    (expand-uc-edges-etc s new-len curr-size-cls)
    )
  ;; return T just for fun
  T
  )

;; =====================================

(defmethod add-tp-gensym ((s nu-stnu) &key (check? t))

  ;; When we're checking sizes of matrices and vectors
  ;; and we discover that there's not enough room for any more tps...
  (when (and check?
	     (>= (num-tps s)
		 (num-rows (dist (ord-edges-etc s)))))
    ;; Then must expand the relevant matrics and vectors in the STNU
    (expand-matrices-num-tps s))

  ;; Okay, now we're ready to add a new tp...

  (let* ((new-index (num-tps s))
	 ;; generate a symbol for this tp!
	 (tp-sym (gensym "x")))
    ;; increment the number of tps in the stnu
    (incf (num-tps s))
    ;; add sym-tp to tp-hash
    (setf (gethash tp-sym (tp-hash s)) new-index)
    (setf (gethash new-index (tp-hash s)) tp-sym)
    )) ;; end DEFMETHOD

(defmethod add-tps-gensym ((s nu-stnu) (incr fixnum))
  (let* ((num (num-tps s))
	 (new-num (+ num incr))
	 (curr-size (num-rows (edges (ord-edges-etc s)))))
    (when (> new-num curr-size)
      (expand-matrices-num-tps s
			       :new-size
			       (max new-num (+ curr-size *n-incr*))))
    (dotimes (i incr)
      (add-tp-gensym s :check? nil))
    ))

;; =====================================


(defmethod add-edge ((etc edges-etc) (from fixnum) (delta fixnum) (to fixnum))
  (let* ((edgies (edges etc))
	 (edg (aref edgies from to))
	 (dmat (dist etc))
	 )
    (cond
     ;; Case 1:  There's an existing edge whose weight is stronger
     ;;          than the edge being added
     ((and edg (<= (edge-wt edg) delta))
      ;;(format t "add-edge case 1~%")
      ;; don't add the edge
      nil)
     ;; Case 2:  No existing edge
     ((not edg)
      ;; insert edge into EDGES, SUCCS and PREDS arrays
      (add-edge-no-check etc from delta to))
     ;; Case 3:  Existing edge is weaker -- just update WT of existing EDGE
     (t
      ;;(format t "add-edge case 3~%")
      (setf (edge-wt (aref edgies from to)) delta)
      ;; ... and update dist matrix, if nec
      (when dmat (setf (aref dmat from to) delta))
      ))))

;; ADD-EDGE-NO-CHECK

(defmethod add-edge-no-check ((etc edges-etc) (from fixnum) delta (to fixnum))
  (let* ((edgies (edges etc))
	 (edg (aref edgies from to))
	 (dmat (dist etc))
	 )
    (cond
     ;; Case 1:  There's a pre-existing edge
     (edg
      (error "Pre-existing edge in ADD-EDGE-NO-CHECK"))
     ;; Case 2:  No pre-existing edge
     (t
      (let* ((curr-num-succs (svref (num-succs etc) from))
	     (curr-num-preds (svref (num-preds etc) to))
	     (new-edge (make-edge :from from
				  :to to
				  :wt delta
				  :succ-index curr-num-succs
				  :pred-index curr-num-preds)))
	;; add the edge to edges
	(setf (aref edgies from to) new-edge)
	;; update num-succs/num-preds
	(incf (svref (num-succs etc) from))
	(incf (svref (num-preds etc) to))
	;; add entries to succs/preds
	(setf (aref (succs etc) from curr-num-succs) new-edge)
	(setf (aref (preds etc) curr-num-preds to) new-edge)
	;; if DMAT exists, and new edge is stronger than current entry,
	;;   update DMAT entry
	(when (and dmat
		   (numberp delta)
		   (< delta (aref dmat from to)))
	  (setf (aref dmat from to) delta))
	;; return the new edge!
	new-edge
	)))))

(defmethod add-edge ((s nu-stnu) from delta to)
  (add-edge (ord-edges-etc s) from delta to)
  (when (dmax-edges-etc s)
    (add-edge (dmax-edges-etc s) from delta to)))

;; ADD-UC-EDGE-NO-CHECK

(defmethod add-uc-edge-no-check ((ss nu-stnu) (from fixnum) delta (cli fixnum))
  (let* ((uc-etc (uc-edges-etc ss))
	 (uc-edgies (uc-edges uc-etc))
	 (uc-edg (aref uc-edgies from cli))
	 )
    (format t "ADD-UC-EDGE-NO-CHECK: from: ~A, delta: ~A, cli: ~A~%" from delta cli)
    (cond
     ;; Case 1:  There's a pre-existing edge
     (uc-edg
      (error "Pre-existing edge in ADD-UC-EDGE-NO-CHECK"))
     ;; Case 2:  No pre-existing edge
     (t
      (let* ((curr-num-succs (svref (num-uc-succs uc-etc) from))
	     (new-uc-edge (make-uc-edge  :from from
					 :cli cli
					 :wt delta)))
	(format t " ... ")
	;; add the uc-edge to uc-edgies
	(setf (aref uc-edgies from cli) new-uc-edge)
	;; update num-uc-succs
	(incf (svref (num-uc-succs uc-etc) from))
	;; add entries to uc-succs
	(setf (aref (uc-succs uc-etc) from curr-num-succs) new-uc-edge)
	(format t " *** ")
	;; return the new edge!
	new-uc-edge
	)))))


(defmethod add-uc-edge ((s nu-stnu) from delta cli)
  (let* ((uc-etc (uc-edges-etc s))
	 ;; UC-EDGIES is an N-by-K matrix of UC-EDGE structs
	 (uc-edgies (uc-edges uc-etc))
	 ;; UC-EDG
	 (uc-edg (aref uc-edgies from cli))
	 )
    (cond
     ;; Case 1:  There's already a UC edge from FROM to Act TP of CLI
     (uc-edg
      (when (< delta (uc-edge-wt uc-edg))
	;; Just update the edge weight and we're done!
	(setf (uc-edge-wt uc-edg) delta)))
     ;; Case 2:  No pre-existing UC edge from FROM to Act TP of CLI
     (t
      ;; Create a new UC-EDGE struct and update matrices and vectors...
      (let (;; New UC edge
	    (new-uc-edge (make-uc-edge :from from :cli cli :wt delta)))
	(setf (aref uc-edgies from cli) new-uc-edge)
	(setf (aref (uc-succs uc-etc) from
		    (svref (num-uc-succs uc-etc) from))
	  new-uc-edge)
	(incf (svref (num-uc-succs uc-etc) from)))))
    ;; Also need to add edge to DMAX graph, if nec.
    (when (dmax-edges-etc s)
      (add-edge (dmax-edges-etc s) from delta (cont-link-a
					       (svref (cl-vec s) cli))))))





;; =====================================

;;  INPUTS:
;;    ACT-TP:  tp index for activation time point
;;         X:  min duration of cont link
;;         Y:  max duration of cont link
;;   CONT-TP:  tp index (not CLI) for contingent time point
;;   ADD-ORDINARY-EDGES?:  if T, add the ordinary edges A--y-->C
;;      and C --(-x)--> A.

(defmethod add-cont-link ((s nu-stnu) act-tp x y cont-tp
			  &key (add-ordinary-ub-edge? nil)
			       (add-ordinary-lb-edge? nil))

  (let* ((num (num-tps s))
	 (max-num (length (cl-index-vec s)))
	 (num-links (num-cls s))
	 )
    (cond
     ;; Bad indices
     ((or (< act-tp 0)
	  (< cont-tp 0)
	  (>= act-tp num)
	  (>= cont-tp num)
	  ;; CONT-TP is already associated with some existing cont link
	  (svref (cl-index-vec s) cont-tp))
      (format t "add-cont-link:  bad indices!~%"))
     ;; Good indices... let's go
     (t
      ;;(format t "add-cont-link:  good indices~%")
      (let* ((uc-etc (uc-edges-etc s))
	     (uc-edgies (uc-edges uc-etc))
	     (numcols (num-cols uc-edgies)))
	;; Check whether there's enough room in the existing vectors/matrices
	(when (>= num-links numcols)
	  (expand-uc-edges-etc s max-num (+ num-links *k-incr*)))
	;; (format t "Done expanding:  ~%")
	;; increment the cont-link counter
	(incf (num-cls s))
	;; insert new CONT-LINK struct into CL-VEC
	(setf (svref (cl-vec s) num-links)
	  (make-cont-link :a act-tp
			  :x x
			  :y y
			  :c cont-tp))
	;; specify the CLI index for this contingent link
	(setf (svref (cl-index-vec s) cont-tp) num-links)

	;; ------- add ordinary edges?
	(when add-ordinary-ub-edge?
	  ;; (format t "====> Inserting ORDINARY edges RE: new cont link!~%")
	  (add-edge s act-tp y cont-tp))
	(when add-ordinary-lb-edge?
	  (add-edge s cont-tp (- x) act-tp))

	;; ------- add *original* UC edge
	(let* ((new-uc-edges (uc-edges uc-etc))
	       (new-cli num-links)
	       ;; create the new UC-EDGE struct
	       (new-uc-edge (make-uc-edge :from cont-tp
					  :cli new-cli
					  :wt (- y))))
	  ;; Insert the new UC-EDGE struct into UC-EDGES
	  (setf (aref new-uc-edges cont-tp new-cli) new-uc-edge)
	  ;; There is only one UC-SUCC emanating from C right now
	  (setf (svref (num-uc-succs uc-etc) cont-tp) 1)
	  ;; That UC-EDGE struct is the zeroeth UC-SUCC emanating from C
	  (setf (aref (uc-succs uc-etc) cont-tp 0) new-uc-edge)
	  ;; NOTE:  Don't explicitly add Lower Case edges...
	  ;;  They are represented by the contingent link structs
	  ;; But we DO have to add edge to the DMAX-EDGES-ETC, if it exists
	  (when (dmax-edges-etc s)
	    (add-edge (dmax-edges-etc s) cont-tp (- y) act-tp))
	  ;; Return T just for fun
	  t
	  ))))))

;; =====================================

;;  TESTS




;;  MAP-OVER-PREDS
;; --------------------------
;;  INPUTS:  ETC, an EDGES-ETC object
;;           U, a TP index
;;           FUNKY, a function of two variables:  P -- a predecessor TP
;;             and PU-WT, the weight of the edge from P to U
;;  OUTPUT:  T
;;  SIDE EFFECT:  Applies FUNKY to each (P,WT) where P ----<WT>----> U
;;                is an edge in ETC.

(defmethod map-over-preds ((etc edges-etc) u funky)
  (let ((num-precs (svref (num-preds etc) u))
	(precs (preds etc)))
    (dotimes (i num-precs)
      (let ((edg (aref precs i u)))
	(funcall funky (edge-from edg) (edge-wt edg))))
    ;;
    t))

;;  MAP-OVER-PREDS-ALT
;; -----------------------------------------------------
;;  Same as MAP-OVER-PREDS, except that FUNKY is a
;;  function of three variables:
;;    P     -- a predecessor TP
;;    U     -- the successor TP
;;    PU-WT -- the weight of the edge from P to U

(defmethod map-over-preds-alt ((etc edges-etc) u funky)
  (let ((num-precs (svref (num-preds etc) u))
	(precs (preds etc)))
    (dotimes (i num-precs)
      (let ((edg (aref precs i u)))
	(funcall funky (edge-from edg) u (edge-wt edg))))
    ;;
    t))


;;  MAP-OVER-SUCCS
;; --------------------------
;;  INPUTS:  ETC, an EDGES-ETC object
;;           U, a TP index
;;           FUNKY, a function of two variables:
;;             V  -- a successor TP
;;             WT -- the weight of the edge from U to V
;;  OUTPUT:  T
;;  SIDE EFFECT:  Applies FUNKY to each (V, WT) where U ----<WT>----> V
;;                is an edge in ETC.

(defun map-over-succs (etc u funky)
  (let ((num-sux (svref (num-succs etc) u))
	(sux (succs etc)))
    (dotimes (i num-sux)
      (let ((edg (aref sux u i)))
	(funcall funky (edge-to edg) (edge-wt edg))))
    ;;
    t))

;;  MAP-OVER-SUCCS-ALT
;; ------------------------------------------------------
;;  Same as MAP-OVER-SUCCS, except that FUNKY is
;;  a function of three variables:
;;     U  -- the predecessor TP
;;     V  -- a successor TP, and
;;     WT -- the weight of the edge from U to V

(defun map-over-succs-alt (etc u funky)
  (let ((num-sux (svref (num-succs etc) u))
	(sux (succs etc)))
    (dotimes (i num-sux)
      (let ((edg (aref sux u i)))
	(funcall funky u (edge-to edg) (edge-wt edg))))
    t))

(defmethod map-over-uc-succs ((uc-etc uc-edges-etc) u funky)

  ;; Note: FUNKY is a function of one variable:  a UC-EDGE struct
  ;;   it contains:  FROM, the source time-point, which should be U
  ;;                 CLI, specifies the relevant contingent link (which
  ;;                    determines the destination/successor time-point), and
  ;;                 WT is the weight of the edge

  (let ((num-uc-sux (svref (num-uc-succs uc-etc) u))
	(uc-sux (uc-succs uc-etc)))
    (dotimes (i num-uc-sux)
      (let ((uc-edg (aref uc-sux u i)))
	(funcall funky uc-edg)))
    t))
