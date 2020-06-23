;; ====================================
;;  FILE:    parser.lisp
;;  AUTHOR:  amy o'connell
;; ====================================
;;  This file contains functions needed to parse a .stnu file
;;  into an instance of the NU-STNU class
;;  for morris's 2014 DC-checking algorithm
;; =========================================================


;;  STRING-TO-LIST
;; ---------------------------------------
;;  INPUT:  S, a string
;;  OUTPUT:  a list containing space-separated
;;    items in the string

(defun string-to-list (s)
  (with-input-from-string (in s)
    (loop for x = (read in nil nil) while x collect x)))

;;  MAKE-INDEX-HASH
;; ---------------------------------------
;;  helper function to map TP names from .stnu file to array indexes
;; ---------------------------------------
;;  INPUT:  STNU, an STNU instance
;;          LISTY, a list of symbols representing TP names
;;  OUTPUT:  who cares!
;;  SIDE EFFECT: destructively modifies tp-hash of STNU
;;     to map each element in LISTY to an index

(defun make-index-hash (stnu listy)
  (let ((i 0)) ; iterator
    ;; for each item in listy
    (dolist (item listy)
    ;; insert item in hashy with key i
            (setf (gethash item (tp-hash stnu)) i)
            ;; put item at index i in tp-names-vec
            (setf (aref (tp-names-vec stnu) i) item)
            ;; increment i
            (incf i))))

;;  MAKE-ORD-EDGE - wraper function to make an edge struct
;; ---------------------------------------
;;  INPUT:  FROM, origin TP symbol/name (NOT index)
;;          TO, destination TP
;;          WT, edge weight
;;          SUCC-I, index into the FROMth row of SUCCS matrix in EDGES-ETC
;;          PRED-I, index into the TOth col of PREDS matrix in EDGES-ETC
;;  OUTPUT: edge struct with values set to INPUTS
;;  SIDE EFFECT: none

(defun make-ord-edge (from to wt succ-i pred-i)
  (make-edge :from from :to to :wt wt :succ-index succ-i :pred-index pred-i))

;;  ADD-ORD-EDGE
;; ---------------------------------------
;;  helper function to add an edge to EDGES-ETC
;; ---------------------------------------
;;  INPUT:  ETC: instance of EDGES-ETC
;;          FROM: origin TP
;;          TO: destination TP
;;          WT: edge weight
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies ETC to contain
;;               a new edge (FROM WT TO) **ONLY if there is
;;               not already an edge between FROM and TO
;;               with a lower weight than WT**

(defun add-ord-edge (etc from to wt)
  ;; if there's already an edge (from to)
  (if (aref (edges etc) from to)
  (let* ( ;; EDGIE: current edge between FROM and TO
          (edgie (aref (edges etc) from to))
          ;; SUCC-I: succ-i of current edge
          (succ-i (edge-succ-index edgie))
          ;; PRED-I: pred-i of current edge
          (pred-i (edge-pred-index edgie))
          ;; EDGIE-WEIGHT: weight of current edge
          (edgie-weight (edge-wt edgie))
          ;; NEW-EDGE: new edge struct (FROM WT TO)
          ;; same succ-i and pred-i as current edge (FROM TO)
          (new-edge (make-ord-edge from to wt succ-i pred-i)))

          ;; if new edge weight is less than current edge weight
          ;; replace current edge with NEW-EDGE
          (when (< wt edgie-weight)
          ; add NEW-EDGE to EDGES matrix in ETC
          (setf (aref (edges etc) from to) new-edge)
          ; replace EDGIE with NEW-EDGE as successor of FROM
          (setf (aref (succs etc) from succ-i) new-edge)
          ; replace EDGIE with NEW-EDGE as predecessor of TO
          (setf (aref (preds etc) to pred-i) new-edge)))

  ;; else: there is not already an edge between FROM and TO
  ;; make new edge (FROM WT TO) and insert
  (let* (; SUCC-I: NUM-SUCCS for FROM
         (succ-i (aref (num-succs etc) from))
         ; PRED-I: NUM-PREDS for TO
         (pred-i (aref (num-preds etc) to))
         ; make new edge (FROM WT TO) with SUCC-I and PRED-I
         (new-edge (make-ord-edge from to wt succ-i pred-i)))

    ; add NEW-EDGE to EDGES matrix in ETC
    (setf (aref (edges etc) from to) new-edge)
    ; add NEW-EDGE as successor of FROM
    (setf (aref (succs etc) from succ-i) new-edge)
    ; increment FROM's number of successors
    (incf (aref (num-succs etc) from))
    ; add NEW-EDGE as predecessor of TO
    (setf (aref (preds etc) to pred-i) new-edge)
    ; increment TO's number of predecessors
    (incf (aref (num-preds etc) to))))) ; END add-ord-edge

;;  ORD-EDGES-HELPER
;; ---------------------------------------
;;  helper function to add edges from .stnu to NU-STNU instance
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;          INPUT: .stnu stream
;;  OUTPUT: meh
;;  SIDE EFFECT: produces new EDGES-ETC instance ETC with all ordinary edges
;;    in .stnu file, destructively sets ORD-EDGES-ETC field of STNU to ETC

(defun ord-edges-helper (input stnu)
  (let* ((etc (make-edges-etc (num-tps stnu))) ; ETC = new EDGES-ETC struct
        ;; other variables for use in dotimes
         (line nil)
         (line-list nil)
         (from-i nil)
         (to-i nil))

    ; iterate through each ORD-EDGE in .stnu
    (dotimes (n (num-ord-edges stnu))
             ; LINE = next line in .stnu as string (ex: "A 5 C")
             (setf line (remove #\' (read-line input nil)))
             ; LINE-LIST = LINE converted to list (ex: '(A 5 C))
             (setf line-list (string-to-list line))
             ; FROM-I, TO-I = TP indexes for FROM and TO TPs
             (setf from-i (gethash (first line-list) (tp-hash stnu)))
             (setf to-i (gethash (third line-list) (tp-hash stnu)))

             ; use wrapper to add new edge to ETC
             (add-ord-edge etc
                           from-i to-i (second line-list)))

    ; all ord-edges have been added to ETC
    ; set ORD-EDGES-ETC of STNU to ETC
    (setf (ord-edges-etc STNU) etc)))

;;  ADD-EDGE-UC
;; ------------------------------------------------
;;  helper function to add an edge to UC-EDGES-ETC
;; ------------------------------------------------
;;  INPUT:  ETC: instance of UC-EDGES-ETC
;;          FROM: origin TP
;;          CLI: destination TP
;;          WT: edge weight
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies ETC to contain a new edge (FROM WT TO)

(defun add-edge-uc (uc-etc from cli wt)
  (let* (; SUCC-I: NUM-SUCCS for FROM
         (succ-i (aref (num-uc-succs uc-etc) from))
         ; make new UC-edge
         (new-uc-edge (make-uc-edge :from from :cli cli :wt wt)))
    ; add to EDGES matrix in ETC
    (setf (aref (uc-edges uc-etc) from cli) new-uc-edge)
    ; add as successor of FROM
    (setf (aref (uc-succs uc-etc) from succ-i) new-uc-edge)
    ;; increment FROM's number of UC successors
    (incf (aref (num-uc-succs uc-etc) from))
    ))

;;  CONT-LINKS-HELPER
;; ---------------------------------------
;;  helper function to add cont. links from .stnu to NU-STNU instance
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;          INPUT: .stnu stream
;;  OUTPUT: meh
;;  SIDE EFFECT: produces new vector of K CONT-LINK structs, destructively sets
;;    STNU's CL-VEC to new vector.
;;    Produces new vector of length NUM-TPS with CL-indexes stored at the index
;;    of contingent time point C for each CL, sets STNU's CL-INDEX-VEC.
;;    Produces new UC-EDGES-ETC instance ETC with all UC edges from contingent
;;    links in .stnu file, destructively sets UC-EDGES-ETC of STNU to ETC

(defun cont-links-helper (input stnu)
  (let* (; UC-ETC = new UC-EDGES-ETC instance
    (uc-etc (make-uc-edges-etc (num-tps stnu) (num-cls stnu)))
        ; set during dotimes
         (line nil)
         (line-list nil)
         (a-i nil)
         (c-i nil))
    ; iterate through each CL in .stnu
    (dotimes (cli (num-cls stnu))  ; CLI = list iterator, contingent link index
             ; LINE = next line in .stnu as string (ex: "A 1 3 C")
             (setf line (remove #\' (read-line input nil)))
             ; LINE-LIST = LINE converted to list (ex: '(A 1 3 C))
             (setf line-list (string-to-list line))
             ; A-I, C-I = TP indexes for activation and contingent TPs
             (setf a-i (gethash (first line-list) (tp-hash stnu)))
             (setf c-i (gethash (fourth line-list) (tp-hash stnu)))
             ; make cont-link struct, add to STNU's CL-VEC at index CLI
             (setf (aref (cl-vec stnu) cli)
                   (make-cont-link :a a-i :x (second line-list) :y (third line-list) :c c-i))
             ; store CLI for new cont-link at index C-I in STNU's CL-INDEX-VEC
             (setf (aref (cl-index-vec stnu) c-i) cli)
             ; use wrapper to add new uc-edge to UC-ETC
             (add-edge-uc uc-etc
                          c-i    ; initial FROM TP is contingent TP C
                          cli    ; list iterator CLI == current cl index
                          (* -1 (third line-list)))) ; uc-weight is negative!!
       ; change UC-EDGES-ETC of STNU from NIL to ETC
       (setf (uc-edges-etc STNU) uc-etc)))

;;  PARSE-FILE
;; ---------------------------------------
;;  function to parse a .stnu file into a NU-STNU instance
;; ---------------------------------------
;;  INPUT:  DOC: .stnu file name
;;  OUTPUT: STNU instance matching .stnu FILE

(defun parse-file (doc)
  (let* ((input (open doc)) ; INPUT: .stnu input stream
         (stnu (make-instance 'nu-stnu))) ; STNU: empty STNU instance

    ;; iterate through each line of input in .stnu file
    (when input
      (loop for line = (read-line input nil)
            while line do
            (cond
              ;; Case 1: kind of network:
              ;; only working with STNU's - don't do anything
              ((string-equal line "# Kind of Network")
               (read-line input nil))
              ;; Case 2: num TPs
              ((string-equal line "# Num Time-Points")
              ;; set NUM-TPS field in STNU to int on next line
               (setf (num-tps stnu) (parse-integer (read-line input nil)))
               ;; set TP-NAMES-VEC and CL-INDEX-VEC fields of STNU to
               ;; NIL vecors of length NUM-TPS
               (setf (tp-names-vec stnu) (make-array (num-tps stnu)))
               (setf (cl-index-vec stnu) (make-array (num-tps stnu))))
               ;; Case 3: num ord edges
              ((string-equal line "# Num Ordinary Edges")
              ;; set NUM-ORD-EDGES field of stnu to int on next line
               (setf (num-ord-edges stnu) (parse-integer (read-line input nil))))
               ;; Case 4: num cont links
              ((string-equal line "# Num Contingent Links")
              ;; set NUM-CLS field of stnu to int on next line
               (setf (num-cls stnu) (parse-integer (read-line input nil)))
               ;; set CL-VEC field of STNU to NIL vecor of length NUM-CLS
               (setf (cl-vec stnu) (make-array (num-cls stnu))))
               ;; Case 5: time point names
              ((string-equal line "# Time-Point Names")
              ;; use MAKE-INDEX-HASH function to map TP names to
              ;; integer indexes in TP-HASH field of STNU
               (make-index-hash stnu (string-to-list
                 ;; remove any single quotes from around TP Names!!
                 (remove #\' (read-line input nil)))))
              ;; Case 6: ordinary edges
              ((string-equal line "# Ordinary Edges")
              ;; use ORD-EDGES-HELPER function to add ordinary edges to STNU
               (ord-edges-helper input stnu))
              ;; Case 7: contingent links
              ((string-equal line "# Contingent Links")
                ;; use CONT-LINKS-HELPER function to add cont links to STNU
               (cont-links-helper input stnu))
              ) ; end COND
              ) ; end LOOP over DOC
      ;; close .stnu stream
      (close input)) ; end WHEN INPUT

      ;; return new STNU instance
      stnu
)) ; THE END (parse-file)

;; Printing Functions -- self explanatory

(defun print-list (list)
  (while list
         (print (char-int (car list)))
         (setq list (cdr list))))

(defun print-stnu (stnu)
    (format t "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STNU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%")
    (format t "NUM TPS: ~A~%" (num-tps stnu))
    (format t "TP-HASH: ~A~%" (tp-hash stnu))
    (format t "NUM CLS: ~A~%" (num-cls stnu))
    (format t "CL VEC: ~A~%" (cl-vec stnu))
    (format t "CL INDEX VEC ~A~%" (cl-index-vec stnu))
    (format t "NUM ORD EDGES: ~A~%" (num-ord-edges stnu))
    (format t "ORD EDGES ETC: ~%")
    (print-edges-etc (ord-edges-etc stnu))
    (format t "DMAX EDGES ETC: ~%")
    (format t "UC EDGES ETC: ~%")
    (print-uc-edges-etc (uc-edges-etc stnu))
    (format t "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END STNU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%"))

(defun print-stnu-to-file (stnu file)
    (format file "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STNU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%")
    (format file "NUM TPS: ~A~%" (num-tps stnu))
    (format file "TP-HASH: ~A~%" (tp-hash stnu))
    (format file "NUM CLS: ~A~%" (num-cls stnu))
    (format file "CL VEC: ~A~%" (cl-vec stnu))
    (format file "CL INDEX VEC ~A~%" (cl-index-vec stnu))
    (format file "NUM ORD EDGES: ~A~%" (num-ord-edges stnu))
    (format file "ORD EDGES ETC: ~%")
    (print-edges-etc-to-file (ord-edges-etc stnu) file)
    (format file "DMAX EDGES ETC: ~%")
    (format file "UC EDGES ETC: ~%")
    (print-uc-edges-etc-to-file (uc-edges-etc stnu) file)
    (format file "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END STNU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%"))

(defun print-edges-etc (etc)
        (format t "%%% edges: ~A~%" (edges etc))
        (format t "%%% num-succs: ~A~%" (num-succs etc))
        (format t "%%% succs: ~A~%" (succs etc))
        (format t "%%% num-preds ~A~%" (num-preds etc))
        (format t "%%% preds ~A~%" (preds etc))
        (format t "%%% dist ~A~%" (dist etc)))

(defun print-edges-etc-to-file (etc file)
        (format file "%%% edges: ~A~%" (edges etc))
        (format file "%%% num-succs: ~A~%" (num-succs etc))
        (format file "%%% succs: ~A~%" (succs etc))
        (format file "%%% num-preds ~A~%" (num-preds etc))
        (format file "%%% preds ~A~%" (preds etc))
        (format file "%%% dist ~A~%" (dist etc)))

(defun print-uc-edges-etc (etc)
        (format t "%%% uc-edges: ~A~%" (uc-edges etc))
        (format t "%%% num-uc-succs: ~A~%" (num-uc-succs etc))
        (format t "%%% uc-succs: ~A~%" (uc-succs etc)))

(defun print-uc-edges-etc-to-file (etc file)
        (format file "%%% uc-edges: ~A~%" (uc-edges etc))
        (format file "%%% num-uc-succs: ~A~%" (num-uc-succs etc))
        (format file "%%% uc-succs: ~A~%" (uc-succs etc)))

(defun print-cl-to-file (cl stnu file)
  (let* ((a (aref (tp-names-vec stnu) (cont-link-a cl)))
          (x (cont-link-x cl))
          (y (cont-link-y cl))
          (c (aref (tp-names-vec stnu) (cont-link-c cl))))
      (format file " (~A ~A ~A ~A)" a x y c)))

(defun print-ord-edge-to-file (edgie stnu file)
  (let ((from (aref (tp-names-vec stnu) (edge-from edgie)))
        (to (aref (tp-names-vec stnu) (edge-to edgie)))
        (weight (edge-wt edgie)))
  (format file " (~A ~A ~A)"
    from weight to)))

(defun print-edges-from-matrix (etc stnu file)
  (dotimes (i (num-tps stnu))
    (dotimes(j (num-tps stnu))
      (let ((edgie (aref (edges etc) i j)))
        (if edgie
          (print-ord-edge-to-file edgie stnu file))))))

(defun print-stnu-short (stnu file)
    (format file "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STNU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%")
    (format file "Time Points:")
    (dotimes (tpi (num-tps stnu))
      (format file " ~A" (aref (tp-names-vec stnu) tpi)))
    (format file "~%Contingent Links:")
    (dotimes (cli (num-cls stnu))
      (print-cl-to-file (aref (cl-vec stnu) cli) stnu file))
    (format file "~%Edges:")
    (print-edges-from-matrix (ord-edges-etc stnu) stnu file)
    (format file "~%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END STNU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%"))
