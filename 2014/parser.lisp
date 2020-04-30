;; ====================================
;;  FILE:    parser.lisp
;;  AUTHOR:  amy o'connell
;; ====================================
;;  This file contains functions needed to parse a .stnu file
;;  into an instance of the NU-STNU class
;;  for morris's 2014 DC-checking algorithm
;; =========================================================

;;  SEPARATE-ATPS-CTPS
;; ---------------------------------------
;;  separate activation timepoints
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;          ORD-EDGES-LIST: list of ordinary edges (FROM WT TO)
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies STNU to reflect
;;      updated STNU graph with separated activation timepoints
;;      - necessary for Morris 2014 algorithm to work

(defun separate-atps-ctps
  ;(n tp-names m ord-edges-list k cont-link-veck)
  (stnu ord-edges-list)
  (let* ((n (num-tps stnu))
        (orig-n n)
        (tp-names (tp-names-vec stnu))
        (m (num-ord-edges stnu))
        (k (num-cls stnu))
        (cont-link-veck (cl-vec stnu))
        (atp-vec (make-array (+ n k) :initial-element nil))
        (new-names ()))
    (format t "32~%")
    ;; separate the ATPs
    (dotimes (i k)
             (let* ((cont-link (svref cont-link-veck i))
                    (atp (first cont-link))
                    (already-used? (svref atp-vec atp))
                    )


                    (format t "atp: ~A~%" atp)

                    (format t "39~%")

               (cond
                 ;; Case 1:  That TP is an ATP for a previously encountered cont link
                 (already-used?

                   (format t "41~%")

                  ;; set the ATP for this cont-link to a new time-point with index n
                  (setf (svref cont-link-veck i)
                        (cons n (rest cont-link)))
                  (setf (svref atp-vec n) i)
                  ;; create name for N
                  (push (format nil "[~A]" n) new-names)
                  ;; constrain ATP and N to occur simultaneously
                  (push (list n 0 atp) ord-edges-list)
                  (push (list atp 0 n) ord-edges-list)
                  ;; increment N
                  (incf n))
                 ;; Case 2:  That TP is available for use as ATP for current cont link
                 (t

                   (format t "58~%")

                  ;; record that fact in ATP-VEC
                  (setf (svref atp-vec atp) i)))))

    ;; Separate the CONT-TPs and ACT-TPs
    (dotimes (i k)
             (let* ((cont-link (svref cont-link-veck i))
                    (ctp (fourth cont-link))
                    (is-an-atp? (svref atp-vec ctp)))
               (when is-an-atp?

                 (format t "70~%")

                 ;; Note:  IS-AN-ATP? equals the index to the other cont link
                 (let* ((other-cont-link (svref cont-link-veck is-an-atp?)))
                   ;; Set the ATP for other-cont-link to be a new time-point with index n
                   (setf (svref cont-link-veck is-an-atp?)
                         (cons n (rest other-cont-link)))
                   ;; Indicate that N is the ATP for other-cont-link
                   (setf (svref atp-vec n) is-an-atp?)
                   ;; Indicate that CTP is no longer an activation time-point
                   (setf (svref atp-vec ctp) nil)
                   ;; Constrain CTP and N to occur simultaneously
                   (push (list ctp 0 n) ord-edges-list)
                   (push (list n 0 ctp) ord-edges-list)
                   ;; Create new name for n
                   (push (format nil "[~A]" n) new-names)
                   ;; Increment n
                   (incf n)))))

     (format t "89~%")

    ;; Create new-names-vec
    (let (new-names-vec)
      (cond
        ((> n orig-n)

             (format t "103~%")

         (setf new-names-vec (make-array n))
         (dotimes (i orig-n)
                  (setf (svref new-names-vec i) (svref tp-names i)))
         (let ((i orig-n))
           (dolist (elt (reverse new-names))
                   (setf (svref new-names-vec i) elt)
                   ;; add new TP to hash table of TP indexes
                   (setf (gethash elt (tp-hash stnu)) i)
                   (incf i))))
        (t

               (format t "116~%")

         (setf new-names-vec tp-names)))


              (format t "121~%")


      ;; update necessary fields in STNU
      (setf (cl-index-vec stnu) atp-vec)
      (setf (num-tps stnu) n)
      (setf (tp-names-vec stnu) new-names-vec)
      (setf (num-ord-edges stnu) (length ord-edges-list))


           (format t "131~%")


      (setf ord-edges-list (make-cl-ord-edges stnu ord-edges-list))


           (format t "137~%")


      (cl-list-to-struct stnu)
      (format t "ord-edges-list: ~A~%" ord-edges-list)

      ;; initialize rest of fields with proper NUM-TPS
      (setf (negative-tp-vec stnu) (make-array n))
      (setf (tp-status-vec stnu) (make-array n))
      (setf (ord-edges stnu) (make-array (list n n)))
      (setf (num-preds stnu) (make-array n :initial-element 0))
      (setf (preds stnu) (make-array (list n n)))

      ;; add edges from updated ord-edges-list to STNU
      (add-ord-edges stnu ord-edges-list)



)))

(defun make-cl-ord-edges (stnu ord-edges-list)
  (dotimes (cli (num-cls stnu))
    (let* ((cl (aref (cl-vec stnu) cli))
          (uc-edge (list (fourth cl)
            (* -1 (third cl)) (first cl)))
            (lc-edge (list (first cl)
              (second cl) (fourth cl))))
      (format t "uc-edge ~A~%" uc-edge)
      (setf ord-edges-list
        (cons lc-edge (cons uc-edge ord-edges-list)))))
        ord-edges-list)



;;  STRING-TO-LIST
;; ---------------------------------------
;;  INPUT:  S, a string
;;  OUTPUT:  a list containing space-separated
;;    items in the string

; (defun string-to-list (s)
;   (format t "stl~%")
;   (with-input-from-string (in s)
;     (format t "142~%")
;     (loop for x = (read in nil nil) while x collect x))
;     (format t "done with stl~%")
;     )

(defun string-to-list (s)		 ; (defun string-to-list (s)
   (with-input-from-string (in s)		 ;   (format t "stl~%")
     (loop for x = (read in nil nil) while x collect x)))

; (defun string-to-list (string)
;     "Returns a list of substrings of string
; divided by ONE space each.
; Note: Two consecutive spaces will be seen as
; if there were an empty string between them."
;     (loop for i = 0 then (1+ j)
;           as j = (position #\Space string :start i)
;           collect (subseq string i j)
;           while j))

; (defun splitty-acc
;     (strng acc)
;   (let ((str (string-trim " " strng))
;         )
;     (cond
;      ((string= "" str)
;       acc)
;      (t
;       (let ((i 0)
;             (len (length str)))
;         (while (and (< i len)
;                     (not (eq (aref str i) #\Space)))
;           (incf i))
; 	(splitty-acc (subseq str i) (cons (read-from-string (subseq str 0 i)) acc)))))))
;
; (defun string-to-list
;     (str)
;   (splitty-acc str ()))

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
  (format t "make-index-hash started~%")
  (let ((i 0)) ; iterator
    ;; for each item in listy
    (dolist (item listy)

            (format t "item: ~A~%" item)

            ;; insert item in hashy with key i
            (setf (gethash item (tp-hash stnu)) i)
            ;; put item at index i in tp-names-vec
            (setf (aref (tp-names-vec stnu) i) item)
            ;; increment i
            (incf i))))

;;  ADD-ORD-EDGE
;; ---------------------------------------
;;  helper function to add an edge to STNU
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;          FROM: origin TP
;;          TO: destination TP
;;          WT: edge weight
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies STNU to contain a new edge (FROM WT TO)

(defun add-ord-edge (stnu from to wt)
  ;; if there's already an edge (from to)
  (if (aref (ord-edges stnu) from to)
      (let* (
             (edgie (aref (ord-edges stnu) from to))
             (pred-i (edge-pred-index edgie))
             (edgie-weight (edge-wt edgie))
             (new-edge (make-ord-edge from to wt pred-i)))
        (when (< wt edgie-weight)
          (setf (aref (ord-edges stnu) from to) new-edge)
          (setf (aref (preds stnu) to pred-i) new-edge)))
      ;(format t "edge already exists from ~A to ~A~%" from to)
      (let* (; pred index = NUM-PREDS for TO
             (pred-i (aref (num-preds stnu) to))
             ; make new edge
             (new-edge (make-ord-edge from to wt pred-i)))
        ; add to EDGES matrix
        (setf (aref (ord-edges stnu) from to) new-edge)
        ; add as predecessor of TO
        (setf (aref (preds stnu) to pred-i) new-edge)
        ; increment TO's number of predecessors
        (incf (aref (num-preds stnu) to))
        )))

;;  PARSE-ORD-EDGES
;; ---------------------------------------
;;  helper function to collect edges from .stnu in a list
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;          INPUT: .stnu stream
;;  OUTPUT: a list of lists representing ordinary edges (FROM WT TO)

(defun parse-ord-edges (stnu input)
  (let ((ord-edges-list nil))
  (dotimes (i (num-ord-edges stnu)) ; iterate through each ORD-EDGE in .stnu
  (let* (
        ; LINE = next line in .stnu as string (ex: "A 5 C")
        (line (remove #\' (read-line input nil)))
        ; LINE-LIST = LINE converted to list (ex: '(A 5 C))
         (line-list (string-to-list line))
         ; FROM-I, TO-I = TP indexes for FROM and TO TPs
         (from-i (gethash (first line-list) (tp-hash stnu)))
         (to-i (gethash (third line-list) (tp-hash stnu))))
             ; use wrapper to add new edge to list
             (setf ord-edges-list (cons
               (list from-i (second line-list) to-i)
               ord-edges-list))))
    ord-edges-list))

;;  ADD-ORD-EDGES
;; ---------------------------------------
;;  helper function to add edges from list to NU-STNU instance
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;          ORD-EDGES-LIST: list of edges (FROM WT TO)
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies ORD-EDGES field of STNU
;;      to contain an N by N matrix of EDGE structs
;;      destructively modifies NEGATIVE-TP-VEC of STNU to
;;      contain t at all indices of negative TPS

(defun add-ord-edges (stnu ord-edges-list)
  (dolist (edgie ord-edges-list)
    ;; add edge to stnu matrix
    (add-ord-edge stnu (first edgie) (third edgie) (second edgie))
    ;; if negative, set TO TP to negative in negative TP vec
    (when (> 0 (second edgie))
      (format t "negative edge: ~A~%" edgie)
      (setf (aref (negative-tp-vec stnu) (third edgie)) t))
      ))

;;  CL-LIST-TO-STRUCT
;; ---------------------------------------
;;  helper function that converts an STNU's array of
;;  cl lists (a x y c) to cont-link structs
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies CL-VEC field of STNU
;;      to contain a vector of K CONT-LINK structs

(defun cl-list-to-struct (stnu)
  (dotimes (i (num-cls stnu))
    (let ((cl-list (aref (cl-vec stnu) i)))
      (setf (aref (cl-vec stnu) i)
        (make-cont-link
          :a (first cl-list)
          :x (second cl-list)
          :y (third cl-list)
          :c (fourth cl-list))))))

;;  CONT-LINKS-HELPER
;; ---------------------------------------
;;  helper function to add cont. links from .stnu to NU-STNU instance
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;          INPUT: .stnu stream
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies STNU's CL-VEC to contain
;;    K cont-link structs. Destructifly modifies STNU's cl-index-vec
;;    by storing each CL's CL-VEC index at CL-INDEX-VEC index C

(defun cont-links-helper (input stnu)
  (let* ((line nil)
         (line-list nil)
         (a-i nil)
         (c-i nil))
    ; iterate through each CL in .stnu
    (dotimes (cli (num-cls stnu))  ; CLI = list iterator, contingent link index
             ; LINE = next line in .stnu as string (ex: "A 1 3 C")
             (setf line (remove #\' (read-line input nil)))
             (format t "301~%")
             ; LINE-LIST = LINE converted to list (ex: '(A 1 3 C))
             (setf line-list (string-to-list line))

             (format t "cl list: ~A~%" line-list)

             (format t "does ~A equal ~A?: ~A~%"
                (first line-list) (aref (tp-names-vec stnu) 2)
                (eql (first line-list) (aref (tp-names-vec stnu) 2)))
                ;
                ; (format t (first line-list))
                ; (format t (aref (tp-names-vec stnu) 2))

             ; A-I, C-I = TP indexes for activation and contingent TPs
             (format t "301~%")
             (setf a-i (gethash (first line-list) (tp-hash stnu)))

             (format t "a-i: ~A~%" a-i)

             (setf c-i (gethash (fourth line-list) (tp-hash stnu)))

             (format t "c-i: ~A~%" c-i)

             ; make cont-link struct, add to STNU's CL-VEC at index CLI
             (format t "301~%")
             (setf (aref (cl-vec stnu) cli)
                   (list a-i (second line-list) (third line-list) c-i))
             ; store CLI for new cont-link at index C-I in STNU's CL-INDEX-VEC
             (format t "313~%")
             )))

;;  PARSE-FILE
;; ---------------------------------------
;;  function to parse a .stnu file into a NU-STNU instance
;; ---------------------------------------
;;  INPUT:  DOC: .stnu file name
;;  OUTPUT: STNU instance matching .stnu FILE
;;    with separated activation time points

(defun parse-file (doc)
  (let* ((input (open doc))
         (stnu (make-instance 'nu-stnu))
         (ord-edges-list nil))
    ;; iterate through each line of input
    (when input
      (loop for line = (read-line input nil)
            while line do
            (format t line)
            (format t "~%")
            (cond
              ;; Case 1: kind of network:
              ;; only working with STNU's - don't do anything
              ((string-equal line "# Kind of Network")
               (read-line input nil))
              ;; Case 2: num TPS
              ((string-equal line "# Num Time-Points")
                ;; NUM-TPS might change - can't initialize most fields yet
               (setf (num-tps stnu) (parse-integer (read-line input nil)))
               (setf (tp-names-vec stnu) (make-array (num-tps stnu)))
               )
              ((string-equal line "# Num Ordinary Edges")
               (setf (num-ord-edges stnu) (parse-integer (read-line input nil)))
               )
              ((string-equal line "# Num Contingent Links")
               (setf (num-cls stnu) (parse-integer (read-line input nil)))
               (setf (cl-vec stnu) (make-array (num-cls stnu)))
               )
              ((string-equal line "# Time-Point Names")
               ;; add current TPs to hash table, save names in TP-names-vec
               ;; will add new TPs as they are created
               ; (make-index-hash stnu (string-to-list (read-line input nil)))
               (make-index-hash stnu (string-to-list
                 (remove #\' (read-line input nil))))
               (format t "index-hash: ~A~%" (tp-hash stnu))
               )
              ((string-equal line "# Ordinary Edges")
                ;; parse edges into a list of (FROM WT TO) - save for later
               (setf ord-edges-list (parse-ord-edges stnu input))
               )
              ((string-equal line "# Contingent Links")
              ;; add vector of cont-link structs to STNU
               (cont-links-helper input stnu)
               )))
      (format t "365~%")
      (close input))
    ;; convert current STNU to format suitable for morris2014
    ;; (separate activation timepoints with multiple cont. links)
    ;; (will also finish initiating all other fields with updated NUM-TPS)
    (format t "370~%")
    (separate-atps-ctps stnu ord-edges-list)
    (format t "372~%")

    stnu)) ;; The End (parse-file function)

;;------------------------
;; Print functions
;;------------------------

(defun print-ord-edge-to-file (edgie stnu file)
  (let ((from (aref (tp-names-vec stnu) (edge-from edgie)))
        (to (aref (tp-names-vec stnu) (edge-to edgie)))
        (weight (edge-wt edgie)))
  (format file " (~A ~A ~A)"
    from weight to)))

(defun print-edges-from-matrix (edges stnu file)
  (dotimes (i (num-tps stnu))
    (dotimes(j (num-tps stnu))
      (let ((edgie (aref edges i j)))
        (if edgie
          (print-ord-edge-to-file edgie stnu file))))))

(defun print-cl-to-file (cl stnu file)
  (let* ((a (aref (tp-names-vec stnu) (cont-link-a cl)))
          (x (cont-link-x cl))
          (y (cont-link-y cl))
          (c (aref (tp-names-vec stnu) (cont-link-c cl))))
      (format file " (~A ~A ~A ~A)" a x y c)))

(defun print-stnu-short (stnu file)
    (format file "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STNU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%")
    (format file "Time Points:")
    (dotimes (tpi (num-tps stnu))
      (format file " ~A" (aref (tp-names-vec stnu) tpi)))
    (format file "~%Contingent Links:")
    (dotimes (cli (num-cls stnu))
      (print-cl-to-file (aref (cl-vec stnu) cli) stnu file))
    (format file "~%Edges:")
    ;(format file " ~A" (ord-edges stnu))
    (print-edges-from-matrix (ord-edges stnu) stnu file)
    ;(dotimes (edge-i (num-ord-edges stnu))
      ;(print-ord-edge-to-file (aref
    (format file "~%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END STNU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%"))
