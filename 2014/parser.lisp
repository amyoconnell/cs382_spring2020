;; ====================================
;;  FILE:    parser.lisp
;;  AUTHOR:  amy o'connell
;; ====================================
;;  This file contains functions needed to parse a .stnu file
;;  into an instance of the NU-STNU class
;;  for morris's 2014 DC-checking algorithm
;; =========================================================

;;  SEPARATE ACTIVATION TIME-POINTS

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
    ;; separate the ATPs
    (dotimes (i k)
             (let* ((cont-link (svref cont-link-veck i))
                    (atp (first cont-link))
                    (already-used? (svref atp-vec atp)))
               (cond
                 ;; Case 1:  That TP is an ATP for a previously encountered cont link
                 (already-used?
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
                  ;; record that fact in ATP-VEC
                  (setf (svref atp-vec atp) i)))))

    ;; Separate the CONT-TPs and ACT-TPs
    (dotimes (i k)
             (let* ((cont-link (svref cont-link-veck i))
                    (ctp (fourth cont-link))
                    (is-an-atp? (svref atp-vec ctp)))
               (when is-an-atp?
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

    ;; Create new-names-vec
    (let (new-names-vec)
      (cond
        ((> n orig-n)
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
         (setf new-names-vec tp-names)))

      (setf (cl-index-vec stnu) atp-vec)
      (setf (num-tps stnu) n)
      (setf (tp-names-vec stnu) new-names-vec)
      (setf (num-ord-edges stnu) (length ord-edges-list))
      (setf (negative-tp-vec stnu) (make-array n))
      (setf (tp-status-vec stnu) (make-array n))
      (setf (ord-edges stnu) (make-array (list n n)))
      ;; TODO: implement this
      (add-ord-edges stnu ord-edges-list)


)))


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

;;  ADD-ORD-EDGE
;; ---------------------------------------
;;  helper function to add an edge to EDGES-ETC
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;          FROM: origin TP
;;          TO: destination TP
;;          WT: edge weight
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies ETC to contain a new edge (FROM WT TO)

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
        (incf (aref (num-preds stnu) to)))))

;;  ORD-EDGES-HELPER
;; ---------------------------------------
;;  helper function to add edges from .stnu to NU-STNU instance
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;          INPUT: .stnu stream
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively sets ORD-EDGES matrix of STNU
;;    to contains ordinary edges listed in .stnu file,

(defun ord-edges-helper (input stnu)
  (let* ((line nil)
         (line-list nil)
         (from-i nil)
         (to-i nil))
    (dotimes (n (num-ord-edges stnu)) ; iterate through each ORD-EDGE in .stnu
             ; LINE = next line in .stnu as string (ex: "A 5 C")
             (setf line (read-line input nil))
             ; LINE-LIST = LINE converted to list (ex: '(A 5 C))
             (setf line-list (string-to-list line))
             ; FROM-I, TO-I = TP indexes for FROM and TO TPs
             (setf from-i (gethash (first line-list) (tp-hash stnu)))
             (setf to-i (gethash (third line-list) (tp-hash stnu)))
             ; use wrapper to add new edge to ETC
             (add-ord-edge stnu from-i to-i (second line-list)))))

;;  PARSE-ORD-EDGES
;; ---------------------------------------
;;  helper function to collect edges from .stnu in a list
;; ---------------------------------------
;;  INPUT:  STNU: instance of NU-STNU
;;          INPUT: .stnu stream
;;  OUTPUT: a list of lists representing ordinary edges (FROM WT TO)

(defun parse-ord-edges (stnu input)
  (let ((ord-edge-list nil))
  (dotimes (i (num-ord-edges stnu)) ; iterate through each ORD-EDGE in .stnu
  (let* (
        ; LINE = next line in .stnu as string (ex: "A 5 C")
        (line (read-line input nil))
        ; LINE-LIST = LINE converted to list (ex: '(A 5 C))
         (line-list (string-to-list line))
         ; FROM-I, TO-I = TP indexes for FROM and TO TPs
         (from-i (gethash (first line-list) (tp-hash stnu)))
         (to-i (gethash (third line-list) (tp-hash stnu))))
             ; use wrapper to add new edge to list
             (cons
               (list from-i (second line-list) to-i)
               ord-edge-list)))
    ord-edge-list))

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
    (if (< 0 (second edgie))
      (setf (aref (negative-tp-vec stnu) to) t))))

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
             (setf line (read-line input nil))
             ; LINE-LIST = LINE converted to list (ex: '(A 1 3 C))
             (setf line-list (string-to-list line))
             ; A-I, C-I = TP indexes for activation and contingent TPs
             (setf a-i (gethash (first line-list) (tp-hash stnu)))
             (setf c-i (gethash (fourth line-list) (tp-hash stnu)))
             ; make cont-link struct, add to STNU's CL-VEC at index CLI
             (setf (aref (cl-vec stnu) cli)
                   (list a-i (second line-list) (third line-list) c-i))
             ; store CLI for new cont-link at index C-I in STNU's CL-INDEX-VEC
             (setf (aref (cl-index-vec stnu) c-i) cli))))

;;  PARSE-FILE
;; ---------------------------------------
;;  function to parse a .stnu file into a NU-STNU instance
;; ---------------------------------------
;;  INPUT:  ETC: instance of EDGES-ETC
;;          FROM: origin TP
;;          CLI: destination TP
;;          WT: edge weight
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies ETC to contain a new edge (FROM WT TO)
;
; (let* ((n (num-tps stnu))
;       (orig-n n)
;       (tp-names (tp-names-vec))
;       (m (num-ord-edges stnu))
;       (k (num-cls stnu))
;       (cont-link-veck (cl-vec stnu))
;       (atp-vec (make-array (+ n k) :initial-element nil))
;       (new-names ()))

(defun parse-file (doc)
  (let* ((input (open doc))
         (stnu (make-instance 'nu-stnu))
         (ord-edges-list nil))
    ; (num-tps 0)
    ; (init-max-n 0)
    ; (init-max-k 0)
    ; (ord-edges nil)
    ; (uc-edges nil))
    ;; iterate through each line of input
    (when input
      (loop for line = (read-line input nil)
            while line do
            (cond
              ;; Case 1: kind of network:
              ;; only working with STNU's - don't do anything
              ((string-equal line "# Kind of Network")
               (read-line input nil))
              ;; Case 2: num TPS
              ((string-equal line "# Num Time-Points")
               (setf (num-tps stnu) (parse-integer (read-line input nil)))
               (setf (tp-names-vec stnu) (make-array (num-tps stnu)))
               (setf (negative-tp-vec stnu) (make-array (num-tps stnu)))
               (setf (tp-status-vec stnu) (make-array (num-tps stnu)))
               (setf (cl-index-vec stnu) (make-array (num-tps stnu)))
               (setf (ord-edges stnu) (make-array (list (num-tps stnu) (num-tps stnu))))
               (setf (num-preds stnu)
                     (make-array (num-tps stnu) :initial-element 0))
               (setf (preds stnu) (make-array (list (num-tps stnu) (num-tps stnu))))
               )
              ((string-equal line "# Num Ordinary Edges")
               ;(format t "num ord edges~%")
               (setf (num-ord-edges stnu) (parse-integer (read-line input nil)))
               )
              ((string-equal line "# Num Contingent Links")
               ;(format t "num cls~%")
               (setf (num-cls stnu) (parse-integer (read-line input nil)))
               (setf (cl-vec stnu) (make-array (num-cls stnu)))
               )
              ((string-equal line "# Time-Point Names")
               ;(format t "tp names~%")
               (make-index-hash stnu (string-to-list (read-line input nil)))
               )
              ((string-equal line "# Ordinary Edges")
               ;(format t "ord edges~%")
               (setf ord-edges-list (parse-ord-edges stnu input))
               )
              ((string-equal line "# Contingent Links")
               ;(format t "cls~%")
               (cont-links-helper input stnu)
               )
              ))
      (close input))
    (separate-atps-ctps stnu ord-edges-list)
    stnu))
