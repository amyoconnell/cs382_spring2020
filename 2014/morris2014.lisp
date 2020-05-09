;; ====================================
;;  FILE:    morris2014.lisp
;;  AUTHOR:  amy o'connell
;; ====================================
;;  This file contains functions needed to run Morris's
;;  2014 DC-checking algorithm on an STNU
;; =========================================================

;; UNSUITABLE
;; ---------------------------------------
;;  INPUT:  SOURCE-TP: time point
;;          U: time point
;;          EDGIE: predecessor edge of U
;;          STNU: an instance of the NU-STNU class
;;  OUTPUT: T if edgie is "unsuitable"

(defun unsuitable (source-tp u edgie stnu)
  (let (; CLI: cont-link index of CL with contingent TP SOURCE-TP
        (cli (aref (cl-index-vec stnu) source-tp)))

    ;; >> if SOURCE-TP is not a contingent TP, CLI will be NIL <<
    ;; if SOURCE-TP is a contingent TP
    (if cli
        (let (; CL: cont-link with contingent TP SOURCE-TP
              (cl (aref (cl-vec stnu) cli)))
          ;; if U = original contingent TP of CL
          ;; and SOURCE-TP is FROM TP of EDGIE
          ;; return T, else return NIL
          (and (eq u (cont-link-c cl))
               (eq source-tp (edge-from edgie)))))))

;; DETERMINE-DC
;; ---------------------------------------
;; implementation of Morris' 2014 DC-checking algorithm
;; ---------------------------------------
;;  INPUT:  STNU: an instance of the NU-STNU class
;;  OUTPUT: NIL if STNU is not dynamically controllable
;;          T otherwise

(defun determine-dc (stnu)
  ;   for each TP in STNU
  (dotimes (n (num-tps stnu)) ; for n=0 to n=num tps in stnu
           ;; if TP N is a negative TP
           (when (aref (negative-tp-vec stnu) n)
             ;; if DC-BACKPROP n returns NIL, return NIL
             (if (not (dc-backprop n stnu))
                 (return-from determine-dc nil))))

  ; no negative TP forms a negative loop: return T
  t)

;; DC-BACKPROP
;; ---------------------------------------
;;  INPUT: SOURCE-TP: negative TP
;;         STNU: an instance of the NU-STNU class
;;  OUTPUT: NIL if backpropagation forms a negative loop

(defun dc-backprop (source-tp stnu)
  (let
      ((source-status (aref (tp-status-vec stnu) source-tp)))

    ;;  if ancestor call with same SOURCE-TP
    (if (eq source-status *ancestor*)
        ;    return NIL
        (return-from dc-backprop nil))

    ;;  if prior terminated call with SOURCE-TP
    (when (eq source-status *terminated*)
      ;   return NIL
      (return-from dc-backprop t)))

  ;; SOURCE-TP is now an ancestor TP for all future calls
  (setf (aref (tp-status-vec stnu) source-tp) *ancestor*)

  ;; DISTANCE: array of n elements
  ;;  contains distance from SOURCE-TP for each TP in STNU
  (let ((distance (make-array (num-tps stnu) :initial-element *infinity*))
        ;; QUEUE: min heap priority queue
        (queue (make-mbh (num-tps stnu))))

    ;; set DISTANCE[SOURCE-TP] to 0
    (setf (aref distance source-tp) 0)

    ;; for each predecessor edge to SOURCE-TP E1
    ;; E1-INDEX = index into SOURCE-TPth row of STNU's PREDS matrix
    (dotimes (e1-index (aref (num-preds stnu) source-tp))
             (let* (; E1: predecessor edge (N1 wt SOURCE-TP)
                    (e1 (aref (preds stnu) source-tp e1-index))
                    ; N1: from TP of E1
                    (n1 (edge-from e1)))

               ;; set DISTANCE[N1] to E1 weight
               (setf (aref distance n1) (edge-wt e1))

               ;; insert TP N1 into QUEUE with priority E1 weight
               (mbh-insert queue n1 (edge-wt e1))))

    ;; while queue not empty do
    (while (not (mbh-empty? queue))
           ;; pop TP U from queue;
           (let ((u (mbh-extract-min queue)))
             (cond
               ;;   if DISTANCE[U] >= 0
               ((>= (aref distance u) 0)
                ;; add edge (U, DISTANCE[U], SOURCE-TP) to STNU
                (add-ord-edge stnu u source-tp (aref distance u)))

               ;; else: DISTANCE[U] < 0
               (t
                ;;   if U is a negative TP
                (if (aref (negative-tp-vec stnu) u)
                    ;; if (DC-BACKPROP U) = NIL
                    (if (not (dc-backprop u stnu))
                        ;; return NIL
                        (return-from dc-backprop nil)))

                ;; for each predecessor edge E of U
                ;; PRED-I = index into Uth row of STNU's PREDS matrix
                (dotimes (pred-i (aref (num-preds stnu) u))
                         (let (; E: predecessor edge of U
                               (e (aref (preds stnu) u pred-i)))
                           (cond
                             ;; if E's weight < 0 -> continue (do nothing)
                             ((< (edge-wt e) 0))

                             ;; if E is unsuitable -> continue (do nothing)
                             ((unsuitable source-tp u e stnu))

                             ;; else: edge E has a positive weight and is suitable
                             (t
                              (let ((v (edge-from e)) ; V: edge E's FROM TP
                                    ;; NEW: DISTANCE[U] + edge E's weight
                                    (new (+ (aref distance u) (edge-wt e))))
                                ;; if new < DISTANCE[V]
                                (when (< new (aref distance v))
                                  ;; reset DISTANCE[V] TO NEW
                                  (setf (aref distance v) new)
                                  ;; insert TP V into queue with priority NEW
                                  (mbh-insert queue v new)
                                  )))))))) ; end cond
             )) ; end while loop

    ;; we made it! call to SOURCE-TP is about to terminate
    ;; change status from ancestor to terminated
    (setf (aref (tp-status-vec stnu) source-tp) *terminated*)

    ;; return T
    t )) ;; the end (dc-backprop)
