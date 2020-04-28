;; ====================================
;;  FILE:    morris2006-min-heap.lisp
;;  AUTHOR:  amy o'connell
;; ====================================
;;  This file contains functions needed to run Morris's
;;  2006 DC-checking algorithm on an STNU
;; =========================================================

;; GET-PF
;; ---------------------------------------
;;  INPUT:  STNU: an instance of the NU-STNU class
;;  OUTPUT: a 1D array containing the shortest distance
;;        from a new TP S (not added) to each TP in STNU
;;        (index for each TP determined by STNU's TP-HASH)

(defun get-pf (stnu)
  (let* ((n (num-tps stnu))
        ;; DIST: array containing shortest distance from each TP
        ;; to new source node S (not added to graph)
        ;; initialize distances to 0
         (dist (make-array n :initial-element 0)))
    ; relax edges n-1 times
    (dotimes (j (- n 1)) ; for j=0 to j=n-2 (we don't use j)
             ;; visit each edge (ord and uc successors of each time point):
             (dotimes (u n) ; for each time-point u = 0 to u = n-1
                      ;; visit each ordinary successor edge (u,delta,v):
                      ;; i: index into uth row of STNU's
                      ;;  ordinary successor edge matrix
                      (dotimes (i (aref (num-succs (ord-edges-etc stnu)) u))
                               (let* ( ; EDGIE: successor edge (u,delta,v)
                                 (edgie (aref (succs (ord-edges-etc stnu)) u i))
                                      (v (edge-to edgie))      ; V: TO TP
                                      (delta (edge-wt edgie))) ; DELTA: EDGIE weight
                                ;; update dist(v) if necessary:
                                 ;; if (dist(u) + delta < dist(v))
                                 (if (< (+ (aref dist u) delta) (aref dist v))
                                     ; then set dist(v) to (dist(u) + delta)
                                     (setf (aref dist v) (+ (aref dist u) delta)))))
                      ;; visit each upper-case successor edge (u,C:delta,a)
                      ;; i: index into uth row of STNU's UC successor edge matrix
                      (dotimes (i (aref (num-uc-succs (uc-edges-etc stnu)) u))
                               (let* ( ; UC-EDGIE: uc successor edge (u,C:delta,a)
                                 (uc-edgie (aref (uc-succs (uc-edges-etc stnu)) u i))
                                      (cli (uc-edge-cli uc-edgie)) ; CLI: UC-EDGIE cont-link index
                                      (a (cont-link-a ; A: UC-EDGIE's activation TP
                                        (aref (cl-vec stnu) (uc-edge-cli uc-edgie))))
                                      (delta (uc-edge-wt uc-edgie))) ; DELTA: UC-EDGIE weight
                                 ;; update dist(v) if necessary:
                                 ;; if (dist(u) + delta < dist(a))
                                 (if (< (+ (aref dist u) delta) (aref dist a))
                                     ;; then set dist(a) to (dist(u) + delta)
                                     (setf (aref dist a) (+ (aref dist u) delta)))))))

    ;; do last iteration of bellman-ford:
    ;; visit each edge (ord and uc successors of each time point):
    (dotimes (u n) ; for each time-point u = 0 to n-1
             ;; visit each ordinary successor edge (u,delta,v):
             ;; i: index into uth row of STNU's ord successor edge matrix
             (dotimes (i (aref (num-succs (ord-edges-etc stnu)) u))
                      (let* ( ; EDGIE: successor edge (u,delta,v)
                        (edgie (aref (succs (ord-edges-etc stnu)) u i))
                             (v (edge-to edgie))      ; V: TO TP
                             (delta (edge-wt edgie))) ; DELTA: EDGIE weight
                        ;; update dist(v), if necessary
                        ;; if (dist(u) + delta < dist(v))
                        (when (< (+ (aref dist u) delta) (aref dist v))
                            ;(return-from get-pf nil)
                            ;(format t "bf failed: u=~A u dist=~A v=~A v dist=~A delta=~A~%"
                            ;u (aref dist u) v (aref dist v) delta)
                            ;(format t "dist: ~A~%" dist)
                            (return-from get-pf nil))))
             ;; visit each upper-case successor edge (u,C:delta,a)
             ;; i: index into uth row of STNU's UC successor edge matrix
             (dotimes (i (aref (num-uc-succs (uc-edges-etc stnu)) u))
                      (let* ( ; UC-EDGIE: uc successor edge (u,C:delta,a)
                        (uc-edgie (aref (uc-succs (uc-edges-etc stnu)) u i))
                             (cli (uc-edge-cli uc-edgie)) ; CLI: UC-EDGIE cont-link index
                             (a (cont-link-a  ; A: UC-EDGIE's activation TP
                               (aref (cl-vec stnu) (uc-edge-cli uc-edgie))))
                             (delta (uc-edge-wt uc-edgie))) ; DELTA: UC-EDGIE weight
                        ;; update dist(v) if necessary
                        ;; if (dist(u) + delta < dist(a))
                        (when (< (+ (aref dist u) delta) (aref dist a))
                            ;; then set dist(a) to (dist(u) + delta)
                            ;(format t "bf failed: u=~A u dist=~A a=~A a dist=~A delta=~A~%"
                            ; u (aref dist u) a (aref dist a) delta)
                            (return-from get-pf nil)))))
    ;; return distance array
    dist))


;; ADD-ORD-EDGES
;; ---------------------------------------
;;  INPUT:  EDGES-ACC: list of lists: (from wt to)
;;          STNU: instance of STNU
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies STNU by addng
;;               an ORD-EDGE to STNU for every list
;;               (FROM WT TO) in EDGES-ACC

(defun add-ord-edges (edges-acc stnu)
;(print 191)
  ;; for each ord-edge list (from wt to) in edges-acc
  (dolist (edgie edges-acc)
    ;(format t "~A~%" edgie)
    ;; add ordinary edge to STNU's ORD-EDGES-ETC struct
    ;; (format t "~A~%" edgie)
    (add-ord-edge (ord-edges-etc stnu)
    ;; from, to, wt
    (first edgie) (third edgie) (second edgie))))

;; ADD-UC-EDGES
;; ---------------------------------------
;;  INPUT:  EDGES-ACC: list of lists: (from wt to)
;;          STNU: instance of STNU
;;  OUTPUT: meh
;;  SIDE EFFECT: destructively modifies STNU by addng
;;               a UC-EDGE to STNU for every list
;;               (FROM WT CLI) in EDGES-ACC

(defun add-uc-edges (edges-acc stnu)
;; for each uc-edge list (from wt cli) in edges-acc
  (dolist (edgie edges-acc)
    ;; add uc edge to STNU's UC-EDGES-ETC struct]
    (add-edge-uc (uc-edges-etc stnu)
    ;; from, wt, cli
    (first edgie) (second edgie) (third edgie)))
  )

;; IS-DC
;; ---------------------------------------
;; implementation of Morris' 2006 DC-checking algorithm
;; ---------------------------------------
;;  INPUT:  STNU: an instance of the NU-STNU class
;;  OUTPUT: NIL if STNU is not dynamically controllable
;;          T otherwise

(defun is-dc (stnu)

  ;; for each contingent link in STNU (k = number of cl's in STNU)
  (dotimes (i (num-cls stnu)) ; for i=0 to i=k-1 (we don't use i)
          ;  (print-stnu stnu)
           (let* (
                  (counter 0)
                  ;; initialize accumulators for ordinary and upper-case edges
                  ;; generated by the LowerCase or CrossCase rules
                  (ord-edge-acc '()) ; accumulator for ordinary edges
                  (uc-edge-acc '())  ; accumulator for uc edges



                  ;; GET-PF: runs bellman-ford on OU-graph
                  ;; (graph consisting of only ordinary and upper-case edges):
                  ;; PF: potential function: array of distances from new source
                  ;;  node S to each TP in STNU
                  (pf (get-pf stnu)))
              ;; if PF is nil, there is a negative loop in STNU - stop
             (when (not pf)
             ;(format t "negative loops. not dc. this is so sad.~%")
             (return-from is-dc nil))
             ;; else we're good to keep going
             ;(format t "no negative loops woot woot~%")
             ;(format t "potential function: ~A~%" pf) ; debugging

             ;; INNER LOOP: loop for each contingent link in STNU
             (dotimes (j (num-cls stnu)) ; for j=0 to j=k-1
                      ; let (a,xx,yy,c) be the jth contingent link
                      (let* ( ; curr-cl: cont-link struct representing jth cl in STNU
                             (curr-cl (aref (cl-vec stnu) j))
                             (c (cont-link-c curr-cl))   ; c: contingent TP
                             (xx (cont-link-x curr-cl))  ; xx: lower-case edge weight
                             (a (cont-link-a curr-cl))  ; a: activatin TP

                             ;; propagate forward from c along allowable paths
                             ;; initialize priority queue
                             (queue (make-mbh (num-tps stnu)))

                             ;; all-keys: keeps track of shortest real path length
                             ;; from C to each TP
                             (all-keys (make-array (num-tps stnu) :initial-element +infinity+))
                             )

                             ;; insert c in priority queue with key=0
                             (mbh-insert queue c 0)
                             (setf (aref all-keys c) 0)

                        ; (format t "1 queue: ~A~%" queue) ; debugging
                        ; (format t "current cl: ~A~%" (list c a)) ; debugging
                         ;(format t "ord-edge-acc: ~A~%" ord-edge-acc)
                        ; (format t "uc-edge-acc: ~A~%" uc-edge-acc)
                        ;; while queue is not empty
                        (while (and (not (mbh-empty? queue))
                                    (< counter (num-tps stnu)))
                              ;(format t "queue: ")
                              ; (print-mbh queue t nil) ; debugging
                               ;(format t "all-keys: ~A~%" all-keys)
                              ;;(incf counter)
                               (let* (
                                      ;; extract min node (tp, key) from priority queue
                                      (min-key-pair (mbh-extract-min-pair queue))

                                      ;; x = tp popped from queue
                                      (x (first min-key-pair))
                                      ;; key = non-negative path length from c to x
                                      (key (second min-key-pair))

                                      ;; rpl1: real path length from c to x:
                                      ;; rpl = -f(c) + key(x) + f(x)
                                      (rpl1 (+ (- 0 (aref pf c)) key (aref pf x))))

                                 (cond
                                   ;; if real path length from c to x < 0
                                   ((< rpl1 0)
                                    ;; accumulate an ordinary edge and stop propagating
                                    ;; ord-edge-acc += (a, xx+rpl, x)
                                    ;(format t "RPL < 0: accumulate ord edge ")
                                    (setf ord-edge-acc (cons (list a (+ xx rpl1) x) ord-edge-acc))
                                    ;(format t "~A~%" (first ord-edge-acc))
                                    )
                                  ;; if real path length from c to x >= 0
                                   (t
                                    ;; Propagate forward along successor edges of X:
                                    ;; for each ordinary successor edge (x,delta,q)
                                    ;; SUCC-I: index into Xth row of STNU's ordinary successor edge matrix
                                    (dotimes (succ-i (aref (num-succs (ord-edges-etc stnu)) x))
                                             (let* (;; SUCC-EDGE: SUCC-Ith successor edge of X
                                                    ;; c -=-=-=-=-=-> x -----[delta]----> q
                                                    (succ-edge (aref (succs (ord-edges-etc stnu)) x succ-i))
                                                    (q (edge-to succ-edge))     ; Q: SUCC-EDGE's to TP
                                                    (delta (edge-wt succ-edge)) ; DELTA: SUCC-EDGE weight
                                                    ;; NN-DELTA: non-neg. value of SUCC-EDGE
                                                    ;; nn-delta := f(x) + delta - f(q)
                                                    (nn-delta (- (+ (aref pf x) delta) (aref pf q))))
                                                    ;(format t "processing ~A~%" (list x delta q))
                                               ;; see if key(q) needs updating
                                               ;; if (key(x) + nn-delta < key(q))
                                               (when (< (+ key nn-delta) (aref all-keys q))
                                                    ;(format t "resetting key of ~A from ~A to ~A~%"
                                                    ;  q (aref all-keys q) (+ key nn-delta))
                                                    (setf (aref all-keys q) (+ key nn-delta))
                                                    ;; if (key(x) + nn-delta < key(q)), update Q's
                                                    ;; current key to (key(x) + nn-delta)
                                                    ;; if Q is not in QUEUE, insert it with key (key(x) + nn-delta)
                                                    (mbh-insert-or-decrease-key queue q (+ key nn-delta))
                                                     )))

                                    ; (format t "4 queue: ~A~%" queue) ; debugging

                                    ;; Propagate forward along UC successor edges of x:
                                    ;; for each upper-case successor edge (x,CC:delta,aa)
                                    ;; SUCC-I: index into Xth row of STNU's UC successor edge matrix
                                    (dotimes (succ-i (aref (num-uc-succs (uc-edges-etc stnu)) x))
                                             (let* ( ;; SUCC-EDGE: SUCC-Ith successor edge of X
                                                    ;; c -=-=-=-=-=-> x -----[CC:delta]----> aa
                                                    ;; contingent link for CC:  (aa,min,max,cc)
                                                    (succ-edge (aref (uc-succs (uc-edges-etc stnu)) x succ-i))
                                                    (delta (uc-edge-wt succ-edge))  ; DELTA: SUCC-EDGE weight
                                                    ;; CLI: cont-link index of (aa,min,max,cc)
                                                    (cli (uc-edge-cli succ-edge))
                                                    ;; CL: cont-link struct representing (aa,min,max,cc)
                                                    (cl (aref (cl-vec stnu) cli))
                                                    ;; AA: activation TP for SUCC-EDGE
                                                    (aa (cont-link-a cl))
                                                    ;; MIN: lower-case edge weight for CC's contingent link
                                                    (min (cont-link-x cl))
                                                    ;; NN-DELTA: (f(x) + delta - f(aa)) (f = potential function)
                                                    (nn-delta (- (+ (aref pf x) delta) (aref pf aa)))
                                                    ;; NN-PATH: non-neg path length from c to x to aa
                                                    (nn-path (+ key nn-delta))
                                                    ;; RPL2: real path length from c to x to aa
                                                    ;; rpl2 = (-f(c) + nn-path + f(aa)))
                                                    (rpl2 (+ (- 0 (aref pf c)) nn-path (aref pf aa))))

                                              ;(format t "processing ~A~%" (list x delta aa))

                                               ;; if rpl2 >= -min, where min is minimum duration of (aa,min,max,cc)
                                               (cond ((>= rpl2 (- 0 min))
                                                   ;; then path from c to x to aa would have an
                                                   ;; upper-case label that would be removed by the
                                                   ;; label-removal rule, so treat it like an ordinary
                                                   ;; path from c to x to aa:

                                                   ;; then see if key(aa) needs updating
                                                   ;(format t "resetting key of ~A from ~A to ~A~%"
                                                    ; aa (aref all-keys aa) (+ nn-path))

                                                   (mbh-insert-or-decrease-key queue aa nn-path))

                                                   ;; else: rpl2 < -min < 0
                                                   ;; so generate an upper-case edge from a to aa
                                                   ;; from pseudocode: uc-edge-acc += (a, CC:xx+rpl, aa)
                                                   ;; we use cli instead of aa to make uc-add-edge happy
                                                   (t
                                                     (if (not (eq a aa))
                                                   (setf uc-edge-acc
                                                     (cons (list a cli (+ min rpl2)) uc-edge-acc)))))
                                                   ))
                                                   ))
                                 ))   ;; End of WHILE
                        ))  ;; End of for j = 0 to k-1

             ;; Add all edges obtained by processing all of the contingent links
             (add-ord-edges ord-edge-acc stnu)
             (add-uc-edges uc-edge-acc stnu)

             )) ;; End of for i = 1 to k

  ;; Run Bellman Ford one last time to check consistency of OU-graph
  (let ((pf (get-pf stnu)))
    ;; if GET-PF (Bellman Ford) returns NIL
    ;; we have a negative loop: return NIL
    ;(format t "is-dc: ~A~%" pf)
    (return-from is-dc pf)

    ; (if (not pf)
    ; (format t "negatlive loops. not dc. this is so sad.~%")
    ; ;; else: no negative loops! Return TRUE
    ; (format t "no negative loops. is dc. woot woot~%"))
    )


  ) ;; The End
