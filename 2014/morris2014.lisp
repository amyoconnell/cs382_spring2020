;; original pseudocode from 2014 paper
;;-------------------------------------------
; Boolean procedure determineDC()
;   for each negative node n do
;     if DCbackprop(n) = false
;       return false;
;   return true;
; end procedure

; Boolean procedure DCbackprop(source)
;  if ancestor call with same source
;    return false;
;  if prior terminated call with source
;    return true;
;  distance(source) = 0;
;  for each node x other than source do
;    distance(x) = infinity;
;  PriorityQueue queue = empty;
;  for each e1 in InEdges(source) do
;     Node n1 = start(e1);
;     distance(n1) = weight(e1);
;     insert n1 in queue;
; while queue not empty do
;   pop Node u from queue;
;   if distance(u) >= 0
;     Edge e’ = new Edge(u, source);
;       weight(e’) = distance(u);
;       add e’ to graph;
;       continue;
;   if u is negative node
;     if DCbackprop(u) = false
;      return false;
;   for each e in InEdges(u) do
;     if weight(e) < 0
;       continue;
;     if e is unsuitable
;       continue;
;     Node v = start(e);
;     new = distance(u) + weight(e);
;     if new < distance(v)
;       distance(v) = new;
;       insert v into queue;
; return true;
; end procedure
;;-------------------------------------------


(defun unsuitable (source-tp u edgie stnu)
  (let ((cli (aref (cl-index-vec stnu) source-tp)))
    (if cli
      (let ((cl (aref (cl-vec stnu) cli)))
        (and (eq u (cont-link-c cl))
            (eq source-tp (edge-from edgie)))))))

;; in lisp:


; Boolean procedure determineDC()
(defun determine-dc (stnu)
  ;   for each negative node n do
  (dotimes (n (num-tps stnu))
    (when (aref (negative-tp-vec stnu) n)
             ;     if DCbackprop(n) = false
             ;       return false;
             (if (not (dc-backprop n stnu))
                 (return-from determine-dc nil)
                 ;(setf (aref (tp-status-vec stnu) n) *terminated*)
                 )))
  ;   return true;
  t)

; Boolean procedure DCbackprop(source)
(defun dc-backprop (source-tp stnu)
  (format t "source-tp: ~A~%" (aref (tp-names-vec stnu) source-tp))

  (let
      ((source-status (aref (tp-status-vec stnu) source-tp)))
    ;  if ancestor call with same source
    (if (eq source-status *ancestor*)
        ;    return false;
        (return-from dc-backprop nil))

    ;  if prior terminated call with source
    (when (eq source-status *terminated*)
        (format t "terminated source-tp: ~A~%" source-tp)
        ;    return true;
        (return-from dc-backprop t)))

  (setf (aref (tp-status-vec stnu) source-tp) *ancestor*)

  ;  for each node x other than source do
  ;    distance(x) = infinity;
  (let ((distance (make-array (num-tps stnu) :initial-element *infinity*))
        ;  PriorityQueue queue = empty;
        (queue (make-mbh (num-tps stnu))))
    ;  distance(source) = 0;
    (setf (aref distance source-tp) 0)

    ;  for each e1 in InEdges(source) do
    (dotimes (e1-index (aref (num-preds stnu) source-tp))
             (let* ((e1 (aref (preds stnu) source-tp e1-index))
                    ;     Node n1 = start(e1);
                    (n1 (edge-from e1)))
               ;     distance(n1) = weight(e1);
               (setf (aref distance n1) (edge-wt e1))
               (format t "e1 index: ~A e1: ~A~% distance vec: ~A~%"
                  e1-index e1 distance)
               ;     insert n1 in queue;
               ;; %%%%%%%%%%%% WHERE DO WE GET PRIORITY FROM??? %%%%%%%%%%%
               (mbh-insert queue n1 0)
               (format t "adding ~A to the queue~%"
                  (aref (tp-names-vec stnu) n1))
               ))

    ; while queue not empty do
    (while (not (mbh-empty? queue))
           ;   pop Node u from queue;
           (let ((u (mbh-extract-min queue)))

           ;(format t "queue: ~A~%" queue)

           (format t "popped TP ~A off queue~%"
              (aref (tp-names-vec stnu) u))

             (cond
               ;   if distance(u) >= 0
               ((>= (aref distance u) 0)
                ;     Edge e’ = new Edge(u, source);
                ;       weight(e’) = distance(u);
                ;       add e’ to graph;
                ;       continue;
                (add-ord-edge stnu u source-tp (aref distance u))

                (format t "added ordinary edge (~A ~A ~A)~%"
                    (aref (tp-names-vec stnu) u)
                    (aref distance u)
                    (aref (tp-names-vec stnu) source-tp))

                )
              (t
               ;   if u is negative node
               (if (aref (negative-tp-vec stnu) u)
                ;     if DCbackprop(u) = false
                (if (not (dc-backprop u stnu))
                    ;      return false;
                    (return-from dc-backprop nil)))
               ;(t
                ;   for each e in InEdges(u) do
                (dotimes (pred-i (aref (num-preds stnu) u))
                         (let ((e (aref (preds stnu) u pred-i)))
                           (cond
                             ;     if weight(e) < 0
                             ;       continue;
                             ((< (edge-wt e) 0))
                             ;     if e is unsuitable
                             ;       continue;
                             ((unsuitable source-tp u e stnu)

                              (format t "unsuitable edge: ~A-->~A-->~A~%"
                                  (aref (tp-names-vec stnu) (edge-from e))
                                  (aref (tp-names-vec stnu) u)
                                    (aref (tp-names-vec stnu) source-tp))

                                )
                             ;; %%%%%%%%%%%%% what does unsuitable mean?? %%%%%%%%%%%%
                             (t
                              ;     Node v = start(e);
                              (let ((v (edge-from e))
                                    ;     new = distance(u) + weight(e);
                                    (new (+ (aref distance u) (edge-wt e))))

                                (format t "~% u: ~A v: ~A new = ~A distance = ~A~%"
                                  (aref (tp-names-vec stnu) u)
                                  (aref (tp-names-vec stnu) v)
                                  new (aref distance v))

                                ;     if new < distance(v)
                                (when (< new (aref distance v))
                                  ;       distance(v) = new;
                                  (setf (aref distance v) new)
                                  ;       insert v into queue;
                                  (mbh-insert queue v new)

                                  (format t "u-v edge")
                                  (format t "inserting ~A into queue~%"
                                    (aref (tp-names-vec stnu) v))

                                  )))))))) ; end cond
                                  )) ;end while loop

    (format t "terminated: ~A~%" source-tp)
    (setf (aref (tp-status-vec stnu) source-tp) *terminated*)
    ; return true;
    t )) ;; the end
