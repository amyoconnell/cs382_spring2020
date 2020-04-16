;;  SEPARATE ACTIVATION TIME-POINTS

(defun separate-atps-ctps
    (n tp-names m ord-edges-list k cont-link-veck)
  (let ((orig-n n)
        (atp-vec (make-array (+ n k) :initial-element nil))
        (new-names ()))
    ;; separate the ATPs
    (dotimes (i k)
      (let* ((cont-link (svref cont-link-veck i))
             (atp (first cont-link))
             (already-used? (svref atp-vec atp))
             )
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
            (incf i))))
       (t
        (setf new-names-vec tp-names)))

      (format t "ATP-vec: ~A~%" atp-vec)
      ;; return
      (list n new-names-vec (length ord-edges-list) ord-edges-list k cont-link-veck))))

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

; Boolean procedure determineDC()
(defun determineDC (stnu)
  ;   for each negative node n do
  (dotimes (n *number of negative nodes in stnu*)
    (let ((tp (*function to get the TP*)))
    ;     if DCbackprop(n) = false
    ;       return false;
    (if (not (DCbackprop tp))
      (return-from determineDC false))))
  ;   return true;
  true)

; Boolean procedure DCbackprop(source)
(defun DCbackprop (sourceTP ancestorTP terminatedTP)

;  if ancestor call with same source
  (if (eq sourceTP ancestorTP)
    ;    return false;
    (return-from DCbackprop false))

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
