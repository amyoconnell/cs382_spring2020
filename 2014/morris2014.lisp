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
; (defun determineDC (stnu)
;   ;   for each negative node n do
;   (dotimes (n *number of negative nodes in stnu*)
;            (let ((tp (*function to get the TP*)))
;              ;     if DCbackprop(n) = false
;              ;       return false;
;              (if (not (DCbackprop tp))
;                  (return-from determineDC false))))
;   ;   return true;
;   true)
;
; ; Boolean procedure DCbackprop(source)
; (defun DCbackprop (sourceTP ancestorTP terminatedTP)
;
;   ;  if ancestor call with same source
;   (if (eq sourceTP ancestorTP)
;       ;    return false;
;       (return-from DCbackprop false))

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
