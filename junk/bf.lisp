(defconstant *inf* 1000)
(defconstant *test-vertices* '(0 1 2))
(defconstant *test-edges* (list (list 0 5 1) (list 1 3 2) (list 2 -9 0)))


(defun bellman-ford (vertices edges source)
  (let ((distance (make-array (list (length vertices))))
        (pred (make-array (list (length vertices)))))
    ;; Step 1: initialize graph
    ;; for each vertex v in vertices:
    (dolist (v vertices)
            (setf (aref distance v) *inf*)
            (setf (aref pred v) nil))
    ;; dist[source] = 0
    (setf (aref distance source) 0)

    ;; Step 2: relax edges repeatedly
    ;; for i from 1 to size(vertices)-1:
    (dotimes (i (- (length vertices) 1))
             ;; for each edge (u, v) with weight w in edges:
             (dolist (e edges)
                     (let ((u (car e)) (v (nth 2 e)) (w (nth 1 e)))
                       ;; if distance[u] + w < distance[v]:
                       ; (format t "u: ~a v: ~a w: ~a u+w: ~a v: ~a~%"
                       ; u v w  (+ (aref distance u) w) (aref distance v))
                       (cond ((< (+ (aref distance u) w) (aref distance v))
                              ;; distance[v] := distance[u] + w
                              (setf (aref distance v) (+ (aref distance u) w))
                              ;; predecessor[v] := u
                              (setf (aref pred v) u))))))

    ;; Step 3: check for negative-weight cycles
    ;; for each edge (u, v) with weight w in edges:
    (dolist (e edges)
            (let ((u (car e)) (v (nth 2 e)) (w (nth 1 e)))
              ;; if distance[u] + w < distance[v]:
              (cond ((< (+ (aref distance u) w) (aref distance v))
                     (format t "Contains negative cycle~%")
                     (return nil)))))))

(bellman-ford *test-vertices* *test-edges* 0)
