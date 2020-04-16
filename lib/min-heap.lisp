;; ===================================
;;  new-heaps.lisp
;; ===================================

;;  Functions for min-binary-heaps (mbh)
;; ---------------------------------------------
;;  (MAKE-MBH <max-entries>)
;;  (MBH-CLEAR <heap>) ==> clears HEAP for re-use
;;  (PRINT-MBH <heap> <str> <depth>)
;;  (MBH-DECREASE-KEY <heap> <tp> <new-key>)
;;     ==> sets key for TP in HEAP to be NEW-KEY
;;  (MBH-INSERT <heap> <tp> <new-key>)
;;     ==> inserts TP into HEAP with key NEW-KEY
;;  (MBH-EXTRACT-MIN <heap>) ==> pops elt with min key from HEAP
;;  (MBH-GET-KEY-FOR <heap> <tp>) ==> gets key for given TP in HEAP
;;  (MBH-GET-STATUS <heap> <tp>)
;;     ==> one of {*not-yet-in-heap*, *in-heap*, *already-popped*}
;;


;;  Use of heaps in LUKE-DC:  Unfortunately, despite the name MAKE-FIB,
;;  the code in LUKE-DC does *not* use Fibonacci heaps...  :(
;;
;;;  make-fib    <-- create a new heap
;;;  empty-heap  <-- clear heap
;;;  add-to-heap <-- insert
;;;  pop-heap    <-- extract min
;;;  decrease-key
;;;    node-item
;;;
;;;  make-fib-item: priority, data
;;;    fib-item-priority
;;;    fib-item-data
;;;
;;;  VECK-OF-NODES -- cleared before entering main loop,
;;;     new nodes put into veck-of-nodes when added to heap,
;;;     used to determine whether a given tp has an entry in heap,
;;;  ALREADY-POPPED? -- cleared as above, set when node popped from
;;;     cue, used to determine that node already processed.

(defconstant *min-binary-heap* 1)
(defconstant *fib-heap* 2)

(defstruct heap-funcs
  (init-func #'mbh-init)
  (clear-func #'mbh-clear)
  (insert-func #'mbh-insert)
  (extract-min-func #'mbh-extract-min)
  (decrease-key-func #'mbh-decrease-key)
  (get-key-for-func #'mbh-get-key-for)
  (get-status-func #'mbh-get-status))

(defstruct (min-bin-heap (:conc-name mbh-) (:print-function print-mbh))
  ;; data
  max-num-items
  num-items-in-heap
  tps->keys      ;; index = tp, val = key for that tp
  tps->statuses  ;; index = tp, val = status for that tp
  tps->locs ;; index = tp, val = location in heap
  locs->tps ;; index = loc in heap, val = tp in that location
  )


;;; ---------------------------

(defconstant *not-yet-in-heap* 0)
(defconstant *in-heap* 1)
(defconstant *already-popped* 2)

(defun mbh-init (heap max-num-nodes)
  (setf (mbh-max-num-items heap) max-num-nodes)
  (setf (mbh-num-items-in-heap heap) 0)
  ;; create vectors of data
  (setf (mbh-tps->keys heap)
        (make-array max-num-nodes :initial-element nil
                    :adjustable nil
                    ))
  (setf (mbh-tps->statuses heap)
        (make-array max-num-nodes
                    :initial-element *not-yet-in-heap*
                    :adjustable nil
                    ))
  (setf (mbh-locs->tps heap)
        (make-array max-num-nodes :initial-element nil
                    :adjustable nil
                    ))
  (setf (mbh-tps->locs heap)
        (make-array max-num-nodes :initial-element nil
                    :adjustable nil
                    ))
  (setf (mbh-locs->tps heap)
        (make-array max-num-nodes :initial-element nil
                    :adjustable nil
                    ))
  t)

(defun mbh-clear (heap)
  (let ((num (mbh-max-num-items heap))
        (keys (mbh-tps->keys heap))
        (statuses (mbh-tps->statuses heap))
        (locs->tps (mbh-locs->tps heap))
        (tps->locs (mbh-tps->locs heap))
        )
    (dotimes (i num)
             (setf (svref keys i) nil)
             (setf (svref statuses i) *not-yet-in-heap*)
             (setf (svref locs->tps i) nil)
             (setf (svref tps->locs i) nil))
    (setf (mbh-num-items-in-heap heap) 0)
    t))

;; Amy's function!!
;; MBH-EMPTY?
;; ---------------------------------------
;;  INPUT:  min-bin-heap struct: an MBH
;;  OUTPUT: T if the MBH contains no elements
;;          NIL otherwise

(defun mbh-empty? (heap)
  (eq 0 (mbh-num-items-in-heap heap)))


;;;      0
;;;    1   2
;;;   3 4 5 6

(defmacro bh-parent (indy)
  `(ash (1- ,indy) -1))

(defmacro bh-left-child (indy)
  `(1+ (ash ,indy 1)))

(defmacro bh-right-child (indy)
  `(ash (1+ ,indy) 1))

(defmacro bh-swap! (locs->tps tps->locs loc-i loc-j)
  `(let ((old-tp-at-i (svref ,locs->tps ,loc-i))
         (old-tp-at-j (svref ,locs->tps ,loc-j)))
     (setf (svref ,tps->locs old-tp-at-i) ,loc-j)
     (setf (svref ,tps->locs old-tp-at-j) ,loc-i)
     (setf (svref ,locs->tps ,loc-i) old-tp-at-j)
     (setf (svref ,locs->tps ,loc-j) old-tp-at-i)
     t))

(defun min-heapify (locs->tps tps->locs keys init-loc num-items-in-heap)
  (do* ((loc init-loc largest-loc)
        (left-loc (bh-left-child loc) (bh-left-child loc))
        (right-loc (bh-right-child loc) (bh-right-child loc))
        (largest-loc loc loc))
       ;; EXIT CONDITION
       (nil)
       ;; BODY
       (when (and (< left-loc num-items-in-heap)
                  (< (svref keys (svref locs->tps left-loc))
                     (svref keys (svref locs->tps largest-loc))))
         (setf largest-loc left-loc))
       (when (and (< right-loc num-items-in-heap)
                  (< (svref keys (svref locs->tps right-loc))
                     (svref keys (svref locs->tps largest-loc))))
         (setf largest-loc right-loc))
       ;; *REAL* exit condition:
       (when (= largest-loc loc) (return-from min-heapify))
       ;; Otherwise...
       (bh-swap! locs->tps tps->locs loc largest-loc))
  t)


(defun mbh-decrease-keyster (heap loc new-key)
  (let* ((locs->tps (mbh-locs->tps heap))
         (tps->locs (mbh-tps->locs heap))
         (keys (mbh-tps->keys heap))
         (my-parent-loc (bh-parent loc)))
    ;;(format t "mbh-decrease-key: ")
    (setf (svref keys (svref locs->tps loc)) new-key)
    (while (and (> loc 0)
                (> (svref keys (svref locs->tps my-parent-loc))
                   (svref keys (svref locs->tps loc))))
           (bh-swap! locs->tps tps->locs my-parent-loc loc)
           (setf loc my-parent-loc)
           (setf my-parent-loc (bh-parent loc)))
    t))

(defun mbh-decrease-key (heap tp new-key)
  (mbh-decrease-keyster heap (svref (mbh-tps->locs heap) tp) new-key))

(defun mbh-insert (heap tp pri)
  (let* ((old-num (mbh-num-items-in-heap heap))
         (tps->locs (mbh-tps->locs heap))
         (locs->tps (mbh-locs->tps heap))
         (keys (mbh-tps->keys heap))
         (statuses (mbh-tps->statuses heap)))
    (setf (svref statuses tp) *in-heap*)
    (setf (svref keys tp) most-positive-fixnum)
    (setf (svref tps->locs tp) old-num)
    (setf (svref locs->tps old-num) tp)
    (incf (mbh-num-items-in-heap heap))
    (mbh-decrease-keyster heap old-num pri)
    t
    ))

;; Amy's function!!
;; MBH-insert-or-decrease-key
;; ---------------------------------------
;;  INPUT:  HEAP: min-bin-heap struct (MBH)
;;          TP: time point (int)
;;          NEW_KEY: possible new key for tp in heap
;;  OUTPUT: meh
;;  SIDE EFFECT: if TP is in HEAP and NEW_KEY is
;;        less than TP's current key, change TP's
;;        key to NEW-KEY and update HEAP
;;        if TP is not in HEAP, insert it
;;        with key NEW-KEY

(defun mbh-insert-or-decrease-key (heap tp new-key)
  ;; old-key: current key of tp in heap
  ;; if tp is not in heap, old-key=nil
  (let ((old-key (svref (mbh-tps->keys heap) tp)))
    ;; if tp is already in the heap
    (if old-key
        ;; if new-key is less than tp's current key
        ;; decrease tp's key in heap to new-key
        (if (< new-key old-key)
            (mbh-decrease-key heap tp new-key))
        ;; else: tp is not in the heap -> insert it
        (mbh-insert heap tp new-key))))

(defmethod mbh-extract-min (heap) ;; ==  POP-HEAP
  (let ((num (mbh-num-items-in-heap heap)))
    (cond
      ((> num 0)
       (let* ((tps->locs (mbh-tps->locs heap))
              (locs->tps (mbh-locs->tps heap))
              (keys (mbh-tps->keys heap))
              (statuses (mbh-tps->statuses heap))
              (minnie (svref locs->tps 0)))
         (setf (svref statuses minnie) *already-popped*)
         (decf (mbh-num-items-in-heap heap))
         (bh-swap! locs->tps tps->locs 0 (mbh-num-items-in-heap heap))
         (min-heapify locs->tps tps->locs keys 0 (mbh-num-items-in-heap heap))
         (setf (svref tps->locs minnie) nil)
         (setf (svref locs->tps (mbh-num-items-in-heap heap)) nil)
         minnie
         ))
      (t
       nil))))

;; Amy's function!!
;; MBH-EXTRACT-MIN-PAIR
;; ---------------------------------------
;;  INPUT:  min-bin-heap struct: an MBH
;;  OUTPUT: pops the min value off of the
;;          the heap, returns a list containing
;;          min value and its key

(defmethod mbh-extract-min-pair (heap) ;; ==  POP-HEAP
  (if (mbh-empty? heap) nil
    (let* ((tps->locs (mbh-tps->locs heap))
              (locs->tps (mbh-locs->tps heap))
              (keys (mbh-tps->keys heap))
              (statuses (mbh-tps->statuses heap))
              (minnie (svref locs->tps 0))
              (minnie-key (svref keys minnie)))
         (setf (svref statuses minnie) *already-popped*)
         (decf (mbh-num-items-in-heap heap))
         (bh-swap! locs->tps tps->locs 0 (mbh-num-items-in-heap heap))
         (min-heapify locs->tps tps->locs keys 0 (mbh-num-items-in-heap heap))
         (setf (svref tps->locs minnie) nil)
         (setf (svref locs->tps (mbh-num-items-in-heap heap)) nil)
         (list minnie minnie-key)
         )))

(defun make-mbh (max-entries)
  (let ((h (make-min-bin-heap)))
    (mbh-init h max-entries)
    h))

(defun mbh-get-key-for (h tp)
  (svref (mbh-tps->keys h) tp))

(defun mbh-get-status (h tp)
  (svref (mbh-tps->statuses h) tp))

(defun print-mbh (h str depth)
  (declare (ignore depth))
  (let ((maxie (mbh-max-num-items h)))
    (format str "MIN-BIN-HEAP:~%")
    (format str "  max-num-items: ~A~%" (mbh-max-num-items h))
    (format str "  num-items: ~A~%" (mbh-num-items-in-heap h))
    (format str "  tps->keys: ")
    (dotimes (i maxie)
             (format str "~A " (aref (mbh-tps->keys h) i)))
    (format str "~%  tps->stat: ")
    (dotimes (i maxie)
             (format str "~A " (aref (mbh-tps->statuses h) i)))
    (format str "~%  tps->locs: ")
    (dotimes (i maxie)
             (format str "~A " (aref (mbh-tps->locs h) i)))
    (format str "~%  locs->tps: ")
    (dotimes (i maxie)
             (format str "~A " (aref (mbh-locs->tps h) i)))
    (format str "~%")))
