; Renders a part of the world and sends updates to its master.
; Possible updates are:
; (tuple 'terrain t) -- terrain has changed, here's the new one
; (tuple 'entity-add e) -- new entity has appeared
; (tuple 'entity-remove eid) -- entity with this id has disappeared
; (tuple 'entity-update e) -- entity has changed state

(defmodule renderer
  (behaviour gen_server)
  ;; API
  (export (start-link 2)
          (set-pos 2)
          (get-render 1))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(defrecord state
  (terrain '())
  (entities '())
  pos
  master)


;;;===================================================================
;;; API
;;;===================================================================

(defun start-link (pos master)
  (gen_server:start_link (MODULE) (list pos master) '()))

(defun set-pos (renderer pos)
  (gen_server:cast renderer (tuple 'set-pos pos)))

; Returns (tuple terrain entities)
(defun get-render (renderer)
  (gen_server:call renderer 'get))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init
  (((list p m))
   (progn (set-pos (self) p)
          (tuple 'ok (make-state pos p master m)))))

(defun handle_call
  (('get from state)
   (tuple 'reply (tuple (state-terrain state)
                        (state-entities state))
          state)))

(defun handle_cast
  (((tuple 'set-pos pos) state)
   (let ((t (world-server:get-chunk #(0 0))))
     (progn (! (state-master state) (tuple 'terrain t))
            (tuple 'noreply (set-state-terrain state t))))))

(defun handle_info (info state)
  (tuple 'noreply state))

(defun terminate (reason state)
  'ok)

(defun code_change (old-version state extra)
  (tuple 'ok state))
