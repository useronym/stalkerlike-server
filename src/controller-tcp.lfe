(defmodule controller-tcp
  (behaviour gen_server)
  ;; API
  (export (start 2))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(defrecord state
  socket
  entity)


;;;===================================================================
;;; API
;;;===================================================================

(defun start (socket entity)
  (gen_server:start (MODULE) '(socket entity) '()))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init
  (((sock entity-pid))
   (progn (renderer:start-link (entity:get-pos entity-pid) (self))
          (inet:setopts sock '(#(active true)))
          (tuple 'ok (make-state socket sock entity entity-pid)))))

(defun handle_call
  ((request from state)
   (tuple 'reply 'ok state)))

(defun handle_cast
  ((message state)
    (tuple 'noreply state)))

(defun handle_info
  (((tuple 'tcp s data) state)
   (tuple 'noreply state))
  (((tuple 'tcp_closed s) state)
   (tuple 'stop "TCP socket closed" state))
  (((tuple 'entity-add e) s)
   (progn
     (gen_tcp:send (state-socket s) (protocol-tcp:make-entity-add e))
     (tuple 'noreply s)))
  (((tuple 'entity-remove eid) s)
   (progn
     (gen_tcp:send (state-socket s) (protocol-tcp:make-entity-remove eid))
     (tuple 'noreply s)))
  (((tuple 'terrain t) s)
   (progn
     (gen_tcp:send (state-socket s) (protocol-tcp:make-terrain t))
     (tuple 'noreply s))))


(defun terminate (reason state)
  'ok)

(defun code_change (old-version state extra)
  (tuple 'ok state))
