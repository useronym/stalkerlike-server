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
  (data (tuple)))


;;;===================================================================
;;; API
;;;===================================================================

(defun start (entity socket)
  (gen_server:start (MODULE) '(entity socket) '()))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init
  (((entity-pid socket))
   (progn (renderer:start-link (entity:get-pos entity-pid) (self))
          (tuple 'ok (tuple entity-pid socket)))))

(defun handle_call
  (((tuple 'new-player name) from state)
   (player:new #(0 0))
   (tuple 'reply 'ok state))
  ((request from state)
   (tuple 'reply 'ok state)))

(defun handle_cast
  (((tuple 'test message) state)
    (: lfe_io format '"Cast: ~p~n" (list message))
    (tuple 'noreply state))
  ((message state)
    (tuple 'noreply state)))

(defun handle_info
  (((tuple 'tcp s data) state)
   (tuple 'noreply state))
  (((tuple 'tcp_closed s) state)
   (tuple 'stop "TCP socket closed" state))
  (((tuple 'terrain t) (tuple e socket))
   (progn (ssl:send socket (jiffy:encode (tuple `(,(tuple 'terrain (lists:map #'list_to_binary/1 t))))))
          (tuple 'noreply (tuple e socket)))))

(defun terminate (reason state)
  'ok)

(defun code_change (old-version state extra)
  (tuple 'ok state))
