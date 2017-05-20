(defmodule entity-state
  (behaviour gen_server)
  (export (start-link 1)
          (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(include-lib "src/data.lfe")


(defun start-link (args)
  (gen_server:start_link (MODULE) args '()))

;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init
  (((list entity conf))
   (tuple 'ok (tuple entity conf))))

(defun handle_call
  (('get-pos from (tuple entity state))
   (tuple 'reply
          (entity-state-pos state)
          (tuple entity state))))

(defun handle_cast
  (((tuple 'set-pos pos) (tuple entity state))
   (progn
     (entity-server:update entity)
     (tuple 'noreply
            (tuple entity (set-entity-state-pos state pos))))))

(defun handle_info (info state)
  (tuple 'noreply state))

(defun terminate (reason state)
  'ok)

(defun code_change (old-version state extra)
  (tuple 'ok state))