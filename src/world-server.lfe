(defmodule world-server
  (behaviour gen_server)
  ;; API
  (export (start-link 0)
          (get-chunk 1)
          (get 1)
          (show 1))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(defrecord tile
  type
  (vision 'true)
  (rads 0))

(defun CHUNK-SIZE ()
  10)

(defun server-name ()
  'world-server)

;;;===================================================================
;;; API
;;;===================================================================

(defun start-link ()
  (gen_server:start_link (tuple 'local (server-name)) (MODULE) '() '()))

(defun get-chunk (chunk-pos)
  (gen_server:call (server-name) (tuple 'get-chunk chunk-pos)))

(defun get (pos)
  (gen_server:call (server-name) (tuple 'get pos)))

(defun show (chunk)
  (lists:concat (lists:map (lambda (row) (++ (lists:map (lambda (x) (tile-type x)) row) '(10))) chunk)))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init (args)
  (tuple 'ok (load-world)))

(defun handle_call
  (((tuple 'get-chunk chunk-pos) from world)
   (tuple 'reply (map-get world chunk-pos) world))
  (((tuple 'get pos) from world)
   (tuple 'reply
          (get-in-chunk pos (map-get world (chunkp-from-pos pos world)))
          world)))

(defun handle_cast
  ((req world)
   (tuple 'noreply world)))

(defun handle_info (info state)
  (tuple 'noreply state))

(defun terminate (reason state)
  'ok)

(defun code_change (old-version state extra)
  (tuple 'ok state))


;;;===================================================================
;;; Private
;;;===================================================================

(defun load-world ()
  (map #(0 0)
       (clj:repeat (CHUNK-SIZE) (clj:repeat (CHUNK-SIZE) (make-tile type #\.)))))

(defun get-in-chunk
  (((tuple x y) chunk)
   (lists:nth (rem x (CHUNK-SIZE))
              (lists:nth (rem y (CHUNK-SIZE)) chunk))))

(defun chunkp-from-pos
  (((tuple x y) world)
   (map-get world (tuple (rem x (CHUNK-SIZE)) (rem y (CHUNK-SIZE))))))
