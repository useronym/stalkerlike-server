(defmodule entity-server
  (behaviour gen_server)
  ;; API
  (export (start-link 0)
          (subscribe 1)
          (unsubscribe 1)
          (get 1)
          (add 1)
          (remove 1)
          (update 1))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(include-lib "src/data.lfe")

(defrecord chunk
  (entities '())
  (subs '()))

(defun CHUNK-SIZE ()
  128)

(defun server-name ()
  'entity-server)

;;;===================================================================
;;; API
;;;===================================================================

(defun start-link ()
  (gen_server:start_link (tuple 'local (server-name)) (MODULE) '() '()))

; Subscribe to receiving updates from this particular part of the world.
(defun subscribe (chunkp)
  (gen_server:cast (server-name) (tuple 'subscribe (self) chunkp)))

(defun unsubscribe (chunkp)
  (gen_server:cast (server-name) (tuple 'unsubscribe (self) chunkp)))

(defun get (chunkp)
  (fen_server:call (server-name) (tuple 'get chunkp)))

; Add a new entity to the world.
(defun add (entity-pid)
  (gen_server:cast (server-name) (tuple 'add entity-pid)))

(defun remove (entity-pid)
  (gen_server:cast (server-name) (tuple 'remove entity-pid)))

; Notify the world that this entity has changed its state.
(defun update (entity-pid)
  (gen_server:cast (server-name) (tuple 'update entity-pid)))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init (args)
  (tuple 'ok (load-world)))

(defun handle_call
  (((tuple 'get chunkp) from world)
   (tuple 'reply (chunk-entities (map-get world chunkp)))))

(defun handle_cast
  (((tuple 'add entity-pid) world)
   (let ((entity-pos (entity:get-pos entity-pid)))
     (progn (broadcast 'add entity-pid world)
          (tuple 'noreply (add-entity entity-pid world)))))
  (((tuple 'remove entity) world)
   (progn (broadcast 'remove entity world)
          (tuple 'noreply (remove-entity entity world))))
  (((tuple 'update entity) world)
   (progn (remove entity)
          (add entity)
          (tuple 'noreply world)))
  (((tuple 'subscribe pid chunkp) world)
   (tuple 'noreply (maps:update_with chunkp
                                     (lambda (chunk)
                                       (set-chunk-subs chunk (cons pid (chunk-subs chunk))))
                                     (make-chunk)
                                     world)))
  (((tuple 'unsubscribe pid chunkp) world)
   (tuple 'noreply (maps:update_with chunkp
                                     (lambda (chunk)
                                       (set-chunk-subs chunk (lists:delete pid (chunk-subs chunk))))
                                     (make-chunk)
                                     world))))

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
  (map #(0 0) (make-chunk entities '() subs '())))

(defun broadcast (info entity-pid world)
  (lists:foreach (lambda (pid) (! pid (tuple info entity-pid)))
                 (chunk-subs (chunk-from-pos (entity:get-pos entity-pid) world))))

(defun locate-pos
  (((tuple x y))
   (tuple (rem x (CHUNK-SIZE)) (rem y (CHUNK-SIZE)))))

(defun chunk-from-pos (pos world)
   (map-get world (locate-pos pos)))

(defun add-entity (ent world)
  (maps:update_with (locate-pos (entity:get-pos ent))
                    (lambda (chunk) (set-chunk-entities chunk
                                     (cons ent (chunk-entities chunk))))
                    world))

(defun remove-entity (ent world)
  (maps:update_with (locate-pos (entity:get-pos ent))
                    (lambda (chunk) (set-chunk-entities chunk
                                     (lists:delete ent (chunk-entities chunk))))
                    world))