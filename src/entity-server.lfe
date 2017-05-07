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
  (gen_server:start_link
   #('local (server-name)) (MODULE) '() '()))

; Subscribe to receiving updates from this particular part of the world.
(defun subscribe (chunkp)
  (gen_server:cast (server-name) (tuple 'subscribe (self) chunkp)))

(defun unsubscribe (chunkp)
  (gen_server:cast (server-name) (tuple 'unsubscribe (self) chunkp)))

(defun get (chunkp)
  (fen_server:call (server-name) (tuple 'get chunkp)))

; Add a new entity to the world.
(defun add (entity)
  (gen_server:cast (server-name) (tuple 'add entity)))

(defun remove (entity)
  (gen_server:cast (server-name) (tuple 'remove entity)))

; Notify the world that this entity has changed its state.
(defun update (entity)
  (gen_server:cast (server-name) (tuple 'update entity)))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init (args)
  (tuple 'ok (load-world)))

(defun handle_call
  (((tuple 'get chunkp) from world)
   (tuple 'reply (chunk-entities (map-get world chunkp)))))

(defun handle_cast
  (((tuple 'add entity) world)
   (progn (broadcast 'add entity world)
          (tuple 'noreply (add-entity entity world))))
  (((tuple 'remove entity) world)
   (progn (broadcast 'remove entity world)
          (tuple 'noreply (remove-entity entity world))))
  (((tuple 'update entity) world)
   (progn (remove entity)
          (add entity)))
  (((tuple 'subscribe pid chunkp) world)
   (tuple 'noreply (maps:update_with chunkp
                                     (lambda (chunk)
                                       (set-chunk-subs chunk (cons pid (chunk-subs chunk))))
                                     (make-chunk)
                                     world)))
  (((tuple 'unsubscribe pid chunkp) world)
   (tuple 'noreply (maps:update_with chunkp
                                     (lambda (chunk)
                                       (set-chunk-subs chunk (lists:remove pid (chunk-subs chunk))))
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

(defun broadcast (info entity world)
  (progn (lists:foreach (lambda (pid) (! pid (tuple info entity)))
                        (chunk-subs (chunk-from-ent entity world)))))

(defun locate-pos
  (((tuple x y))
   (tuple (rem x (CHUNK-SIZE)) (rem y (CHUNK-SIZE)))))

(defun chunk-from-pos (pos world)
   (map-get world (locate-pos pos)))

(defun chunk-from-ent (ent world)
  (chunk-from-pos (entity:pos ent) world))

(defun add-entity (ent world)
  (maps:update_with (locate-pos (entity:pos ent))
                    (lambda (chunk) (set-chunk-entities chunk
                                     (cons ent (chunk-entities chunk))))
                    world))

; Not functional
(defun remove-entity (ent world)
  (maps:update_with (locate-pos (entity:pos ent))
                    (lambda (chunk) (set-chunk-entities chunk
                                     (lists:remove ent (chunk-entities chunk))))
                    world))