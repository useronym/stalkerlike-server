(defmodule stalker-sup
  (behaviour supervisor)
  ;; API
  (export (start-link 0))
  ;; Supervisor callbacks
  (export (init 1)))

(defun server-name ()
  'stalker-sup)

(defun start-link ()
  (supervisor:start_link
   (tuple 'local (server-name)) (MODULE) '()))

(defun init (args)
  (let* ((world-server (map
                        'id 'world-server
                        'start #(world-server start-link ())))
         (entity-server (map
                          'id 'entity-server
                          'start #(entity-server start-link ())))
         (tcp-server (map
                       'id 'tcp-server
                       'start #(server-tcp start-link ())))
         (children (list world-server entity-server tcp-server))
         (restart-strategy (map
                            'strategy 'one_for_one
                            'intensity 10
                            'period 10)))
    (tuple 'ok (tuple restart-strategy children))))
