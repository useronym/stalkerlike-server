(defmodule server-tcp
  (behaviour gen_server)
  (export (start-link 0)
          (stop 0))
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))


(defun PORT ()
  31415)

(defun server-name ()
  'server-tcp)

;;;===================================================================
;;; API
;;;===================================================================

(defun start-link ()
  (gen_server:start_link (tuple 'local (server-name)) (MODULE) '() '()))

(defun stop ()
  (gen_server:call (server-name) 'stop))

;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init (args)
  (progn
    (process_flag 'trap_exit 'true)
    (let (((tuple ok listen-socket)
           (gen_tcp:listen (PORT) '(#(packet line)
                                    #(reuseaddr true)
                                    #(mode list)
                                    #(active false)))))
      (progn
        (spawn-acceptor listen-socket)
        (tuple 'ok listen-socket)))))

(defun handle_call
  (('stop from sock)
   (progn (gen_tcp:close sock)
          (tuple 'stop 'ok 'ok '()))))

(defun handle_cast
  ((msg sock)
   (progn (error_logger:warning_report (tuple "Unrequested message arrived" msg))
          (tuple 'noreply sock))))

(defun handle_info
  (((tuple 'EXIT from reason) sock)
   (progn (spawn-acceptor sock)
          (tuple 'noreply sock))))

(defun terminate (reason state)
  'ok)

(defun code_change (old-version state extra)
  (tuple 'ok state))

;;;===================================================================
;;; Private
;;;===================================================================

(defun accept (listen-socket)
  (let (((tuple 'ok socket) (gen_tcp:accept listen-socket)))
    (progn
      (gen_tcp:send socket (protocol-tcp:make-motd "Come in, don't stand there!"))
      (let* (((tuple 'ok data) (gen_tcp:recv socket 0))
             ((list user pass) (protocol-tcp:get-login data)))
        (if (can-login user pass)
          (progn
            (gen_tcp:send socket (protocol-tcp:make-login-success))
            (controller-tcp:start socket (lookup-player user)))
          (gen_tcp:send socket (protocol-tcp:make-login-fail "Bad user & pass combination")))))))

(defun spawn-acceptor (sock)
  (spawn_link (lambda () (accept sock))))

(defun can-login (user pass)
  'true)

(defun lookup-player (user)
  (entity:start-link #(0 0) (entity:stalker)))