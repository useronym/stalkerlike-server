(defmodule protocol-tcp
  (export (make-motd 1)
          (make-login-success 0)
          (make-login-fail 1)
          (get-login 1)))

(include-lib "src/data.lfe")


(defun make-motd (motd)
  (make 'EventLoginMOTD motd))

(defun make-login-success ()
  (make 'EventLoginSuccess))

(defun make-login-fail (reason)
  (make 'EventLoginFail reason))

(defun make-entity-add (entity)
  (make 'EventEntityAdd (tuple-entity entity)))


(defun get-login (data)
  (get 'CommandLogin data))


(defun tuple-entity (entity)
  (let* ((state (lists:keyfind 'entity-state 1 entity))
         (rest (lists:keydelete 'entity-state 1 entity)))
    (tuple
     (tuple 'entityState (tuple-component state))
     (tuple 'comps (lists:map #'tuple-component/1 rest)))))

(defun tuple-component
  ((rec) (when (is-entity-state rec))
   (tuple-state rec))
  ((rec) (when (is-entity-moves rec))
   (tuple-moves rec)))

(defun tuple-state (r)
  (tuple
   (list
    (tuple 'stateName (entity-state-name r))
    (tuple 'stateIcon (entity-state-icon r))
    (tuple 'statePos (tuple-pos (entity-state-pos r)))
    (tuple 'stateRot (entity-state-rot r))
    (tuple 'stateHP (entity-state-hp r))
    (tuple 'stateMaxHP (entity-state-max-hp r)))))

(defun tuple-moves (r)
  (tuple
   (list
    (tuple 'movesWalkSpeed (entity-moves-walk-speed r))
    (tuple 'movesRunSpeed (entity-moves-run-speed r))
    (tuple 'movesCrouchSpeed (entity-moves-crouch-speed r))
    (tuple 'movesModes (entity-moves-modes r)))))

(defun tuple-pos
  (((tuple x y))
   (tuple
    (list
     (tuple 'posX x)
     (tuple 'posY y)))))


(defun make (tag)
  (jiffy:encode (tuple
                 (list
                  (tuple 'tag tag)))))

(defun make (tag contents)
  (jiffy:encode (tuple
                 (list
                  (tuple 'tag tag)
                  (tuple 'contents (list_to_binary contents))))))

(defun get (tag data)
  (let ((m (jiffy:decode data '(return_maps))))
    (if (== (map-get m #"tag") (atom_to_binary tag 'latin1))
      (map-get m #"contents")
      '(error wring-tag))))