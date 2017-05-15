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
  (tuple
   (list
    (tuple 'entityState (tuple-entity-state entity))
    (tuple 'entityConf (tuple-entity-conf entity)))))

(defun tuple-entity-state (e)
  (tuple
   (list
    (tuple 'entityPos (tuple-pos (entity-state-pos e)))
    (tuple 'entityRot (entity-state-rot e))
    (tuple 'entityMoves (entity-state-moves e))
    (tuple 'entityBearing (tuple-bearing (entity-state-bearing e)))
    (tuple 'entityHP (entity-state-hp e))
    (tuple 'entityWielding (tuple-wielding (entity-state-wielding e))))))

(defun tuple-entity-conf (e)
  (tuple
   (list
    (tuple 'entityName (entity-conf-name e))
    (tuple 'entityIcon (entity-conf-icon e))
    (tuple 'entityMaxHP (entity-conf-max-hp e))
    (tuple 'entityActions (entity-conf-actions e)))
   (tuple 'entityMore (tuple-more (entity-conf-more e)))))

(defun tuple-pos
  (((tuple x y))
   (tuple
    (list
     (tuple 'posX x)
     (tuple 'posY y)))))

(defun tuple-bearing (b)
  'Standing)

(defun tuple-wielding (w)
  'null)

(defun tuple-more (items)
  [])


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