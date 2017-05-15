(defmodule entity
  (behaviour gen_server)
  ;; API
  (export ;(new 2)
          ;(move 2)
          ;(rotate 2)
          all)
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(include-lib "src/data.lfe")


;;;===================================================================
;;; API
;;;===================================================================

(defun new (pos conf)
  (gen_server:start (MODULE) (list pos conf) '()))

; Dir is 0-7.
(defun move (player dir)
  (gen_server:cast player (tuple 'move dir)))

; Dir is 1 or -1.
(defun rotate (player dir)
  (gen_server:cast player (tuple 'rotate dir)))

(defun add-moves (player moves)
  (gen_server:cast player (tuple 'add-moves moves)))

(defun get-state (entity)
  (gen_server:call entity 'get-state))

(defun get-conf (entity)
  (gen_server:call entity 'get-conf))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init
  (((p conf))
   (let ((state (make-entity-state pos p hp (entity-conf-max-hp conf))))
     (progn (entity-server:add state)
            (tuple 'ok (tuple state conf))))))

(defun handle_call
  (('get-state from (tuple state conf))
    (tuple 'reply state (tuple state conf)))
  (('get-conf from (tuple state conf))
    (tuple 'reply conf (tuple state conf))))

(defun handle_cast
  (((tuple 'move dir) (tuple state conf))
   (let ((new-pos (move-in-dir (entity-state-pos state) (entity-state-rot state) dir)))
     (progn (entity-server:update state)
            (tuple 'noreply (tuple (set-entity-state-pos state new-pos)
                                   conf)))))
  (((tuple 'rotate dir) (tuple state conf))
   (tuple 'noreply (tuple (set-entity-state-rot state (+ dir (entity-state-rot state)))
                          conf))))

(defun handle_info (info state)
  (tuple 'noreply state))

(defun terminate (reason state)
  'ok)

(defun code_change (old-version state extra)
  (tuple 'ok state))


;;;===================================================================
;;; Private
;;;===================================================================

(defun rot-to-vec
  ((0) #(0 1))
  ((1) #(1 1))
  ((2) #(1 0))
  ((3) #(1 -1))
  ((4) #(0 -1))
  ((5) #(-1 -1))
  ((6) #(-1 0))
  ((7) #(-1 1)))

(defun move-in-dir (pos rot dir)
  (vec-add pos (rot-to-vec (rem (+ rot dir) 8))))

(defun vec-add
  (((tuple x1 y1) (tuple x2 y2))
   (tuple (+ x1 x2) (+ y1 y2))))


;;;===================================================================
;;; Entities
;;;===================================================================

(defun entity-stalker ()
  (make-entity-conf
   name "Stalker"
   icon #\@
   max-hp 100
   actions '(walk run crouch equip-item consume-item attack)
   more (list (make-alive
               speed-walk 100
               speed-run 40
               attack-min 5
               attack-max 10))))

(defun entity-dog ()
  (make-entity-conf
   name "Blind Dog"
   icon #\d
   max-hp 40
   actions '(walk run attack)
   more (list(make-alive
              speed-walk 75
              speed-run 200
              attack-min 0
              attack-max 10))))

(defun entity-knife ()
  (make-entity-conf
   name "Knife"
   icon #\t
   max-hp 10000
   actions '(pick-up wield)
   more (list (make-weapon melee-damage 40)
              (make-weapon-upgrade melee-damage 40))))

(defun entity-ak74 ()
  (make-entity-conf
   name "AK-74"
   icon #\/
   max-hp 1000
   actions '(pick-up wield fire reload-weapon upgrade-weapon)
   more (list (make-weapon
               ammo '("5.45×39mm")
               mags '("AK-74 Magazine")
               accuracy 1.0
               fire-modes '(auto one)
               upgrades '("Knife" "1P29")))))

(defun entity-AK74-mag ()
  (make-entity-conf
   name "AK-74 Magazine"
   icon #\=
   max-hp 1000
   actions '(pick-up reload-mag)
   more (list (make-mag capacity 30))))

(defun entity-5.45x39mm ()
  (make-entity-conf
   name "5.45×39mm"
   icon #\a
   max-hp 20
   actions '(pick-up)
   more (list (make-ammo damage 100))))

(defun entity-1P29 ()
  (make-entity-conf
   name "1P29"
   icon #\o
   max-hp 50
   actions '(pick-up)
   more (list (make-weapon-upgrade accuracy 1.0))))