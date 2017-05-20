(defrecord entity-state
  (name "Unknown")
  (icon "?")
  pos
  (rot 0)
  (visible 'true)
  (hp 100)
  (max-hp 100))

(defrecord entity-moves
  (walk-speed 1.0)
  (run-speed 2.0)
  (crouch-speed 0.5)
  (modes '(walk run)))

(defrecord entity-item
  value
  weight)

(defrecord entity-weapon
  ammo
  mags
  accuracy
  fire-modes
  upgrades
  melee-damage)

(defrecord entity-ammo
  damage)

(defrecord entity-weapon-upgrade
  (ammo '())
  (accuracy 0.0)
  (fire-modes '())
  (melee-damage 0))

(defrecord entity-mag
  capacity)