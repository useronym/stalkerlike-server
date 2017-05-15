(defrecord entity-state
  pos
  (rot 0)
  (moves 100)
  (bearing 'standing)
  hp
  wielding
  (inv '()))

(defrecord entity-conf
  name
  icon
  max-hp
  actions
  (more '()))

(defrecord alive
  speed-walk
  speed-run
  attack-min
  attack-max)

(defrecord weapon
  ammo
  mags
  accuracy
  fire-modes
  upgrades
  melee-damage)

(defrecord ammo
  damage)

(defrecord weapon-upgrade
  (ammo '())
  (accuracy 0.0)
  (fire-modes '())
  (melee-damage 0))

(defrecord mag
  capacity)