(ns joa-dices-clojure.core
  (:require [joa-dices-clojure.dice :refer :all]))

; cancel roll faces by an amount of shield count
(defn cancel [face [roll shield-count]]
  (defn f [[r s] cur]
    (cond
      (<= s 0)     [(conj r cur) 0]
      (= cur face) [r (dec s)]
      :else        [(conj r cur) s]))
  (reduce f [[] shield-count] roll))

; count the number of a given face in a roll
(defn count-face [face roll]
  (count (filter (partial = face) roll)))

; apply defence shields on the attack and remove unrelevant faces from the attack
(defn apply-defense [attack defense]
  (let [[result _] (->> [attack (count-face :shield defense)]
                        (cancel :kill)
                        (cancel :disrupt)
                        (cancel :push))]
    (->> result
         (filter (partial not= :shield))
         (filter (partial not= :blank)))))

; frequency of the faces in a roll
(defn frequency [roll]
  (apply merge-with + (for [face roll] {face 1})))

; tests
(rolln 1 black-dice)
(cancel :push (cancel :kill [(rolln 3 black-dice) 3]))
(count-face :kill (rolln 100 red-dice))
(apply-defense (rolln 10 red-dice) (rolln 10 black-dice))
(roll-dices [[1 black-dice] [2 red-dice]])
(apply-defense [:kill :kill :push :blank] [:shield :shield])
(frequency [:kill :kill :push :blank])
