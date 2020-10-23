(ns joa-dices-clojure.core
  (:require [joa-dices-clojure.dice :refer :all]))

; apply counter shields on the attack and remove unrelevant faces from the attack
(defn apply-counter [attack counter]
  (let [[result _] (->> [attack (count-face :shield counter)]
                        (cancel :kill)
                        (cancel :disrupt)
                        (cancel :push))]
    (->> result
         (filter (partial not= :shield))
         (filter (partial not= :blank)))))

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

; frequency of the faces in a roll (just for fun, already present in the std lib)
(defn frequencies' [roll]
  (apply merge-with + (for [face roll] {face 1})))

; compute the result of a fight
(defn fight [attack-dices counter-dices counter?]
  (let [attack  (roll-dices attack-dices)
        counter (roll-dices counter-dices)]
    (if counter?
      {:attack (frequencies attack)
       :counter (frequencies counter)
       :result (frequencies (apply-counter attack counter))}
      (frequencies attack))))

; tests
(rolln 1 black-dice)
(cancel :push (cancel :kill [(rolln 3 black-dice) 3]))
(count-face :kill (rolln 100 red-dice))
(apply-counter (rolln 10 red-dice) (rolln 10 black-dice))
(roll-dices [[1 black-dice] [2 red-dice]])
(apply-counter [:kill :kill :push :blank] [:shield :shield])
(frequencies' (apply-counter (rolln 100 red-dice) (rolln 80 black-dice)))
(frequencies [:kill :kill :push :blank])
(fight [[100 red-dice]] [[80 black-dice]] true)
