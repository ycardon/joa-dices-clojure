(ns joa-dices-clojure.core
  (:require [joa-dices-clojure.dice :as dice]))

; count the number of a given face in a roll
(defn count-face [face roll]
  (count (filter (partial = face) roll)))

; cancel roll faces by an amount of shield count
(defn cancel [face [roll shield-count]]
  (defn f [[r s] cur]
    (if (and (= cur face) (> s 0))
      [r (dec s)]
      [(conj r cur) s]))
  (reduce f [[] shield-count] roll))

; apply counter shields on the attack and remove unrelevant faces from the attack
(defn apply-counter [attack counter]
  (let [[result _] (->> [attack (count-face :shield counter)]
                        (cancel :kill)
                        (cancel :disrupt)
                        (cancel :push))]
    (->> result
         (filter (partial not= :shield))
         (filter (partial not= :blank)))))

; frequency of the faces in a roll (just for fun, already present in the std lib)
(defn frequencies' [roll]
  (apply merge-with + (for [face roll] {face 1})))

; compute the result of a fight
(defn fight [attack-dices counter-dices counter?]
  (let [attack  (dice/roll-dices attack-dices)
        counter (dice/roll-dices counter-dices)]
    (if counter?
      {:attack (frequencies attack)
       :counter (frequencies counter)
       :result (frequencies (apply-counter attack counter))}
      (frequencies attack))))

; tests
(comment
  (dice/rolln 1 dice/black-dice)
  (cancel :push (cancel :kill [(dice/rolln 3 dice/black-dice) 3]))
  (count-face :kill (dice/rolln 100 dice/red-dice))
  (apply-counter (dice/rolln 10 dice/red-dice) (dice/rolln 10 dice/black-dice))
  (dice/roll-dices [[1 dice/black-dice] [2 dice/red-dice]])
  (frequencies' (apply-counter (dice/rolln 100 dice/red-dice) (dice/rolln 80 dice/black-dice)))
  (frequencies [:kill :kill :push :blank])
  (fight [[100 dice/red-dice]] [[80 dice/black-dice]] true)
  (apply-counter [:kill :kill :push :blank] [:shield :shield]))