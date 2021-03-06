(ns joa-dices-clojure.core
  (:require (joa-dices-clojure [dice :as dice]
                               [parser :as parser])))

(defn count-face
  "count the number of a given face in a roll"
  [face roll]
  (count (filter (partial = face) roll)))

(defn cancel
  "cancel roll faces by an amount of shield count, return cancelled roll and remaining shield count"
  [face [roll shield-count]]
  (reduce (fn [[r s] cur]
            (if (and (= cur face) (> s 0))
              [r (dec s)]
              [(conj r cur) s]))
          [[] shield-count]
          roll))

(defn apply-counter
  "apply counter shields on the attack and remove unrelevant faces from the attack"
  [attack counter]
  (let [[result _] (->> [attack (count-face :shield counter)]
                        (cancel :kill)
                        (cancel :disrupt)
                        (cancel :push))]
    (->> result
         (filter (partial not= :shield))
         (filter (partial not= :blank)))))

(defn frequencies'
  "frequency of the faces in a roll (just for fun, already present in the std lib)"
  [roll]
  (apply merge-with + (for [face roll] {face 1})))

(defn fight
  "compute the result of a fight"
  [attack-dices counter-dices counter?]
  (let [attack  (dice/roll-dices attack-dices)
        counter (dice/roll-dices counter-dices)]
    (if counter?
      {:attack (frequencies attack)
       :counter (frequencies counter)
       :result (frequencies (apply-counter attack counter))}
      (frequencies attack))))

(defn main []
  (apply fight (parser/parse (second *command-line-args*))))



(comment
  (dice/rolln 1 dice/black-dice)
  (cancel :push (cancel :kill [(dice/rolln 3 dice/black-dice) 3]))
  (count-face :kill (dice/rolln 100 dice/red-dice))
  (apply-counter (dice/rolln 10 dice/red-dice) (dice/rolln 10 dice/black-dice))
  (dice/roll-dices [[1 dice/black-dice] [2 dice/red-dice]])
  (frequencies' (apply-counter (dice/rolln 100 dice/red-dice) (dice/rolln 80 dice/black-dice)))
  (frequencies [:kill :kill :push :blank])
  (apply fight (parser/parse "14R - 10B")))

;; Calva commands
;;   - ctrl+alt+c ctrl+alt+j : load script in repl
;;   - ctrl+enter            : evaluate expression at cursor position
