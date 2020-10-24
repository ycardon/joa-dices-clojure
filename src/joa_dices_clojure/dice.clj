(ns joa-dices-clojure.dice)

(def black-dice    [:kill :disrupt :disrupt :shield :shield :shield])
(def red-dice      [:kill :kill :disrupt :disrupt :push :shield])
(def yellow-dice   [:disrupt :push :push :shield :blank :blank])
(def white-dice    [:disrupt :disrupt :push :shield :shield :blank])
(def gigantic-dice [:kill :disrupt :disrupt :push :trample :trample])
(def doom-dice     [:disrupt :death :death, :rally, :rally, :relayed-rally])

(defn roll1 [dice] (rand-nth dice))
(defn rolln [n dice] (repeatedly n #(roll1 dice)))
(defn roll-dices [dices] (mapcat (partial apply rolln) dices))



(comment
  (def faces? #{:kill :disrupt :push :shield :blank :trample :death :rally :relayedRally}))