(ns joa-dices-clojure.parser
  (:require [joa-dices-clojure.dice :as dice]
            [clojure.string :as str]))

(defn parse-dice
  "returns a dice corresponding to the char"
  [dice]
  (case dice
    \B dice/black-dice
    \R dice/red-dice
    \Y dice/yellow-dice
    \W dice/white-dice
    \G dice/gigantic-dice
    \D dice/doom-dice
    :none))

(defn parse-number
  "returns a number or 1"
  [num]
  (try
    (Integer/parseInt num)
    (catch NumberFormatException _ 1)))

(defn parse
  "parse a roll dice command in the form: 3R Y - 2B 1W"
  [command]
  (reduce (fn [[a, d, d?] cur]
            (if (= cur "-")
              [a, d, true]                          ; switch to defense mode
              (let [dice (parse-dice (last cur))
                    n (parse-number (str/join (butlast cur)))]
                (if (= dice :none)
                  [a, d, d?]                        ; parse error, do nothing
                  (if d?
                    [a, (conj d [n dice]), d?]       ; add the dice to defense
                    [(conj a [n dice]), d, d?])))))  ; add the dice to attack
          [[] [] false]
          (str/split command #" ")))



(comment
  (parse-number "10")
  (parse-number "")
  (parse-number "a")
  (parse-number (str/join (butlast "12R")))
  (parse-dice \B)
  (parse-dice \Z)
  (parse "13R - 6Y"))


