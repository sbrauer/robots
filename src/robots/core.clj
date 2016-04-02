(ns robots.core
  (:gen-class)
  (:require [robots.game :as game]
            [robots.util :as util]))

(defn -main
  "Resistance is useless!"
  [& args]
  (let [level (or (util/parse-int (first args)) 1)]
    (loop []
      (if (game/play-game level) (recur))))
  (println "\nGoodbye!"))
