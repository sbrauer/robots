(ns robots.core
  (:gen-class)
  (:require [robots.game :as game]
            [robots.util :as util]))

(defn -main
  "Resistance is useless!"
  [& args]
  (let [start-level (or (util/parse-int (first args)) 1)]
    (while (game/play-game start-level)))
  (println "\nGoodbye!"))
