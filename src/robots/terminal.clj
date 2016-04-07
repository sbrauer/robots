(ns robots.terminal
  (:import [jline.console ConsoleReader]))

(defn move-cursor
  [[x y]]
  (print (str "\u001b[" y ";" x "f")))

(defn clear-screen
  []
  (print "\u001b[2J")
  (move-cursor [0 0]))

(defn ring-bell
  []
  (print (char 7))
  (flush))

(defn get-key
  []
  (let [cr (ConsoleReader.)
            keyint (.readCharacter cr)]
    (char keyint)))
