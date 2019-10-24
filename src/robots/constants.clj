(ns robots.constants)

(def ^:const robots-per-level 10)

;; Same as classic BSD game (fits nicely on 80x25 terminal)
(def ^:const cols 59)
(def ^:const rows 22)

(def ^:const player-char \@)
(def ^:const dead-player-char \X)
(def ^:const robot-char \+)
(def ^:const pile-char \*)
(def ^:const empty-char \space)
