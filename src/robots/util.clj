(ns robots.util)

(defn parse-int
  [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException e
      nil)))

(defn pad
  [coll n padding]
  (take n (concat coll (repeat padding))))

(defn til-truthy
  [f]
  (first (filter identity (repeatedly f))))

(defn vos+vos
  [vos1 vos2]
  (let [length (max (count vos1) (count vos2))]
  (->> (interleave (pad vos1 length "") (pad vos2 length ""))
       (partition 2)
       (map #(apply str %)))))

(defn add-border-to-vos
  [vos]
  (let [width (count (first vos))
        border (apply str (flatten [\+ (repeat width \-) \+]))]
    (vec (concat [border] (map #(format "|%s|" %) vos) [border]))))
