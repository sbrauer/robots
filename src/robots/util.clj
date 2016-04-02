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

(defn append-to-vos
  "vos is a vector of strings.
  appends is a seq of pairs where first item is an index int and second is a string to append at that index.
  Returns a new vector of strings with the appends applied."
  [vos appends]
  (reduce
    (fn
      [coll [idx s]]
      (assoc coll idx (str (nth coll idx) s)))
    vos
    appends))

(defn add-border-to-vos
  [vos]
  (let [width (count (first vos))
        border (apply str (flatten [\+ (repeat width \-) \+]))]
    (vec (concat [border] (map #(format "|%s|" %) vos) [border]))))
