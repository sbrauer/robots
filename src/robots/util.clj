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
  "Call f until it returns a truthy value. Returns the truthy value."
  [f]
  (some identity (repeatedly f)))

(defn append-strings
  [strings1 strings2]
  (let [length (max (count strings1) (count strings2))]
    (map str (pad strings1 length "") (pad strings2 length ""))))

(defn add-border-to-strings
  [strings]
  (let [width (count (first strings))
        border (apply str (flatten [\+ (repeat width \-) \+]))]
    (vec (concat [border] (map #(format "|%s|" %) strings) [border]))))
