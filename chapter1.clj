(defn dotproduct [a b]
  (+(* (first a) (first b))
    (recur (rest a) (rest b)))
  )


(defn dotproduct2 [a b]
  (loop [dp 0, a1 a, b1 b]
    (if (pos? (count a1))
      (recur (+ dp (* (first a1) (first b1))) (rest a1) (rest b1))
      dp)))


