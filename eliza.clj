                                        ;eliza clojure implementation
(def single-valued? (complement coll?))
 ; no such thing as atom predicate in clojure (atom means a completely different thing)
 ; collection is essentially the inverse of atom

(defn simple-equal [x y]
  ; Are x and y equal? (Don't check inside strings.)
  (if  (or (or (single-valued? x) (= () x)) (or (single-valued? y) (= () y)))
    (= x y)
    (and (simple-equal (first x) (first y))
         (simple-equal (rest x) (rest y)))))


;(or (single-valued? x) (= () x))

   ;seems like () might be posing a problem
