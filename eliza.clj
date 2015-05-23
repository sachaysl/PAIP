                                        ;eliza clojure implementation
(def single-valued? (complement coll?))
 ; no such thing as atom predicate in clojure (atom means a completely different thing)
 ; collection is essentially the inverse of atom except we must
 ; also handle () 

(defn simple-equal [x y]
  ; Are x and y equal? (Don't check inside strings.)
  (if  (or (or (single-valued? x) (= () x)) (or (single-valued? y) (= () y)))
    (= x y)
    (and (simple-equal (first x) (first y))
         (simple-equal (rest x) (rest y)))))

(defn variable-p [x]
  ; is x a variable (a symbol beginning with '?')?
  (and (symbol? x) (.startsWith (name x) "?")))
       ; check the clojure equivalent of symbol-name

(defn pat-match [pattern input]
  ;Does pattern match input? Any variable can match anything.
  (if (variable-p pattern)
    true
    (if  (or (or (single-valued? pattern) (= () pattern)) (or (single-valued? input) (= () input)))
      (= pattern input)
      (and (pat-match (first pattern) (first input))
           (pat-match (rest pattern) (rest input))))))
    
