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
      

(defn pat-match [pattern input]
  ;Does pattern match input? WARNING: buggy version.
  (if (variable-p pattern)
     ;the second argument to cons must be a list in clojure
    (if (list? input) (cons pattern input) (cons pattern (list input)))
    (if  (or (or (single-valued? pattern) (= () pattern)) (or (single-valued? input) (= () input)))
      (= pattern input)
      (concat (pat-match (first pattern) (first input))
           (pat-match (rest pattern) (rest input))))))
    
;(clojure.string/replace '(what would it mean to you if you got a ?X ?) "?X" "vacation") the above is an attempt to find an equaivalent of sublis in clojure. The replace function
; works but requires the match and the replacement to be strings.

(def ^:const fail false ) ;indicates pat-match failure

(def ^:const no-bindings '((true true))) ;indicates pat-match success with no variables

(defn get-binding [var bindings]
 ;Find a (variable value) pair in a binding list
  (assoc1 var bindings))

;assoc behaves differently in clojure to common lisp so have to define it ourselves
(defn assoc1 [var bindings]
   (loop [var var
          bindings bindings]
     (if (nil? bindings) false
         (if (= (first (first bindings)) var) (first bindings)
             (recur var
                    (seq (rest bindings)))))))

(defn binding-val [binding]
  ;Get the value part of a single binding
  (rest binding))

(defn lookup [var bindings]
  ;Get the value part (for var) from a binding list.
  (binding-val (get-binding var bindings)))

;note to self: lookup gives error if var doesn't exist (look at get-binding function)

(defn extend-bindings [var val bindings]
  ;Add a (var value) pair to a binding list.
  (cons (cons var (list val)) bindings))
