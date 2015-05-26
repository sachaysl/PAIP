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

;(defn variable-p [x]
  ;; is x a variable (a symbol beginning with '?')?
 ; (and (symbol? x) (.startsWith (name x) "?")))

(defn variable-p [x]
  (if (and (list? x) (unarylength? x))
    (and (symbol? (first x)) (.startsWith (name (first x)) "?"))
    (and (symbol? x) (.startsWith (name x) "?"))))

(defn unarylength? [x]
  (if (= (rest x) ()) true false))

(def rest1 [x]
  (if (= (rest (rest x)) ()) (first (rest x)) (rest x)))


(defn pat-match0 [pattern input]
  (if (variable-p pattern)
     ;the second argument to cons must be a list in clojure
    (if (list? input) (cons pattern input) (cons pattern (list input)))
    (if  (or (or (single-valued? pattern) (= () pattern)) (or (single-valued? input) (= () input)))
      (= pattern input)
      (concat (pat-match0 (first pattern) (first input))
           (pat-match0 (rest pattern) (rest input))))))
    
;(clojure.string/replace '(what would it mean to you if you got a ?X ?) "?X" "vacation") the above is an attempt to find an equaivalent of sublis in clojure. The replace function
; works but requires the match and the replacement to be strings.

(def ^:const fail false ) ;indicates pat-match failure

(def ^:const no-bindings '((true true))) ;indicates pat-match success with no variables

(defn get-binding [var bindings]
 ;Find a (variable value) pair in a binding list
  (assoc1 var bindings))

;assoc behaves differently in clojure than in common lisp so have to implement it ourselves
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

(defn pat-match [pattern input bindings];&{:keys [bindings] :or {bindings no-bindings}}]
  ;print bindings Note, need to make bindings an optional argument here
  ;(pat-match 'hey 'hey :bindings '((?X vacation)))
  (cond
    (= bindings fail) fail
    (variable-p pattern) (match-variable pattern input bindings)
    (= pattern input) bindings
    (and (list? pattern) (list? input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings))
         :else fail))
(defn match-variable [var input bindings]
  ;Does VAR match input? Uses (or updates) and returns bindings.
  (let [binding (get-binding var bindings)] (print var) (print input)
    (cond
      (not binding) (if (list? var) (extend-bindings (first var) input bindings)
                        (extend-bindings var input bindings))
      (or (= input (binding-val binding)) (= (list input) (binding-val binding)))
      bindings
      :else fail)))
;POSSIBLE BUG HERE, BINDING-VAL BINDING RETURNS A LIST, INPUT COULD BE JUST A VALUE SO    COULD BE COMPARING (HEY) AND HEY, AND GET A FALSE, EVEN THOUGH WE WANT THEM TO MATCH
                                        ;CHECK THE LISP IMPLEMENTATION TO SEE IF THIS IS CONSISTENT OR NOT.
;This is indeed a problem, sorted it with a quick hack to check input and (list input)
;against the value returned by binding-val binding. Should change this to check if input
;is a list and if not make it a list and then compare with value returned from
;binding-val binding to avoid any unwanted side effects.



(defn pat-match1 [pattern input]
  (if (variable-p pattern)
     ;the second argument to cons must be a list in clojure
    (cons1 pattern input)
    (if  (or (atom1 pattern) (atom1 input))
      (= pattern input)
      (concat (pat-match1 (first pattern) (first input))
           (pat-match1 (rest pattern) (rest input))))))

(defn atom1 [pattern]
  (or (single-valued? pattern) (= () pattern)))

(defn cons1 [pattern input]
  (if (list? input) (cons pattern input) (cons pattern (list input))))
        
