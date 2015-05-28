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

;(def rest1 [x]
;  (if (= (rest (rest x)) ()) (first (rest x)) (rest x)))


;(defn pat-match0 [pattern input]
 ; (if (variable-p pattern)
  ;   ;the second argument to cons must be a list in clojure
   ; (if (list? input) (cons pattern input) (cons pattern (list input)))
    ;(if  (or (or (single-valued? pattern) (= () pattern)) (or (single-valued? input) (= ;() input)))
 ;     (= pattern input)
  ;    (concat (pat-match0 (first pattern) (first input))
   ;        (pat-match0 (rest pattern) (rest input))))))
    
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
    (segment-pattern-p pattern) (segment-match pattern input bindings 0) ; ***
    (and (list? pattern) (list? input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings))
         :else fail))


(defn segment-pattern-p [pattern]
 ;is this a segment matching pattern: ((?* var) pat)
 ; check if 'first pattern gives a symbol and the below conditions, else return falseA 
  (and (list? pattern) (list? (first pattern))
       (.startsWith (name (first (first pattern))) "?*")))
   

(defn segment-match [pattern input bindings start]
  ;Match the segment pattern ((?* var) pat) against input
  (let [var (second (first pattern))
        pat (rest pattern)]
    (if (= pat ()) ;not sure if this is a correct translation of the lisp yet
      (match-variable var input bindings)
      ; we assume that pat starts with a constant
      ; In other words, a pattern can't have two consecutive vars
      (let [ pos (returnposition (first pat) input start)]
        (if (false? pos)
          fail
          (let [b2 (pat-match pat
                              (subseq1 input pos)
                              (match-variable var (subseq2 input 0 pos) bindings))]
            ;;if this match failed,try another longer one
            (if (= b2 fail)
              (segment-match pattern input bindings (+ pos 1))
              b2)))))))


(defn subseq2 [input start end]
  (loop [input input
         start start
         result '()]
    (if (or (nil? input) (> start end)) false
        (if (= start end)
          (reverse result)
          (recur (seq (rest input))
                 (+ start 1)
                 (cons (first input) result))))))  


            
;check if the core clojure function subseq is suitable to replace subseq1
(defn subseq1 [input pos] 
  (loop [input input
         pos pos]
    (if (nil? input) false
        (if (= pos 0)
          input
          (recur (seq (rest input))
                 (- pos 1))))))
                

(defn returnposition [target input searchfrom]
  (loop [input input
         pos 0]
    ;(print pos)
    (if (nil? input) false
         (if (and (= (first input) target) (>= pos searchfrom))
           pos
           (recur (seq (rest input))
                  (+ pos 1))))))

(defn match-variable [var input bindings]
  ;Does VAR match input? Uses (or updates) and returns bindings.
  (let [binding (get-binding var bindings)]; (print var) (print input)
    (cond
      (not binding) (if (list? var)
                      (extend-bindings (first var)
                                       (if (list? input) input (list input))
                                       bindings)
                      (extend-bindings var
                                       (if (list? input) input (list input)) 
                                       bindings))
      (or (= input (binding-val binding)) (= (list input) (binding-val binding)))
      bindings
      :else fail)))
;POSSIBLE BUG HERE, BINDING-VAL BINDING RETURNS A LIST, INPUT COULD BE JUST A VALUE SO    COULD BE COMPARING (HEY) AND HEY, AND GET A FALSE, EVEN THOUGH WE WANT THEM TO MATCH
                                        ;CHECK THE LISP IMPLEMENTATION TO SEE IF THIS IS CONSISTENT OR NOT.
;This is indeed a problem, sorted it with a quick hack to check input and (list input)
;against the value returned by binding-val binding. Should change this to check if input
;is a list and if not make it a list and then compare with value returned from
;binding-val binding to avoid any unwanted side effects.



;(defn pat-match1 [pattern input]
 ; (if (variable-p pattern)
   ;  ;the second argument to cons must be a list in clojure
  ;  (cons1 pattern input)
    ;(if  (or (atom1 pattern) (atom1 input))
     ; (= pattern input)
      ;(concat (pat-match1 (first pattern) (first input))
       ;    (pat-match1 (rest pattern) (rest input))))))

(defn atom1 [pattern]
  (or (single-valued? pattern) (= () pattern)))

(defn cons1 [pattern input]
  (if (list? input) (cons pattern input) (cons pattern (list input))))
        

     ; 5.4 onwards 'The Eliza Program: A Rule-Based Translator


;"Now that we have a working pattern matcher we need some patterns to match.
;what's more we want the patterns to be associated with responses. We can do
;this by inventing a data structure called a rule, which consists of a pattern and one
;or more associated responses."

;"We will choose the simplest possible implementation for rules: as lists where the
;first element is the pattern and the rest is a list of responses."

(defn rule-pattern [rule]
  (first rule))

(defn rule-responses [rule]
  (rest rule))


(def ^:dynamic  *eliza-rules* ;dynamic vs not dynamic?
  '((((?* ?x) hello (?* ?y))
     (How do you do.  Please state your problem.))
    (((?* ?x) computer (?* ?y))
     (Do computers worry you?) (What do you think about machines?)
     (Why do you mention computers?)
     (What do you think machines have to do with your problem?))
    (((?* ?x) name (?* ?y))
     (I am not interested in names))
    (((?* ?x) sorry (?* ?y))
     (Please don't apologize) (Apologies are not necessary)
     (What feelings do you have when you apologize))
    (((?* ?x) I remember (?* ?y))
     (Do you often think of ?y)
     (Does thinking of ?y bring anything else to mind?)
     (What else do you remember) (Why do you recall ?y right now?)
     (What in the present situation reminds you of ?y)
     (What is the connection between me and ?y))
    (((?* ?x) do you remember (?* ?y))
     (Did you think I would forget ?y ?)
     (Why do you think I should recall ?y now)
     (What about ?y) (You mentioned ?y))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))

    (((?* ?x) I dreamt (?* ?y))
     (Really-- ?y) (Have you ever fantasized ?y while you were awake?)
     (Have you dreamt ?y before?))
    (((?* ?x) dream about (?* ?y))
     (How do you feel about ?y in reality?))
    (((?* ?x) dream (?* ?y))
     (What does this dream suggest to you?) (Do you dream often?)
     (What persons appear in your dreams?)
     (Don't you believe that dream has to do with your problem?))
    (((?* ?x) my mother (?* ?y))
     (Who else in your family ?y) (Tell me more about your family))
    (((?* ?x) my father (?* ?y))
     (Your father) (Does he influence you strongly?)
     (What else comes to mind when you think of your father?))

    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) I am glad (?* ?y))
     (How have I helped you to be ?y) (What makes you happy just now)
     (Can you explain why you are suddenly ?y))
    (((?* ?x) I am sad (?* ?y))
     (I am sorry to hear you are depressed)
     (I'm sure its not pleasant to be sad))
    (((?* ?x) are like (?* ?y))
     (What resemblance do you see between ?x and ?y))
    (((?* ?x) is like (?* ?y))
     (In what way is it that ?x is like ?y)
     (What resemblance do you see?)
     (Could there really be some connection?) (How?))
    (((?* ?x) alike (?* ?y))
     (In what way?) (What similarities are there?))
    (((?* ?x) same (?* ?y))
     (What other connections do you see?))

    (((?* ?x) I was (?* ?y))
     (Were you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) was I (?* ?y))
     (What if you were ?y ?) (Do you thin you were ?y)
     (What would it mean if you were ?y))
    (((?* ?x) I am (?* ?y))
     (In what way are you ?y) (Do you want to be ?y ?))
    (((?* ?x) am I (?* ?y))
     (Do you believe you are ?y) (Would you want to be ?y)
     (You wish I would tell you you are ?y)
     (What would it mean if you were ?y))
    (((?* ?x) am (?* ?y))
     (Why do you say "AM?") (I don't understand that))
    (((?* ?x) are you (?* ?y))
     (Why are you interested in whether I am ?y or not?)
     (Would you prefer if I weren't ?y)
     (Perhaps I am ?y in your fantasies))
    (((?* ?x) you are (?* ?y))
     (What makes you think I am ?y ?))

    (((?* ?x) because (?* ?y))
     (Is that the real reason?) (What other reasons might there be?)
     (Does that reason seem to explain anything else?))
    (((?* ?x) were you (?* ?y))
     (Perhaps I was ?y) (What do you think?) (What if I had been ?y))
    (((?* ?x) I can't (?* ?y))
     (Maybe you could ?y now) (What if you could ?y ?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))
    (((?* ?x) I (?* ?y) you (?* ?z))
     (Perhaps in your fantasy we ?y each other))
    (((?* ?x) why don't you (?* ?y))
     (Should you ?y yourself?)
     (Do you believe I don't ?y) (Perhaps I will ?y in good time))
    (((?* ?x) yes (?* ?y))
     (You seem quite positive) (You are sure) (I understand))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative)
     (Are you saying "NO" just to be negative?))

    (((?* ?x) someone (?* ?y))
     (Can you be more specific?))
    (((?* ?x) everyone (?* ?y))
     (surely not everyone) (Can you think of anyone in particular?)
     (Who for example?) (You are thinking of a special person))
    (((?* ?x) always (?* ?y))
     (Can you think of a specific example) (When?)
     (What incident are you thinking of?) (Really-- always))
    (((?* ?x) what (?* ?y))
     (Why do you ask?) (Does that question interest you?)
     (What is it you really want to know?) (What do you think?)
     (What comes to your mind when you ask that?))
    (((?* ?x) perhaps (?* ?y))
     (You do not seem quite certain))
    (((?* ?x) are (?* ?y))
     (Did you think they might not be ?y)
     (Possibly they are ?y))
    (((?* ?x))
     (Very interesting) (I am not sure I understand you fully)
     (What does that suggest to you?) (Please continue) (Go on)
         (Do you feel strongly about discussing such things?))))


   ;"Finally we are ready to implement Eliza proper. The main program should be
   ;a loop that reads input, transforms it, and prints the result. Transformation is
   ;done primarily by finding some rule such that its pattern matches the input,
   ;and then substituting the variables into the rule's responses.

 
(defn eliza []
  ;Respond to user input using pattern matching rules.
  (while true
    (print 'eliza>)
    (print (use-eliza-rules (read)))))
                                        ;should pretty print and flatten here (see norvig)





(defn use-eliza-rules [input]
  ;find some rule with which to transform the input
  (some (fn [rule]
          (let [result (pat-match (rule-pattern rule) input '((t t)))]
            (print result)
            (if (not (= result fail))
              (sublis result ;SHOULD BE (sublis (switch-viewpoint result))
                      (random-elt (rule-responses rule))))))
        *eliza-rules*))
;(clojure.string/replace '(what would it mean to you if you got a ?X ?) "?X" "vacation")

(defn sublis [substitutes sentence]
  (loop [sentence sentence
         substitutes substitutes]
    (if (= nil substitutes)
      sentence
      (recur (clojure.string/replace sentence
                                     (stringify (first (first substitutes)))
                                     (stringify (second (first substitutes))))
             (seq (rest substitutes))))))




;BUG BELOW (switch viewpoint) --> words is a list of list pairs,our sublis does not handle those yet, invent new function
;FIGURE OUT WHY READ BUFFER ISN'T WORKING PROPERLY (RESTART REPL)
;nb disable switch-viewpoint method in use-eliza-rules for the time being

(defn switch-viewpoint [words] ;change I to you and vice versa. and so on
  (sublis1 '((I You) (you I) (me you) (am Are) (are am))
          words))

(defn sublis1 [substitutes pairlists]
  (loop [pairlists pairlists
         substitutes substitutes]
    (if (= nil substitutes)
      pairlists
      (recur (clojure.string/replace pairlists
                                     (stringify (first (first substitutes)))
                                     (stringify (first (second (first substitutes)))))
             (seq (rest substitutes))))))
      


(defn stringify [input] ;take a symbol or list and return a string 
  (if (list? input)
    (clojure.string/join " " input)
    (clojure.string/join " " (list input))))

(defn random-elt [choices] ;choose an element from a list at random
  (elt choices (rand-int (length choices))))


(defn length [choices] ; returns length of list
  (loop [choices choices
         tlength 0]
    (if (= nil choices)
      tlength
      (recur (seq (rest choices))
             (+ tlength 1)))))

(defn elt [choices number]
  (loop [choices choices
         number number]
         (if (= number 0)
           (first choices)
           (recur (seq (rest choices))
                  (- number 1)))))

; start program by typing (eliza)
; make sure you put brackets around everthing you say
;press enter to submit
