; This file should be placed in clojure-solutions
; You may use it via (load-file "parser.clj")

(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)


(defn _empty [value] (partial -return value))

(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))

(defn _map [f]
  (fn [result]
    (if (-valid? result)
      (-return (f (-value result)) (-tail result)))))

(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        ((_map (partial f (-value ar)))
          ((force b) (-tail ar)))))))

(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))

(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0001})) (str input \u0001)))))
(mapv (_parser (_combine str (_char #{\a \b}) (_char #{\x}))) ["ax" "ax~" "bx" "bx~" "" "a" "x" "xa"])



(defn +char [chars] (_char (set chars)))
(defn +char-not [chars] (_char (comp not (set chars))))
(defn +map [f parser] (comp (_map f) parser))
(def +ignore (partial +map (constantly 'ignore)))

(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))

(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))

(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))

(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))

(defn +or [p & ps]
  (reduce (partial _either) p ps))

(defn +opt [p]
  (+or p (_empty nil)))

(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))

(defn +plus [p] (+seqf cons p (+star p)))

(defn +str [p] (+map (partial apply str) p))

(def +parser _parser)


(defn +collect [defs]
  (cond
    (empty? defs) ()
    (seq? (first defs)) (let [[[key args body] & tail] defs]
                          (cons
                            {:key key :args args :body body}
                            (+collect tail)))
    :else (let [[key body & tail] defs]
            (cons
              {:key key :args [] :synth true :body body}
              (+collect tail)))))

(defmacro defparser [name & rules]
  (let [collected (+collect rules)
        keys (set (map :key (filter :synth collected)))]
    (letfn [(rule [{key :key, args :args, body :body}] `(~key ~args ~(convert body)))
            (convert [value]
              (cond
                (seq? value) (map convert value)
                (char? value) `(+char ~(str value))
                (keys value) `(~value)
                :else value))]
      `(def ~name (letfn ~(mapv rule collected) (+parser (~(:key (last collected)))))))))
