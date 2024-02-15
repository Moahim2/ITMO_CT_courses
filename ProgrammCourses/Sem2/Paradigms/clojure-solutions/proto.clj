; This file should be placed in clojure-solutions
; You may use it via (load-file "proto.clj")

(defn proto-get
  "Returns object property respecting the prototype chain"
  ([obj key] (proto-get obj key nil))
  ([obj key default]
   (cond
     (contains? obj key) (obj key)
     (contains? obj :prototype) (proto-get (obj :prototype) key default)
     :else default)))

(defn proto-call
  "Calls object method respecting the prototype chain"
  [this key & args]
  (apply (proto-get this key) this args))

(defn field [key]
  "Creates field"
  (fn
    ([this] (proto-get this key))
    ([this def] (proto-get this key def))))

(defn method
  "Creates method"
  [key] (fn [this & args] (apply proto-call this key args)))

(defn constructor
  "Defines constructor"
  [ctor prototype]
  (fn [& args] (apply ctor {:prototype prototype} args)))

(defmacro deffield
  "Defines field"
  [name]
  (let [key (keyword (subs (str name) 1))]
    `(defn ~name
       ([this#] (proto-get this# ~key))
       ([this# def#] (proto-get this# ~key def#)))))

(defmacro deffields
  "Defines multiple fields"
  [& names]
  `(do ~@(mapv (fn [name] `(deffield ~name)) names)))

(defmacro defmethod
  "Defines method"
  [name]
  (let [key (keyword (subs (str name) 1))]
    `(defn ~name [this# & args#] (apply proto-call this# ~key args#))))

(defmacro defmethods
  "Defines multiple methods"
  [& names]
  `(do ~@(mapv (fn [name] `(defmethod ~name)) names)))

(defn to-symbol [name] (symbol (str "_" name)))
(defmacro defconstructor
  "Defines constructor"
  [name fields prototype]
  `(do
     (deffields ~@(map to-symbol fields))
     (defn ~name ~fields
       (assoc {:prototype ~prototype}
         ~@(mapcat (fn [f] [(keyword f) f]) fields)))))

(defmacro defclass
  "Defines class"
  [name super fields & methods]
  (let [-name (fn [suffix] (fn [class] (symbol (str class "_" suffix))))
        proto-name (-name "proto")
        fields-name (-name "fields")
        method (fn [[name args body]] [(keyword name) `(fn ~(apply vector 'this args) ~body)])
        base-proto (if (= '_ super) {} {:prototype (proto-name super)})
        prototype (reduce (fn [m nab] (apply assoc m (method nab))) base-proto methods)
        public-prototype (proto-name name)
        public-fields (fields-name name)
        all-fields (vec (concat (if (= '_ super) [] (eval (fields-name super))) fields))]
    `(do
       (defmethods ~@(map (comp to-symbol first) methods))
       (deffields ~@(map to-symbol fields))
       (def ~public-prototype ~prototype)
       (def ~public-fields (quote ~all-fields))
       (defconstructor ~name ~all-fields ~public-prototype))))
