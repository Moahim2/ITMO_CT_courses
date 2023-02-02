(chapter "Macro")

(section "Showcase")
(example "Code generation"
         (defmacro infix [[a op b]]
           (list op a b))
         (macroexpand '(infix (10 + 20)))
         (infix (10 + 20)))

(example "Syntax quote"
         (defmacro infix [[a op b]]
           `(~op ~a ~b))
         (macroexpand '(infix (10 + 20)))
         (infix (10 + 20)))

(example "Recursion"
         (defmacro infix [v]
           (cond
             (list? v) (let [[a op b] v] `(~op (infix ~a) (infix ~b)))
             :else v))
         (infix (10 + 20))
         (macroexpand '(infix (10 + (a * 3))))
         (let [a 2] (infix (10 + (a * 3)))))

(section "JS-like Objects")
(example "Single field"
         (defmacro deffield
           "Defines field"
           [name]
           `(def ~name (field ~(keyword (subs (str name) 1)))))
         (macroexpand '(deffield _x))
         (deffield _x)
         (_x {:x 100})
         (_x {})
         (_x {} 100))

(example "Multiple fields"
         (defmacro deffields
           "Defines multiple fields"
           [& names]
           `(do ~@(map (fn [name] `(deffield ~name)) names)))
         (macroexpand '(deffields _x _y))
         (deffields _x _y)
         (_x {:x 100})
         (_y {:y 200}))

(example "Single method"
         (defmacro defmethod
           "Defines method"
           [name]
           `(def ~name (method ~(keyword (subs (str name) 1)))))
         (macroexpand '(defmethod _getX))
         (defmethod _getX)
         (_getX {:getX (fn [this] 10)})
         (defmethod _add)
         (_add {:add (fn [this a b] (+ a b))} 10 20))

(example "Multiple methods"
         (defmacro defmethods
           "Defines multiple methods"
           [& names]
           `(do ~@(map (fn [name] `(defmethod ~name)) names)))
         (macroexpand '(defmethods _getX _getY))
         (defmethods _getX _getY)
         (_getX {:getX (fn [this] 10)})
         (_getY {:getY _y :y 20}))

(example "Constructors"
         (defn to-symbol [name] (symbol (str "_" name)))
         (defmacro defconstructor
           "Defines constructor"
           [name fields prototype]
           `(do
              (deffields ~@(map to-symbol fields))
              (defn ~name ~fields
                (assoc {:prototype ~prototype}
                  ~@(mapcat (fn [f] [(keyword f) f]) fields)))))
         (macroexpand '(defconstructor Point [x y] PointPrototype)))

(example "Classes"
         (defmacro defclass
           "Defines class"
           [name super fields & methods]
           (let [-name (fn [suffix] (fn [class] (symbol (str class "_" suffix))))
                 proto-name (-name "proto")
                 fields-name (-name "fields")
                 method (fn [[name args body]] [(keyword name) `(fn [~'this ~@args] ~body)])
                 base-proto (if (= '_ super) {} {:prototype (proto-name super)})
                 prototype (apply assoc base-proto (mapcat method methods))
                 public-prototype (proto-name name)
                 public-fields (fields-name name)
                 all-fields (vec (concat (if (= '_ super) [] (eval (fields-name super))) fields))]
             `(do
                (defmethods ~@(map (comp to-symbol first) methods))
                (deffields ~@(map to-symbol fields))
                (def ~public-prototype ~prototype)
                (def ~public-fields (quote ~all-fields))
                (defconstructor ~name ~all-fields ~public-prototype))))
         (macroexpand '(defclass Point _ [x y]
                                 (getX [] (_x this))
                                 (getY [] (_y this))
                                 (setX [x] (assoc this :x x))
                                 (setY [y] (assoc this :y y)))))

(example "Point"
         (defclass Point _ [x y]
                   (getX [] (_x this))
                   (getY [] (_y this))
                   (setX [x] (assoc this :x x))
                   (setY [y] (assoc this :y y))
                   (sub [that] (Point (- (_getX this) (_getX that))
                                      (- (_getY this) (_getY that))))
                   (length [] (let [square #(* % %)] (Math/sqrt (+ (square (_getX this)) (square (_getY this))))))
                   (distance [that] (_length (_sub this that))))
         (_length (Point 3 4))
         (_distance (Point 5 5) (Point 1 2))
         (_getX (_setX (Point 3 4) 100)))

(example "Shifted point"
         (defclass ShiftedPoint Point [dx dy]
                   (getX [] (+ (_x this) (_dx this)))
                   (getY [] (+ (_y this) (_dy this)))
                   (setDX [dx] (assoc this :dx dx))
                   (setDY [dy] (assoc this :dy dy)))
         (_distance (ShiftedPoint 2 2 3 3) (Point 1 2))
         (_getX (_setX (ShiftedPoint 10 20 1 2) 100)))
