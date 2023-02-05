(defn abstractExpression [operation] (fn[& args]
                                       (fn f[m]
                                         (cond
                                           (string? (first args)) (m (first args))
                                           :else (apply operation (mapv #(% m) args))))))

(def constant constantly)
(def variable (abstractExpression identity))
(def add (abstractExpression +))
(def subtract (abstractExpression -))
(def multiply (abstractExpression *))
(def divide (abstractExpression (fn [& args] (cond
                                               (some zero? args) (first args)
                                               :else (apply / args)))))

(def pow (abstractExpression (fn[a, b] (Math/pow a b))))
(def log (abstractExpression (fn[a, b] (/ (Math/log (Math/abs b)) (Math/log (Math/abs a))))))
(def negate (abstractExpression -))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(load-file "proto.clj")

(def _operation (field :op))
(def _args (field :argums))
(def _symbol (field :symbol))
(def _diffImpl (field :diffImpl))

(def evaluate (method :eval))
(def toString (method :toStr))
(def diff (method :df))
(def toStringSuffix (method :toStrSuf))

;Abstract
(def AbstractOpPrototype
  {:eval (fn [this m] (apply (_operation this) (mapv #(evaluate % m) (_args this))))
   :toStr (fn [this] (str "(" (_symbol this) " " (clojure.string/join " " (mapv toString (_args this))) ")"))
   :df (fn [this str] ((_diffImpl this) this (mapv #(diff % str) (_args  this))))
   :toStrSuf (fn [this] (str "(" (clojure.string/join " " (mapv toStringSuffix (_args this))) " " (_symbol this) ")"))
   })
(defn AbstractOp [operation sym, diffImpl] (fn [this & args]
  (assoc this
    :op operation
    :argums args
    :symbol sym
    :diffImpl diffImpl
    )))
(defn _AbstractOp [operation sym diffImpl] (constructor (AbstractOp operation sym diffImpl) AbstractOpPrototype))
;;;Abs

;Const
(def Constant)
(def ConstantPrototype
  {:eval     (fn [this _] (_args this))
   :toStr    (fn [this] (str (_args this)))
   :df       (fn [_ _] (Constant 0))
   :toStrSuf toString
   })
(defn ConstantImpl [this arg]
  (assoc this
    :op constantly
    :argums arg
    ))
(def Constant (constructor ConstantImpl ConstantPrototype))
;;;C

;Variable
(def VariablePrototype
  {:eval (fn [this m] (m (str (Character/toLowerCase (first (_args this))))))
   :toStr (fn [this] (_args this))
   :df (fn [this s] (cond
                          (= s (_args this)) (Constant 1)
                          :else (Constant 0)))
   :toStrSuf toString
   })
(defn VariableImpl [this arg]
  (assoc this
    :op identity
    :argums arg
    ))
(def Variable (constructor VariableImpl VariablePrototype))
;;;V

(def Add (_AbstractOp + "+" (fn [_, args] (apply Add args))))
(def Subtract (_AbstractOp - "-" (fn [_, args] (apply Subtract args))))
(def Negate (_AbstractOp - "negate" (fn [_, args] (Negate (first args)))))
(def Multiply)

;ImplFunc
(defn mulDiff [this, args]
  (cond
    (= 1 (count args)) (Multiply (first args))
    :else (Add
            (Multiply
              (first args) (apply Multiply (rest (_args this))))
            (Multiply
              (first (_args this))
              (mulDiff
                (apply Multiply (rest (_args this)))
                (rest args))))))

(def Multiply (_AbstractOp * "*" mulDiff))
(def Divide (_AbstractOp
              (fn [& args] (cond
                             (some zero? args) (first args)
                             :else (apply / args)))
              "/"
             (fn [this, args]
                             (cond
                               (= 1 (count args)) (Negate
                                                    (Divide
                                                      (first args)
                                                      (Multiply (first (_args this)) (first (_args this)))))
                               :else (Divide
                                       (Subtract
                                         (Multiply
                                           (first args)
                                           (apply Multiply (rest (_args this))))
                                         (Multiply
                                           (first (_args this))
                                           (mulDiff
                                             (apply Multiply (rest (_args this)))
                                             (rest args))))
                                       (Multiply
                                         (apply Multiply (rest (_args this)))
                                         (apply Multiply (rest (_args this)))))))))
;
(def Log (_AbstractOp (fn[a, b]
                        (/ (Math/log (Math/abs b)) (Math/log (Math/abs a))))
                      "log"
                      (fn [this args]
                        (Divide
                          (Subtract
                            (Multiply (Multiply (Divide (second (_args this))) (second args))
                                      (Log (Constant Math/E) (first (_args this))))
                            (Multiply (Log (Constant Math/E) (second (_args this)))
                                      (Multiply (Divide (first (_args this))) (first args))))
                          (Multiply
                            (Log (Constant Math/E) (first (_args this)))
                            (Log (Constant Math/E) (first (_args this))))))))
(def Pow (_AbstractOp (fn[a, b] (Math/pow a b)) "pow" (fn [this args]
                                                        (Multiply (apply Pow (_args this))
                                                                  (Add
                                                                    (Multiply (Log (Constant Math/E)
                                                                                   (first (_args this)))
                                                                              (second args))
                                                                    (Multiply (Multiply (Divide (first (_args this)))
                                                                                        (first args))
                                                                              (second (_args this))))))))
;;;CombParseMode(34-35)Constructors

(defn bitOperateImpl [bit-func] (fn [a b] (cond
                                            (or (double? a) (double? b)) (Double/longBitsToDouble (bit-func
                                                                             (Double/doubleToLongBits (double a))
                                                                             (Double/doubleToLongBits (double b))))
                                            :else (bit-func a b))))
(def BitAnd (_AbstractOp
              (bitOperateImpl bit-and)
              "&"
              nil))

(def BitOr (_AbstractOp
             (bitOperateImpl bit-or)
              "|"
              nil))

(def BitXor (_AbstractOp
              (bitOperateImpl bit-xor)
              "^"
              nil))
;
;;Parse
(def mapOperations {'+ {:first add :second Add}
                    '- {:first subtract :second Subtract}
                    '* {:first multiply :second Multiply}
                    '/ {:first divide :second Divide}
                    'negate {:first negate :second Negate}
                    'pow {:first pow :second Pow}
                    'log {:first log :second Log}
                    '& {:first nil :second BitAnd}
                    '| {:first nil :second BitOr}
                    (symbol "^") {:first nil :second BitXor}
                    })

(defn parseImpl [type] (fn [input]
  (let [mapVars {'x {:first (variable "x") :second (Variable "x")}
                 'y {:first (variable "y") :second (Variable "y")}
                 'z {:first (variable "z") :second (Variable "z")}
                 }
        mapConst {:first constant :second Constant}
        ]
    (letfn
      [(parseImpl [operation]
         (cond
           (number? operation) ((type mapConst) operation)
           (contains? mapVars operation) (type (mapVars operation))
           :else (apply (type (mapOperations (first operation))) (map parseImpl (rest operation)))
           ))
       ]
      (parseImpl (read-string input))))))
(def parseFunction (parseImpl :first))
(def parseObject (parseImpl :second))


;;;;;;;CombParser
(load-file "parser.clj")

(def parseObjectSuffix
  (let
    [*all-chars (mapv char (range 0 128))
     *digit (+or (+char (apply str (filter #(Character/isDigit %) *all-chars))) (+char "."))
     *space (+char (apply str (filter #(Character/isWhitespace %) *all-chars)))
     *ws (+ignore (+star *space))
     *var (+map (comp Variable str) (+map (partial apply str) (+plus (+char "XxYyzZ"))))
     *constant (+map (comp Constant read-string) (+seqf
                                                   (partial apply str)
                                                   (+opt (+char "-"))
                                                   (+plus *digit)))
     *symbolOperation (+or
                        (+char "+-*/&|^")
                        (+seqf (constantly 'negate) (+char "n") (+char "e") (+char "g") (+char "a") (+char "t") (+char "e"))
                        (+seqf (constantly 'pow) (+char "p") (+char "o") (+char "w"))
                        (+seqf (constantly 'log) (+char "l") (+char "o") (+char "g")))]
    (letfn [(*arguments [] (+star (+seqn 0 *ws (delay (*value)) *ws)))
            (*operation []
              (+seqn
                1
                (+char "(") *ws
                (+map
                  #(apply ((comp :second mapOperations symbol str) (second %)) (first %))
                  (+seq (*arguments) *symbolOperation))
                *ws (+char ")")))
            (*value [] (+or *constant *var (*operation)))]
      (+parser (+seqn 0 *ws (*value) *ws)))))


;(println (evaluate(BitAnd (Constant 5) (Constant 6.0)) {"x" 2}))
;(println (str(Character/toUpperCase (first "xxx"))))
;(println (evaluate (Variable "Xxxxxx") {"x" 2}))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(+seqn 1 (+char "(")
;  (+map (partial apply Add) (+star (+seqn 0 *ws (delay (*value)) *ws)))
;  *ws
;  *symbolOperation
; *ws
;      (+char ")")))
;(+f (+map (fn [op] #(apply op %))(+map mapOper (last seqc))) (first seqc))
;(+seqn 0 (+map (partial apply Add) seqc) *symbolOperation)))
;(+map (partial apply Add) (+map first seqc))

;(defn _f [p] (
;fn [result]
;(if (and (-valid? p) (ifn? (-value p))) ((_map (-value p)) result))
;))
;(defn +f [p1 p2] (comp (_f p1) p2))
;(defn +-f [p1 p2] (+f p2 p1))
