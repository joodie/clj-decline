(ns decline.core
  {:doc "A dynamic validation system"})

(defn err
  "Construct an error with key and one or more messages."
  [key & msgs]
  {key (into #{} msgs)})

(defn merge-errors
  "Merge error sets"
  [& sets]
  (reduce (partial merge-with (partial reduce conj)) sets))

(defn validate
  "Make a validator from a test. If (test args*) is false,
return errors"
  [test & errors]
  (fn [& args]
    (if-not (apply test args)
      (apply merge-errors errors))))

(defn validate-val
  "make a validator from a key k and a predicate.
returns errors if pred doesnt match the value if k in object."
  [k pred & errors]
  (apply validate #(pred (get % k)) errors))

(defn- validations*
  "Create a validator that collects any errors into errors using (collector errors error)."
  [collector errors fns]
  (fn [& args]
    (reduce (fn [es f]
              (if-let [e (apply f args)]
                (collector es e)
                es))
            errors
            fns)))

(defn validations
  "merge validation fns into a single validation.
validations should return a map of key -> set of errors or nil, which
  you can construct using the `err' function."
  [& fns]
  (validations* merge-errors nil fns))
