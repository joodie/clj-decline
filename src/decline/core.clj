(ns decline.core
  {:doc "clj-decline: A dynamic validation system

clj-decline is built on two concepts: error sets and validations.

* An error set is a map of key -> sets of messages.

* A validation is a function that, given some arguments to validate,
 returns an error set or nil.

You can construct a validation from a predicate using the `validation'
function.

Validations that take the same arguments can be merged using the
`validations' function."})

(defn err
  "Construct an error set with key and one or more messages.
Messages and keys can be any type."
  [key & msgs]
  {key (into #{} msgs)})

(defn merge-errors
  "Merge error sets. If a key is already associated with messages,
adds the new messages to that set."
  [& sets]
  (apply merge-with (partial reduce conj) sets))

(defn validation
  "Make a validation `v' from `predicate'. If (predicate args*) is false,
(v args*) returns `errors' as a single error set, nil otherwise."
  [predicate & errors]
  (fn [& args]
    (if-not (apply predicate args)
      (apply merge-errors errors))))

(defn validations
  "Merge validations `fns' that all take the same arguments into a
single validation function."
  [& fns]
  (fn [& args]
    (reduce (fn [errors f]
              (if-let [e (apply f args)]
                (merge-errors errors e)
                errors))
            nil
            fns)))

(defn validate-val
  "Make a validation that takes a single argument `object' from a
`key' and a `predicate'. Returns an error set if `predicate' doesnt
match the value associated with `key' in `object'."
  [key predicate & errors]
  (apply validation #(predicate (get % key)) errors))
