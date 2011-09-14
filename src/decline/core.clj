(ns decline.core
  "clj-decline: A dynamic validation system

clj-decline is built on two concepts: error sets and validations.

* An error set is a map of key -> seq of messages.

* A validation is a function that, given some arguments to validate,
 returns an error set or nil.

You can construct a validation from a predicate using the `validation'
function.

Validations that take the same arguments can be merged using the
`validations' and `validate-some' functions.")

(defn merge-errors
  "Merge error sets. If a key is already associated with messages,
concat new messages."
  [& sets]
  (apply merge-with concat sets))

(defn validations
  "Merge validations `fns' that all take the same arguments into a
single validation function. Runs all validations."
  [& fns]
  (fn [& args]
    (apply merge-errors
           (map #(apply % args) fns))))

(defn validate-some
  "Merge validations `fns', returns the first error encountered."
  [& fns]
  (fn [& args]
    (some #(apply % args) fns)))

(defn validation
  "Make a validation `v' from `predicate'. If (predicate args*) is false,
 (v args*) returns error set `errors', nil otherwise."
  [predicate errors]
  (fn [& args]
    (if-not (apply predicate args)
      errors)))

(defn validate-val
  "Make a validation that takes one or more arguments given a `key'
and a `predicate'. Returns `errors' if `predicate' is false for the
value associated with `key' in the first argument.

The reason additional arguments are ignored is that this makes it
easier to use the validation in a set of (validate new old) tests,
where the typical case is just to test the new object."
  [key predicate errors]
  (validation (fn [arg & _] (predicate (get arg key))) errors))
