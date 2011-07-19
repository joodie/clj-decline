(ns decline.core
  {:doc "clj-decline: A dynamic validation system

clj-decline is built on two concepts: error sets and validations.

* An error set is a map of key -> seq of messages.

* A validation is a function that, given some arguments to validate,
 returns an error set or nil.

You can construct a validation from a predicate using the `validation'
function.

Validations that take the same arguments can be merged using the
`validations' and `validate-some' functions."}
  (:use [clojure.walk :only [postwalk]]))

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

(defn- add-parent-key
  "Adds the parent-key to all keys of the errors map.
   If the error key is already a vector the parent-key
   is added to the front of the existing error key vector."
  [errors parent-key]
  (if errors
    (into {} (map #(let [[k v] %]
                     (if (vector? k)
                       [(into [parent-key] k) v]
                       [[parent-key k] v])) errors))
    nil))

(defn validate-nested
  "Allows to specify a validation for a nested map. Example:
   (def validate (validate-nested :nested
                   (validate-val :num integer?
                                 {:num [:not-a-number]})))
   >(validate {:nested {:num \"not-a-number\"}})
    {[:nested :num] [:not-a-number]}

   The key of the error is the path to the nested value, so it
   can be used as second parameter for get-in for example."
  [key validate-fn]
  (fn [m]
    (add-parent-key
     (validate-fn (get m key))
     key)))

(defn- wrap-required [spec f]
  (validations
   (fn [x]
     (let [spec-keys (keys spec)
           missing-keys (filter #(= ::not-found (get x % ::not-found)) spec-keys)
           required-errors
               (into {} (map #(identity [% [:required]]) missing-keys)) ]
       (when (seq required-errors) required-errors)
       ))
   f))

(defn- wrap-not-specified [spec f]
  (validations
   (fn [x]
     (let [spec-keys (into #{} (keys spec))
           not-specified-keys
               (map key (remove #(spec-keys (key %)) x))
           not-specified-errors
               (into {} (map #(identity [% [:not-specified]]) not-specified-keys))]
       (when (seq not-specified-keys) not-specified-errors)
       ))
   f))

(defn wrap-optional [spec f]
  (let [remove-questionmark #(keyword (apply str (drop 1 (name %))))
        optional-keyword? #(and (keyword? (key %))
                                (.startsWith (name (key %)) "?"))
        optional-keywords (into #{}
                                (map
                                 (comp remove-questionmark key)
                                 (filter optional-keyword? spec)))
        optional-spec-keys
           (into #{}
                 (map key (filter optional-keyword? spec)))
        spec
           (into {} (map
                      #(let [[k v] %]
                         (if (optional-keyword? %)
                           [(remove-questionmark k)
                            v]
                           %)) spec))
        remove-required-errors
           (fn [result]
             (into {}
                   (remove nil?
                           (map #(let [[k v] %]
                                   (if (and (keyword? k)
                                            (optional-keywords k))
                                     (if (some (fn [error]
                                                 (= error :required)) v)
                                       nil
                                       %)
                                     %)) result))))
        empty-map->nil #(if (seq %) % nil)]
    (validations
     (comp empty-map->nil remove-required-errors (f spec)))))

(defn- flat-validation-spec [spec]
  (let [no-op (fn [x] nil)
        vector-of-fns? #(and (vector? %)
                             (every? fn? %))]
    (wrap-optional
     spec
     (fn [spec]
       (wrap-not-specified
        spec
        (wrap-required
         spec
         (apply validations
                (map (fn [[param value]]
                       (if (fn? value)
                         (value param)
                         (if vector-of-fns?
                           (apply validate-some
                                  (map #(% param) value))
                           no-op))) spec))))))))

(defn validation-spec [spec]
  "Allows to specify the validations in an ordinary map:
   >(def not-empty? (fn [param] (validate-val param seq
                                           {param [:empty]})))
   >(def int? (fn [param] (validate-val param integer?
                                      {param [:not-a-number]})))
   >(let [spec {:name not-empty? :num int?}
          check (validation-spec spec)]
     (check {:name \"\"
                   :num \"not a number\"}))
   {:name [:empty] :num [:not-a-number]}

   It is also possible to specify validations for nested
   elements of the map:
   >(let [spec {:name not-empty?
                :nested {:val int?}}
          check (validation-spec spec)]
     (check {:name \"\" :nested {:val \"not-a-number\"}}))
   {[:nested :val] [:not-a-number], :name [:empty]}

   The error key for a nested element is the path to it in the
   map, so it can be used as second parameter for get-in for
   example. If the value of a map entry is a vector of validation
   functions (something like {:name [not-empty? uppercase?]}) then
   these functions are composed via validate-some.
   All entries in a spec are required by default. If a map which
   should be validated don't contain all keys of the spec map, then
   the :required error is added to the error sequence of the
   corresponding key in the validation result:

   >(def check (validation-spec {:name not-empty?}))
   >(check {})
   {:name (:required :empty)}

   On the other hand, all additional entries in the map which should
   be validated lead to a :not-specified error:
   >(check {:name \"name\" :unknown 123})
   {:unknown [:not-specified]}

   Keys can also be optional, therefor its keyword in the spec map
   has to start with a questionmark:

   >(def check (validation-spec {:?name not-empty? :num int?}))
   >(= (check {:num 1}) nil)
   true
   "
  [spec]
  (let [nested-spec? #(and (vector? %)
                                 (map? (second %)))
        flat-spec (postwalk
                   (fn [x]
                     (if (nested-spec? x)
                       (let [[k v] x
                             v (fn [param]
                                 (validate-nested
                                  param
                                  (flat-validation-spec v)))]
                         [k v])
                       x)) spec)]
    (flat-validation-spec flat-spec)))
