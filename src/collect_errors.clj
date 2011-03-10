(ns collect-errors
  ^{:doc "A dynamic validation system"}
  (:use clojure.test))

(defn err
  "Construct an error with key and one or more messages."
  [key & msgs]
  {key (into #{} msgs)})

(defn- merge-errors*
  "Merge error sets"
  ([set]
     set)
  ([set1 set2]
     (reduce (fn [coll [key msgs]]
               (assoc coll key (if-let [existing (get coll key)]
                                 (into msgs existing)
                                 msgs)))
             set1
             set2)))

(defn- merge-errors
  "Merge error sets"
  ([set & sets]
     (reduce merge-errors* set sets)))

(deftest test-merge
  (is (= (merge-errors {} (err :e :msg))
         (err :e :msg)))
  (is (= (merge-errors (err :e :msg1)
                       (err :e :msg2))
         {:e #{:msg1 :msg2}}))
  (is (= (merge-errors (err :e1 :msg1)
                       (err :e2 :msg2)
                       (err :e1 :msg3))
         {:e1 #{:msg1 :msg3}
          :e2 #{:msg2}})))

(defn validate
  "Make a validator from a test. If (test args*) is false,
return errors"
  [test & errors]
  (fn [& args]
    (if-not (apply test args)
      (apply merge-errors errors))))

(deftest test-validate
  (let [v (validate even? (err :num :not-even))]
    (is (= (v 1)
           (err :num :not-even)))
    (is (= (v 2)
           nil))))

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
  (validations* merge-errors {} fns))

(deftest test-validations
  (let [check (validations
               (validate-val :name seq
                             (err :name :empty))
               (validate-val :num integer?
                             (err :num :not-a-number))
               (validate #(= "my secret" (:secret (:hidden %)))
                         (err :secret :no-secret)))]
    (is (= (check {:name ""
                   :num "not a number"
                   :hidden {:secret "my secret"}})
             {:name #{:empty}
              :num  #{:not-a-number}}))
    (is (= (check {:name "ok"
                   :num "not a number"})
             {:num #{:not-a-number}
              :secret #{:no-secret}}))))

(deftest test-multiple-errors
  (let [check (validate-val :name seq
                            (err :val :required)
                            (err :val :seq))]
    (is (= (check {:name "str"})
           nil))
    (is (= (check {:foo :bar})
           {:val #{:required :seq}}))))
