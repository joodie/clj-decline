(ns collect-errors
  (:use clojure.test))

(defn validations*
  "Create a validator that collects any errors into errors.
Default collector is `conj'. Default `errors' is {}"
  ([collector errors fns]
     (fn [& args]
       (reduce (fn [es f]
                 (if-let [e (apply f args)]
                   (collector es e)
                   es))
               errors
               fns)))
  ([errors fns]
     (validations* conj errors fns))
  ([fns]
     (validations* conj {} fns)))

(defn- merge-errors
  "Add errors to any existing errors for key in coll."
  [coll [key & errors]]
  (assoc coll key (concat errors (get coll key))))

(defn keyed-validations
  [& fns]
  (validations* merge-errors {} fns))

(defn validate
  [test err]
  "Make a validator from a test. If (test args*) false,
return err"
  (fn [& args]
    (if-not (apply test args)
      err)))

(defn validate-val
  "make a validator from a key k and a predicate.
returns err if pred doesnt match the value if k in object."
  [k pred err]
  (validate #(pred (get % k)) err))

(deftest test-check
  (let [check (keyed-validations
               (validate-val :name seq [:name :empty])
               (validate-val :num integer? [:num :not-a-number])
               (validate #(= "my secret" (:secret (:hidden %))) [:secret :no-secret]))]
    (is (= (check {:name ""
                   :num "not a number"
                   :hidden {:secret "my secret"}})
             {:name [:empty]
              :num  [:not-a-number]}))
    (is (= (check {:name "ok"
                   :num "not a number"})
             {:num [:not-a-number]
              :secret [:no-secret]}))))

