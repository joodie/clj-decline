(ns test-decline
  (:use clojure.test
        decline.core))

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

(deftest test-validate
  (let [v (validate even? (err :num :not-even))]
    (is (= (v 1)
           (err :num :not-even)))
    (is (= (v 2)
           nil))))

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
