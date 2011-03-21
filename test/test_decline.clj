(ns test-decline
  (:use clojure.test
        decline.core))

(deftest test-merge
  (is (= (merge-errors {} {:e [:msg]})
         {:e [:msg]}))
  (is (= (merge-errors {:e [:msg1]}
                       {:e [:msg2]})
         {:e [:msg1 :msg2]}))
  (is (= (merge-errors {:e1 [:msg1]}
                       {:e2 [:msg2]}
                       {:e1 [:msg3]})
         {:e1 [:msg1 :msg3]
          :e2 [:msg2]}))
  (is (= (merge-errors {:e [:msg :m2]})
         {:e [:msg :m2]})))

(deftest test-validation
  (let [v (validation even? {:num [:not-even]})]
    (is (= (v 1)
           {:num [:not-even]}))
    (is (= (v 2)
           nil))))

(deftest test-validations
  (let [check (validations
               (validate-val :name seq
                             {:name [:empty]})
               (validate-val :num integer?
                             {:num [:not-a-number]})
               (validation #(= "my secret" (:secret (:hidden %)))
                         {:secret [:no-secret]}))]
    (is (= (check {:name ""
                   :num "not a number"
                   :hidden {:secret "my secret"}})
             {:name [:empty]
              :num  [:not-a-number]}))
    (is (= (check {:name "ok"
                   :num "not a number"})
             {:num [:not-a-number]
              :secret [:no-secret]}))

    (is (= (check {:name "name"
                   :num 12
                   :hidden {:secret "my secret"}})
           nil))))

(deftest test-multiple-errors
  (let [check (validate-val :name seq
                            {:val [:required :seq]})]
    (is (= (check {:name "str"})
           nil))
    (is (= (check {:foo :bar})
           {:val [:required :seq]}))))


(deftest test-new-old
  (let [check (validations
               (validate-val :name seq {:name [:blank]})
               (validation #(= (:fixed %1) (:fixed %2))
                           {:fixed [:cannot-change]}))]
    (is (= (check {:name "new name"
                   :fixed "something"}
                  {:name "name"
                   :fixed "something"})
           nil))
    (is (= (check {:name ""
                   :fixed "something else"}
                  {:name "bla"
                   :fixed "something"})
           {:name  [:blank]
            :fixed [:cannot-change]}))))

(deftest test-some
  (let [check (validations
               (validate-some
                (validation seq {:err [:seq]})
                (validation string? {:err [:string]}))
               (validation #(< 3 (count %)) {:err [:count]}))]
    (is (= (check []) {:err [:seq :count]}))
    (is (= (check [1 2 3 4]) {:err [:string]}))
    (is (= (check "1234") nil))))