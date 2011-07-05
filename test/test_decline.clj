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

(deftest test-parent-key
  (let [error {:num [:not-a-number]}
        nested-errors {:num [:not-a-number]
                       [:nested2 :name] [:empty]}
        add-parent-key (ns-resolve 'decline.core 'add-parent-key)]
    (is (= (add-parent-key error :nested)
           {[:nested :num] [:not-a-number]}))
    (is (= (add-parent-key nested-errors :nested1)
           {[:nested1 :num] [:not-a-number]
            [:nested1 :nested2 :name] [:empty]}))
    (is (= (add-parent-key nil :nested) nil))))

(deftest test-nested
  (let [check (validations
               (validate-val :name seq
                             {:name [:empty]})
               (validate-nested :nested
                                (validate-val :num integer?
                                              {:num [:not-a-number]})))]
    (is (= (check {:name "name"
                   :nested {:num 1}}) nil))
    (is (= (check {:name ""
                   :nested {:num 1}}) {:name [:empty]}))
    (is (= (check {:name ""
                   :nested {:num "not a number"}})
           {:name [:empty]
            [:nested :num] [:not-a-number]}))))

(deftest test-spec
  (let [not-empty? (fn [param] (validate-val param seq
                                           {param [:empty]}))
        int? (fn [param] (validate-val param integer?
                                      {param [:not-a-number]}))
        spec {:name not-empty?
              :num int?}
        check (validation-spec spec)]
    (is (= (check {:name "name"
                   :num 1}) nil))
    (is (= (check {:name ""
                   :num "not a number"})
           {:name [:empty] :num [:not-a-number]}))
    (let [spec-nested {:name not-empty?
                       :num int?
                       :nested1 {:val1 int?
                                 :num1 int?
                                 :nested2 {:name2 not-empty?
                                           :val2 int?}}}
          check (validation-spec spec-nested)]
      (is (= (check {:name "name"
                     :num 1
                     :nested1 {:val1 2
                               :num1 3
                               :nested2 {:name2 "name"
                                         :val2 4}}})
             nil))
      (is (= (check {:name ""
                 :num "not-a-number"
                 :nested1 {:val1 "not-a-number"
                           :num1 "not-a-number"
                           :nested2 {:name2 ""
                                     :val2 "not-a-number"}}})
             {:name [:empty]
              :num [:not-a-number]
              [:nested1 :val1] [:not-a-number]
              [:nested1 :num1] [:not-a-number]
              [:nested1 :nested2 :name2] [:empty]
              [:nested1 :nested2 :val2] [:not-a-number]})))))

(deftest test-spec-some
  (let [not-empty? (fn [param] (validate-val param seq
                                            {param [:empty]}))
        uppercase? (fn [param]
                     (validate-val param
                      (fn [s] (every? #(java.lang.Character/isUpperCase %) s))
                      {param [:not-uppercase]}))
        spec {:name [not-empty? uppercase?]}
        check (validation-spec spec)]
    (is (= (check {:name ""})
           {:name [:empty]}))
    (is (= (check {:name "name"})
           {:name [:not-uppercase]}))
    (is (= (check {:name "NAME"})
           nil))))
