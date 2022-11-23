(ns week-cycle.api-test
  (:require #?@(:clj [[clojure.test :refer :all]
                      [clojure.spec.alpha :as s]
                      [clojure.spec.gen.alpha :as gen]]
                :cljs [[cljs.test :refer-macros [deftest is testing]]
                       [cljs.spec.alpha :as s]
                       [cljs.spec.gen.alpha :as gen]])
            [tick.core :as t]
            [week-cycle.api :as w]))

; TODO https://github.com/bbatsov/clojure-style-guide#testing

(deftest duration
  (let [d1 (t/new-date 2018 1 1)
        d2 (t/new-date 2018 1 8)]
    (testing "Duration calculation"
      (is (= (w/in-weeks d1 d2) 1))
      (is (= (w/in-weeks d1 d1) 0))
      (is (= (w/in-weeks d2 d1) -1)))))

(deftest cycle-functions
  ; Reference data from old Excel system
  (testing "Date calculation"
    (is (= (w/cycle-date 2016 1 0) (t/new-date 2016 2 1)))
    (is (= (w/cycle-date 2017 8 2) (t/new-date 2017 8 28)))
    (is (= (w/cycle-date 2018 3 1) (t/new-date 2018 4 2)))
    (is (= (w/cycle-date 2019 5 1) (t/new-date 2019 6 24))))
  (testing "Cycle identification"
    (is (= [2016 1 0 0] (w/cycle-id (t/new-date 2016 2 1))))
    (is (= [2017 8 2 0] (w/cycle-id (t/new-date 2017 8 28))))
    (is (= [2018 3 1 0] (w/cycle-id (t/new-date 2018 4 2))))
    (is (= [2019 5 1 1] (w/cycle-id (t/new-date 2019 6 25))))
    (is (= [2020 0 0 0] (w/cycle-id (w/cycle-date 2019 13 0)))))
  (testing "Labels"
    (is (= (w/cycle-label 2018 10) "Oct-Nov")) ; Excel says "Oct" but week 4 reaches into November
    (is (= (w/cycle-label 2019 0) "Jan-Feb"))
    (is (= (w/cycle-label 2019 12) "Dec-Jan 2020"))))
