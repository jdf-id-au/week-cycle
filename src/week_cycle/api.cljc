(ns week-cycle.api
  "Calculate n-week cycle where n=4."
  (:require [tick.alpha.api :as t]
            #?@(:cljs [["@js-joda/locale_en" :refer [Locale]]]))
  (:import #?@(:clj [(java.util Locale)]))
  (:refer-clojure :exclude [format]))

; Setup

(def n "Cycle length in weeks: four!" 4)

(def epoch-monday
  "First day of 2016 clinical year happened to be start of week one (and of cycle two!)."
  (t/new-date 2016 2 1))

; Support

(defn int-day-of-week
  "Monday = 1 – Sunday = 7 unless platforms change their enums! ISO-8601"
  [d] (-> d t/day-of-week #?(:clj .getValue :cljs .value)))

(defn in-weeks [d1 d2] (.until d1 d2 (tick.core/unit-map :weeks)))

(defn format [f d]
  ; FIXME handle locale
  (t/format (tick.format/formatter f
                                   #?(:clj Locale/ENGLISH
                                      :cljs (.-ENGLISH Locale))) d))

(def month-abbr (partial format "MMM"))

; Calculation

(defn first-monday
  "First Monday of given year."
  [given-year]
  (let [year-start (t/new-date given-year 1 1)]
    (t/+ year-start (t/new-period (mod (- 8 (int-day-of-week year-start)) 7) :days))))

(defn cycle-week
  "Cycle-week of given date. **ZERO BASED**"
  [given-date]
  (let [monday (t/- given-date (t/new-period (-> given-date int-day-of-week dec) :days))]
    (mod (in-weeks epoch-monday monday) 4)))

(defn first-week
  "Cycle-week of first Monday of given year. **ZERO BASED**"
  [given-year]
  (cycle-week (first-monday given-year)))

(defn first-week-one
  "Monday of first cycle-week one of given year. **ZERO BASED** i.e. 'week one' = 0."
  [given-year]
  (t/+ (first-monday given-year)
       (t/new-period (mod (- n (first-week given-year)) n) :weeks)))

(defn cycle-date
  "Date corresponding to given (year, cycle, ?week, ?day).
   Cycle, week and day are **ZERO BASED** here; convert to familiar one-based values only in UI.
   Day is therefore NOT ISO-8601. Monday is 0.
   Start 'cycle one' on the first 'week one' of the year, ignoring the clinical year."
  ([cycle-year cycle week day]
   (t/+ (first-week-one cycle-year)
        (t/new-period (+ (* n cycle) week) :weeks)
        (t/new-period day :days)))
  ([cycle-year cycle week] (cycle-date cycle-year cycle week 0))
  ([cycle-year cycle] (cycle-date cycle-year cycle 0 0)))

(defn cycle-id
  "Canonical (year, cycle, week, day) for given date or given (year, cycle, ?week, ?day).
   Cycle, week and day are **ZERO BASED**. Day is therefore NOT ISO-8601. Monday is 0.
   Days before the first 'week one' of the year belong to the previous year's last cycle."
  ([given-date]
   (let [given-year (t/int (t/year given-date))
         canonical-year (if (t/< given-date (first-week-one given-year))
                          (- given-year 1)
                          given-year)]
     [canonical-year
      (quot (in-weeks (first-week-one canonical-year) given-date) n)
      (cycle-week given-date)
      (-> given-date int-day-of-week dec)]))
  ([given-year given-cycle given-week given-day]
   (cycle-id (cycle-date given-year given-cycle given-week given-day)))
  ([given-year given-cycle given-week]
   (cycle-id (cycle-date given-year given-cycle given-week)))
  ([given-year given-cycle]
   (cycle-id (cycle-date given-year given-cycle))))

(defn cycle-label
  "Cycle is **ZERO BASED**."
  [cycle-year cycle]
  (let [w1-monday (cycle-date cycle-year cycle 0)
        w1-month (t/month w1-monday)
        wn-friday (cycle-date cycle-year cycle (dec n) 4)
        wn-month (t/month wn-friday)
        wn-year (t/int (t/year wn-friday))]
    (cond-> (month-abbr w1-monday)
            (not= w1-month wn-month) (str "-" (month-abbr wn-friday))
            (> wn-year cycle-year) (str " " wn-year))))