(ns leinsweeper.util-test
  (:require [clojure.test :refer :all]
            [leinsweeper.util :as util]
            [clojure.spec.alpha :as s]))

(deftest generate
  (testing "Generating minefield")
    (is (s/valid? ::util/minefield (util/generate-minefield 10 10 5))))

(deftest win
  (testing "Winning the game")
  (is (= ::util/won
         (util/get-game-status
           (reduce
             (fn [m [x y]]
               (let [cell (util/get-cell m x y)]
                 (if (= ::util/untouched (::util/cell-status cell))
                   (if (::util/has-mine cell)
                     (util/flag-cell m x y)
                     (util/reveal-cell m x y))
                   m)))
             (util/generate-minefield 10 10 5)
             (for [x (range 10) y (range 10)] [x y]))))))

(deftest lose
  (testing "Losing the game")
  (let [initial-minefield (util/generate-minefield 10 10 5)
        [mine-x mine-y] (first (take 1 (filter (fn [[x y]] (::util/has-mine (util/get-cell initial-minefield x y))) (for [x (range 10) y (range 10)] [x y]))))
        final-minefield (util/reveal-cell initial-minefield mine-x mine-y)]
    (is (= ::util/lost (util/get-game-status final-minefield)))))

(deftest unflag
  (testing "Flagging and unflagging cells")
  (let [initial-minefield (util/generate-minefield 10 10 5)
        flagged-minefield (util/flag-cell initial-minefield 0 0)
        unflagged-minefield (util/unflag-cell flagged-minefield 0 0)]
    (is (= ::util/untouched (::util/cell-status (util/get-cell unflagged-minefield 0 0))))))
