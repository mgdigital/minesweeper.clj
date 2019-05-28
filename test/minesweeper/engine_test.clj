(ns minesweeper.engine-test
  (:require [clojure.test :refer :all]
            [minesweeper.engine :as engine]))

(deftest unflag
  (testing "Flagging and unflagging cells")
  (let [initial-minefield (engine/generate-minefield 10 10 5)
        flagged-minefield (engine/flag-cell initial-minefield 0 0)
        unflagged-minefield (engine/unflag-cell flagged-minefield 0 0)]
    (is (= ::engine/untouched (::engine/cell-status (engine/get-cell unflagged-minefield 0 0))))))

(deftest win
  (testing "Winning the game")
  (is (= ::engine/won
         (engine/get-game-status
           (reduce
             (fn [m [x y]]
               (let [cell (engine/get-cell m x y)]
                 (if (= ::engine/untouched (::engine/cell-status cell))
                   (if (::engine/has-mine cell)
                     (engine/flag-cell m x y)
                     (engine/reveal-cell m x y))
                   m)))
             (engine/generate-minefield 10 10 5)
             (for [x (range 10) y (range 10)] [x y]))))))

(deftest lose
  (testing "Losing the game")
  (let [initial-minefield (engine/generate-minefield 10 10 5)
        [mine-x mine-y] (first (take 1 (filter (fn [[x y]] (::engine/has-mine (engine/get-cell initial-minefield x y))) (for [x (range 10) y (range 10)] [x y]))))
        final-minefield (engine/reveal-cell initial-minefield mine-x mine-y)]
    (is (= ::engine/lost (engine/get-game-status final-minefield)))))
