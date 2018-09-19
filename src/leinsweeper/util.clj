(ns leinsweeper.util
  (:require [clojure.spec.alpha :as s]))

(s/def ::game-status #(::in-progress ::won ::lost))

(s/def ::cell-status #{::untouched ::flagged ::revealed})

(s/def ::has-mine boolean?)

(s/def ::cell
  (s/keys :req [
    ::has-mine
    ::cell-status]))

(s/def ::width pos-int?)
(s/def ::height pos-int?)
(s/def ::mine-count pos-int?)
(s/def ::cells (s/coll-of (s/coll-of ::cell)))

(s/def ::minefield
  (s/and
    (s/keys
      :req [::width
            ::height
            ::mine-count
            ::cells])
    #(= (::width %) (count (::cells %)))
    (fn [m] (not-any? #(not= (::height m) (count %)) (::cells m)))
    #(= (::mine-count %) (count (filter ::has-mine (reduce concat (::cells %)))))
    #(< (::mine-count %) (* (::width %) (::height %)))))

(defn valid-coords? [minefield ^long x ^long y]
  (and (>= x 0) (>= y 0) (> (::width minefield) x) (> (::height minefield) y)))

(defn get-cell [minefield ^long x ^long y]
  (assert (valid-coords? minefield x y))
  (nth (nth (::cells minefield) x) y))

(defn generate-minefield [^long width ^long height ^long mine-count]
  (assert (< mine-count (* width height)) "Too many mines for grid size!")
  (hash-map ::width width
            ::height height
            ::mine-count mine-count
            ::cells (reduce
                      (fn [cells [x y]] (assoc-in cells [x y ::has-mine] true))
                      (vec (repeat width (vec (repeat height (hash-map ::has-mine false
                                                                       ::cell-status ::untouched)))))
                      (take mine-count (shuffle (for [x (range width) y (range height)] [x y]))))))

(defn get-game-status [minefield]
  (s/assert ::minefield minefield)
  (let [cells (reduce concat (::cells minefield))]
    (cond
      (some #(and
               (= ::revealed (::cell-status %))
               (::has-mine %)) cells
            ) ::lost
      (some #(or
               (not= (= ::flagged (::cell-status %)) (::has-mine %))
               (and (not= ::revealed (::cell-status %)) (not (::has-mine %)))) cells
            ) ::in-progress
      :otherwise ::won)))

(defn in-progress? [minefield] (= ::in-progress (get-game-status minefield)))

(defn assert-in-progress [minefield] (assert (in-progress? minefield) "The game is over"))

(defn get-adjacent-cell-coords [minefield ^long x ^long y]
  (assert (valid-coords? minefield x y))
  (filter (fn [[adjx adjy]]
            (and (not= [adjx adjy] [x y]) (valid-coords? minefield adjx adjy))) (for [diffx [-1 0 1] diffy [-1 0 1]] [(+ x diffx) (+ y diffy)])))

(defn get-adjacent-mine-count [minefield ^long x ^long y]
  (count (vec (filter (fn [[adjx adjy]]
                        (::has-mine (get-cell minefield adjx adjy))) (get-adjacent-cell-coords minefield x y)))))

(defn reveal-cell [minefield ^long x ^long y]
  (assert-in-progress minefield)
  (let [cell (get-cell minefield x y)]
    (if (= (::cell-status cell) ::revealed)
      minefield
      (let [revealed (assoc-in minefield [::cells x y ::cell-status] ::revealed)]
        (if (and (not (::has-mine cell)) (= 0 (get-adjacent-mine-count revealed x y)))
          (reduce (fn [current [adjx adjy]] (if (= ::untouched (::cell-status (get-cell current adjx adjy)))(reveal-cell current adjx adjy) current)) revealed (get-adjacent-cell-coords revealed x y))
          revealed)))))

(defn flag-cell [minefield ^long x ^long y]
  (assert-in-progress minefield)
  (assert (not= ::revealed (::cell-status (get-cell minefield x y))) "Cannot flag revealed cell")
  (assoc-in minefield [::cells x y ::cell-status] ::flagged))

(defn unflag-cell [minefield ^long x ^long y]
  (assert-in-progress minefield)
  (assert (= ::flagged (::cell-status (get-cell minefield x y))) "Cannot unflag unflagged cell")
  (assoc-in minefield [::cells x y ::cell-status] ::flagged))
