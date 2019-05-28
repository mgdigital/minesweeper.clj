(ns minesweeper.ui
    (:require [minesweeper.engine :as engine]
            [quil.core :as q]
            [quil.middleware :as m]))

(def width 10)
(def height 10)
(def mine-count 20)
(def cell-size 30)
(def cell-padding 2)
(def cell-offset (+ cell-size cell-padding))
(def area-width (- (* width cell-offset) cell-padding))
(def area-height (- (* height cell-offset) cell-padding))

(defn update-state [state] (identity state))

(def game-status-colors {
                         ::engine/in-progress [240 240 240]
                         ::engine/won [100 255 100]
                         ::engine/lost [255 100 100]})

(def cell-status-colors {
                         ::engine/untouched [0 0 255]
                         ::engine/flagged [255 255 0]
                         ::engine/revealed [0 255 0]
                         })

(defn get-cell-color [cell]
  (let [status (::engine/cell-status cell)]
    (if (and (::engine/has-mine cell) (= status ::engine/revealed)) [255 0 0] (status cell-status-colors))))

(defn draw-state [state]
  (let [game-status (engine/get-game-status state)]
    (apply q/background (game-status game-status-colors))
    (dorun
      (map
        (fn [[x y]]
          (let [cell (engine/get-cell state x y)]
            (q/with-fill (get-cell-color cell)
                         (q/rect (* cell-offset x) (* cell-offset y) cell-size cell-size))
            (if (= ::engine/revealed (::engine/cell-status cell))
              (q/with-fill [0 0 0] (q/text
                                     (if
                                       (and (= ::engine/lost game-status) (::engine/has-mine cell))
                                       "M"
                                       (str (engine/get-adjacent-mine-count state x y)))
                                     (* cell-offset x)
                                     (- (* cell-offset y) 3)
                                     cell-offset
                                     cell-offset)))))
        (for [x (range (::engine/width state)) y (range (::engine/height state))] [x y]))
      )
    ))

(defn convert-coords [m x y]
  "Returns corresponding minefield coordinates for clicked cell coordinates"
  (let [width (::engine/width m)
        height (::engine/height m)
        cellx (int (/ x cell-offset))
        celly (int (/ y cell-offset))]
    (if (and (< cellx width) (< celly height)) [cellx celly])))

(defn handle-cell-clicked [m x y b]
  (cond
    (not= ::engine/in-progress (engine/get-game-status m)) m
    :otherwise (let [cell (engine/get-cell m x y)
                     cell-status (::engine/cell-status cell)]
                 (cond
                   (= cell-status ::engine/revealed) m
                   :otherwise (case b
                                :right (engine/reveal-cell m x y)
                                :left (case cell-status
                                        ::engine/flagged (engine/unflag-cell m x y)
                                        ::engine/untouched (engine/flag-cell m x y))
                                m)))))

(defn handle-click [state event]
  (let [coords (convert-coords state (:x event) (:y event))]
    (if coords (let [[x y] coords]
                 (handle-cell-clicked state x y (:button event))) state)))

(defn -main [& args]
  (let [initial-minefield (engine/generate-minefield width height mine-count)]
    (q/defsketch quilsweeper
                 :title "Minesweeper"
                 :size [area-width area-height]
                 ; setup function called only once, during sketch initialization.
                 :setup #(do
                           (q/color-mode :rgb)
                           (q/text-align :center :center)
                           (identity initial-minefield))
                 ; update-state is called on each iteration before draw-state.
                 :update update-state
                 :draw draw-state
                 :features [:keep-on-top]
                 :mouse-clicked handle-click
                 :middleware [m/fun-mode])
    ))
