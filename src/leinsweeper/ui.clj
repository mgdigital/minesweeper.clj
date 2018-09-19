(ns leinsweeper.ui
  (:require [leinsweeper.util :as util]
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
                         ::util/in-progress [240 240 240]
                         ::util/won [100 255 100]
                         ::util/lost [255 100 100]})

(def cell-status-colors {
                         ::util/untouched [0 0 255]
                         ::util/flagged [255 255 0]
                         ::util/revealed [0 255 0]
                         })

(defn get-cell-color [cell]
  (let [status (::util/cell-status cell)]
    (if (and (::util/has-mine cell) (= status ::util/revealed)) [255 0 0] (status cell-status-colors))))

(defn draw-state [state]
  (let [game-status (util/get-game-status state)]
    (apply q/background (game-status game-status-colors))
    (dorun
      (map
        (fn [[x y]]
          (let [cell (util/get-cell state x y)]
            (q/with-fill (get-cell-color cell)
                         (q/rect (* cell-offset x) (* cell-offset y) cell-size cell-size))
            (if (= ::util/revealed (::util/cell-status cell))
              (q/with-fill [0 0 0] (q/text
                                     (if
                                       (and (= ::util/lost game-status) (::util/has-mine cell))
                                       "M"
                                       (str (util/get-adjacent-mine-count state x y)))
                                     (* cell-offset x)
                                     (- (* cell-offset y) 3)
                                     cell-offset
                                     cell-offset)))))
        (for [x (range (::util/width state)) y (range (::util/height state))] [x y]))
      )
    ))

(defn convert-coords [m x y]
  "Returns corresponding minefield coordinates for clicked cell coordinates"
  (let [width (::util/width m)
        height (::util/height m)
        cellx (int (/ x cell-offset))
        celly (int (/ y cell-offset))]
    (if (and (< cellx width) (< celly height)) [cellx celly])))

(defn handle-cell-clicked [m x y b]
  (cond
    (not= ::util/in-progress (util/get-game-status m)) m
    :otherwise (let [cell (util/get-cell m x y)
                     cell-status (::util/cell-status cell)]
                 (cond
                   (= cell-status ::util/revealed) m
                   :otherwise (case b
                                :right (util/reveal-cell m x y)
                                :left (case cell-status
                                        ::util/flagged (util/unflag-cell m x y)
                                        ::util/untouched (util/flag-cell m x y))
                                m)))))

(defn handle-click [state event]
  (let [coords (convert-coords state (:x event) (:y event))]
    (if coords (let [[x y] coords]
                 (handle-cell-clicked state x y (:button event))) state)))

(defn -main [& args]
  (let [initial-minefield (util/generate-minefield width height mine-count)]
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
                 ; This sketch uses functional-mode middleware.
                 ; Check quil wiki for more info about middlewares and particularly
                 ; fun-mode.
                 :mouse-clicked handle-click
                 :middleware [m/fun-mode])
    ))
