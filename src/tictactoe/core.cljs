(ns tictactoe.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rd]
   [cljs.test :refer-macros [deftest is]]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defn new-board [n]
  (vec (repeat n (vec (repeat n "B")))))

(def board-size 3)
(def win-length 3)

(defonce app-state
  (atom {:text "Welcome to tic tac toe!"
         :board (new-board board-size)
         :game-status :in-progress}))

(defn blank? [c] (= c "B"))

(defn computer-move [board]
  (let [remaining-spots (for [i (range board-size)
                              j (range board-size)
                              :when (= (get-in board [i j]) "B")]
                          [i j])
        move (rand-nth remaining-spots)]
    (assoc-in board move "C")))

(deftest computer-move-test
  (is (= [["C"]]
         (computer-move [["B"]])))
  (is (= [["P"]]
         (computer-move [["P"]]))))

(defn straight [owner board [x y] [dx dy] n]
  (every? true?
          (for [i (range n)]
            (= (get-in board [(+ (* dx i) x)
                              (+ (* dy i) y)])
               owner))))

(defn win? [owner board n]
  (some true? (for [i (range board-size)
                    j (range board-size)
                    dir [[1 0] [0 1] [1 1] [1 -1]]]
                (straight owner board [i j] dir n))))

(deftest win?-test
  (is (win? "P" [["P"]] 1))
  (is (not (win? "P" [["P"]] 2)))
  (is (win? "P" [["C" "P"]
                 ["P" "C"]] 2)))

(defn full? [board]
  (every? #{"P" "C"} (apply concat board)))

(deftest full?-test
  (is (not (full? [["P" "B"]])))
  (is (full? [["P" "C"]])))

(defn game-status [board]
  (cond
    (win? "P" board win-length) :player-victory
    (win? "C" board win-length) :computer-victory
    (full? board) :draw
    :else :in-progress))

(defn get-key [i j]
  (+ i (* j board-size)))

(defn rect-click [i j]
  (when (= (:game-status @app-state) :in-progress)
    (swap! app-state assoc-in [:board j i] "P")
    (swap! app-state assoc :game-status (game-status (:board @app-state)))
    (if (= (:game-status @app-state) :in-progress)
      [(swap! app-state update-in [:board] computer-move)
       (swap! app-state assoc :game-status (game-status (:board @app-state)))]
      nil)))

(defn blank [i j]
  [:rect
   {:width 0.9
    :height 0.9
    :fill "grey"
    :x (+ 0.05 i)
    :y (+ 0.05 j)
    :on-click #((rect-click i j))}])

(defn circle [i j]
  [:circle
   {:r 0.35
    :cx (+ 0.5 i)
    :cy (+ 0.5 j)
    :fill "none"
    :stroke "green"
    :stroke-width 0.12}])

(defn cross [i j]
  [:g {:stroke "darkred"
       :stroke-width 0.4
       :stroke-linecap "round"
       :transform
       (str "translate(" (+ 0.5 i) "," (+ 0.5 j) ") "
            "scale(0.3)")}
   [:line {:x1 -1 :y1 -1 :x2 1 :y2 1}]
   [:line {:x1 1 :y1 -1 :x2 -1 :y2 1}]])

(defn new-game-click [e]
  (swap! app-state assoc :board (new-board board-size))
  (swap! app-state assoc :game-status :in-progress))

(defn tictactoe []
  [:center
   [:h1 (:text @app-state)]
   (case (:game-status @app-state)
     :player-victory [:h2 "You won!"]
     :computer-victory [:h2 "Computer wins."]
     :draw [:h2 "Draw."]
     nil)
   (into
    [:svg
     {:view-box (str "0 0 " board-size " " board-size)
      :width 500
      :height 500}]
    (for [i (range (count (:board @app-state)))
          j (range (count (:board @app-state)))]
      (case (get-in @app-state [:board j i])
        "B" [blank i j]
        "P" [circle i j]
        "C" [cross i j])))
   [:p
    [:button {:on-click new-game-click} "New Game"]]])

(rd/render [tictactoe]
           (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
