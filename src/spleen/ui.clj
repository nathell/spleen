(ns spleen.ui
  (:require [cljfx.api :as fx]
            [spleen.core :as core])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.paint Color]
           [javafx.scene.text Font FontWeight TextAlignment]))

(defn rgb [s]
  (let [[r g b] (map #(Integer/parseInt (apply str %) 16) (partition 2 s))]
    (Color/rgb r g b)))

(def field-colors
  {\. (rgb "3C8571")
   \2 (rgb "79B6E5")
   \3 (rgb "4194E0")
   \D (rgb "EFA284")
   \T (rgb "DB3920")})

(defn tile [{:keys [letter score size]
             :or {size 50}}]
  (let [typeface "Gill Sans"
        round (double (* size 3/10))
        letter-size (double (* size 72/100))
        number-size (double (* size 3/10))
        letter-x (double (* size 1/2))
        letter-y (double (* size 3/4))
        number-x (double (* size 85/100))
        number-y (double (* size 90/100))]
    {:fx/type :canvas
     :width size
     :height size
     :draw (fn [^Canvas canvas]
             (let [ctx (.getGraphicsContext2D canvas)]
               (doto ctx
                 (.clearRect 0 0 size size)
                 (.setFill Color/IVORY)
                 (.fillRoundRect 0 0 size size round round)
                 (.strokeRoundRect 0 0 size size round round)
                 (.setFill Color/BLACK)
                 (.setTextAlign TextAlignment/CENTER))
               (when letter
                 (doto ctx
                   (.setFont (Font/font typeface FontWeight/NORMAL letter-size))
                   (.fillText letter letter-x letter-y)))
               (when score
                 (doto ctx
                   (.setFont (Font/font typeface FontWeight/NORMAL number-size))
                   (.fillText (str score) number-x number-y)))))}))

(defn board [field-size]
  (let [nx (count (first core/scrabble-board-layout))
        ny (count core/scrabble-board-layout)]
    {:fx/type :canvas
     :width (* field-size nx)
     :height (* field-size ny)
     :draw (fn [^Canvas canvas]
             (let [ctx (.getGraphicsContext2D canvas)]
               (doseq [[y s] (map-indexed vector core/scrabble-board-layout)
                       [x c] (map-indexed vector s)]
                 (doto ctx
                   (.setFill (field-colors c))
                   (.fillRect (* field-size x) (* field-size y) (* field-size (inc x)) (* field-size (inc y)))))
               (doseq [y (range (inc ny))]
                 (.strokeLine ctx 0 (* field-size y)
                              (* field-size (inc nx)) (* field-size y)))
               (doseq [x (range (inc nx))]
                 (.strokeLine ctx (* field-size x) 0
                              (* field-size x) (* field-size (inc nx))))))}))

(defn root []
  {:fx/type :stage
   :showing true
   :title "Spleen"
   :width 750
   :height 750
   :scene {:fx/type :scene
           :root {:fx/type :group
                  ;; :alignment :center
                  :children [(board 50)
                             {:fx/type :h-box
                              :alignment :center
                              :children [(tile {:letter "D", :score 2})
                                         (tile {:letter "U", :score 3})
                                         (tile {:letter "P", :score 2})
                                         (tile {:letter "A", :score 1})]}]}}})

#_(fx/on-fx-thread (fx/create-component (root)))
