(ns spleen.ui
  (:require [cljfx.api :as fx]
            [clojure.string :as string]
            [spleen.core :as core])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.input MouseEvent TransferMode]
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

(defn tile [{:keys [letter score size translate]
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
     :translate-x (:x translate 0)
     :translate-y (:y translate 0)
     :on-mouse-pressed {:event/type ::drag-started}
     :on-mouse-dragged {:event/type ::mouse-dragged}
     :on-mouse-released {:event/type ::drag-stopped}
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

(defn empty-rack [size]
  (let [border-width (/ size 10)
        upspace (* 2 border-width)
        height (+ size upspace border-width)]
    {:fx/type :canvas
     :width (+ (* size 8) (* border-width 2))
     :height height
     :draw (fn [^Canvas canvas]
             (let [ctx (.getGraphicsContext2D canvas)]
               (doto ctx
                 (.setFill (rgb "122721") #_(field-colors \.))
                 (.fillRect 0 0 border-width height)
                 (.fillRect border-width (+ size upspace) (+ border-width (* 8 size)) height)
                 (.fillRect (+ border-width (* 8 size)) 0
                            (+ (* 2 border-width (* 8 size))) height)
                 (.setFill (field-colors \.))
                 (.fillRect border-width 0
                            (* 8 size) (+ upspace size)))))}))

(defn atile [state letter]
  (tile {:letter (string/upper-case (str letter)),
         :translate (:dragging state),
         :score (->> core/tiles
                     (filter #(= (first %) letter))
                     first last)}))

(defn atiles [str]
  (mapv atile str))

(defn rack [state size]
  {:fx/type :pane
   :children [(empty-rack size)
              {:fx/type :pane
               :layout-x 5
               :layout-y 10
               :children (mapv (partial atile state) "ż")}]})

(defn root [{state :state}]
  {:fx/type :stage
   :showing true
   :title "Spleen"
   :width 750
   :height 1000
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  ;; :alignment :center
                  :children [(board 50)
                             (rack state 50)]}}})

(defn letters []
  {:fx/type :h-box
   :alignment :center
   :children [(tile {:letter "H", :score 4})
              (tile {:letter "E", :score 1})
              (tile {:letter "L", :score 1})
              (tile {:letter "L", :score 1})
              (tile {:letter "O", :score 2})]})

(def *state
  (atom {:dragging {:x 0, :y 0}}))

(defmulti event-handler :event/type)

(def delta (atom nil))

(defmethod event-handler ::mouse-dragged [e]
  (let [fx-event ^MouseEvent (:fx/event e)]
    (swap! *state assoc :dragging
           (merge-with +
                       @delta
                       {:x (.getSceneX fx-event),
                        :y (.getSceneY fx-event)}))))

(defmethod event-handler ::drag-started [e]
  (let [fx-event ^MouseEvent (:fx/event e)
        source (.getSource fx-event)]
    (reset! delta {:x (- (.getLayoutX source) (.getSceneX fx-event))
                   :y (- (.getLayoutY source) (.getSceneY fx-event))})))

(defmethod event-handler ::drag-stopped [e]
  (swap! *state assoc :dragging {:x 0, :y 0}))

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc (fn [state]
                                    {:fx/type root
                                     :state state}))
    :opts {:fx.opt/map-event-handler event-handler}))

(fx/mount-renderer *state renderer)
