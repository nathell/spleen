(ns spleen.core
  (:require [reagent.core :as reagent]
            [clojure.string :as string]
            [spleen.utils :refer [reg-getters reg-accessors dispatch-value]]
            [ajax.core :refer [ajax-request url-request-format]]
            [ajax.protocols :refer [-body]]
            [re-frame.core :refer [reg-event-db reg-sub dispatch dispatch-sync subscribe]]
            [spleen.fsa :refer [fsa check anagrams]]))

(enable-console-print!)

(reg-event-db
 :init
 (fn [_ _]
   {:page :arbiter
    :board {[6 7] \k [7 7] \o [8 7] \c [9 7] \h [10 7] \a [11 7] \m [8 8] \i [8 9] \ę}}))

(reg-getters :board)
(reg-accessors :dict :word :page)

(def scores
  {\a 1, \ą 5, \b 3, \c 2, \ć 6, \d 2, \e 1, \ę 5, \f 5, \g 3, \h 3, \i 1,
   \j 3, \k 2, \l 2, \ł 3, \m 2, \n 1, \ń 7, \o 1, \ó 5, \p 2, \r 1, \s 1,
   \ś 5, \t 2, \u 3, \w 1, \y 2, \z 1, \ź 9, \ż 5})

(defn tile [letter]
  [:div.tile
   [:span.letter (string/upper-case letter)]
   [:span.points (scores letter)]])

(def *scrabble-board-layout*
  ["T..2...T...2..T"
   ".D...3...3...D."
   "..D...2.2...D.."
   "2..D...2...D..2"
   "....D.....D...."
   ".3...3...3...3."
   "..2...2.2...2.."
   "T..2...D...2..T"
   "..2...2.2...2.."
   ".3...3...3...3."
   "....D.....D...."
   "2..D...2...D..2"
   "..D...2.2...D.."
   ".D...3...3...D."
   "T..2...T...2..T"])

(defn board []
  (let [data @(subscribe [:board])]
    [:div.board
     (for [[y line] (map-indexed vector *scrabble-board-layout*)
           [x ch] (map-indexed vector line)
           :let [cls (if (= ch \.) "normal" (str "f" ch))]]
       ^{:key (str "f" y "-" x)}
       [:div.field {:class cls :style {:left (str (* x 51) "px")
                                       :top (str (* y 51) "px")}}
        (when-let [ch (data [x y])]
          [tile ch])])]))

#_(defn )

(def osps-url "http://cors-anywhere.herokuapp.com/http://www.pfs.org.pl/osps/update35.upd")

(defn read-file [f]
  (let [rdr (js/FileReader.)]
    (set! (.-onload rdr) #(set! js/window.buf (-> % .-target .-result js/Uint32Array.)))
    (.readAsArrayBuffer rdr f)))

(defn extract-osps [data]
  (let [length-arr (js/Uint32Array. data 0 1)
        length (aget length-arr 0)
        osps-arr (js/Uint32Array. data 16 length)]
    osps-arr))

(defn u32->u8 [arr]
  (let [buf (.-buffer arr)
        length-arr (js/Uint32Array. buf 0 1)
        length (aget length-arr 0)]
    (js/Uint8Array. buf 16 (* 4 length))))

(defn save-osps [osps]
  (js/localStorage.setItem "osps" (-> osps u32->u8 js/ua2text)))

(defn load-osps-from-pfs []
  (ajax-request {:uri osps-url
                 :method :get
                 :handler (fn [[ok response]]
                            (when ok
                              (let [osps (extract-osps response)]
                                (save-osps osps)
                                (dispatch [:set-dict (fsa osps)]))))
                 :format url-request-format
                 :response-format {:content-type "text/plain" :read -body :type :arraybuffer}}))

(defn load-osps []
  (let [s (js/localStorage.getItem "osps")]
    (if (and s (not= s "reload"))
      (let [osps (-> s js/text2ua .-buffer js/Uint32Array.)]
        (dispatch [:set-dict (fsa osps)]))
      (load-osps-from-pfs))))

(defn root []
  [board]
  #_[:div
   [:input {:type "file" :on-change #(-> % .-target .-files (aget 0) read-file)}]])

(defn wordfield []
  (let [word (subscribe [:word])]
    [:input {:type "text"
             :auto-focus true
             :value @word
             :pattern "+"
             :on-change #(let [word (-> % .-target .-value
                                        string/lower-case
                                        (string/replace "*" "?")
                                        (string/replace #"[^aąbcćdeęfghijklłmnńoóprsśtuwyzźż?]" ""))]
                           (js/console.log word)
                           (dispatch-sync [:set-word word]))}]))

(defn result []
  (let [word (subscribe [:word])
        dict (subscribe [:dict])]
    [:div.result
     (cond
       (not @dict) "Ładuję słownik..."
       (not (seq @word)) [:span.emo "?"]
       (check @dict (string/lower-case @word)) [:span.emo.ok "☺"]
       :otherwise [:span.emo.wrong "☹"])]))

(defn osps []
  [:div.osps
   [wordfield]
   [result]])

(defn anagramy []
  (let [word (subscribe [:word])
        dict (subscribe [:dict])]
    [:div.anagramy
     (if-not @dict
       "Ładuję słownik..."
       (let [ana (anagrams @dict @word)]
         (cond
           (not (seq @word)) "Wpisz słowo do przeanagramowania"
           (not (seq ana)) [:span.brak "Brak anagramów!"]
           :otherwise
           [:ul
            (for [x ana]
              ^{:key x} [:li x])])))]))

(defn anagramator []
  [:div.osps
   [wordfield]
   [anagramy]])

(def pages
  [{:page "Arbiter",
    :key :arbiter,
    :view [osps]}
   {:page "Anagramy",
    :key :anagramy,
    :view [anagramator]}])

(defn layout []
  (let [current-page (subscribe [:page])]
    [:div.layout
     [:header
      [:ul
       (doall
        (for [{:keys [page key]} pages]
          ^{:key (str "page-" page)}
          [:li
           (if (= key @current-page)
             [:span.current page]
             [:a {:href "#" :on-click #(dispatch [:set-page key])} page])]))]]
     [:main (-> (filter #(= (:key %) @current-page) pages) first :view)]]))

(defn init []
  (dispatch-sync [:init])
  (load-osps)
  (reagent/render [layout] (.getElementById js/document "app")))

(js/document.addEventListener "DOMContentLoaded" init)
