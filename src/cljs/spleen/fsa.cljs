(ns spleen.fsa)

(defprotocol INode
  (root [this])
  (children [this]))

(def charset (zipmap "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                   "aąbcćdeęfghijklłmnńoóprsśtuwyzźż"))

(defn node-params [buf pos]
  (let [x (aget buf pos)]
    {:dest (bit-and (bit-shift-right x 10) 0x3fffff)
     :char (let [ch (char (bit-and (bit-shift-right x 2) 0xff))]
             (or (charset ch) ch))
     :sibling (zero? (bit-and (bit-shift-right x 1) 1))
     :end (= 1 (bit-and x 1))}))

(deftype Node [buf pos]
  INode
  (root [this]
    (Node. buf (aget buf 0)))
  (children [this]
    (loop [i pos res []]
      (let [x (node-params buf i)
            el {:node (let [dest (:dest x)]
                        (when-not (zero? dest)
                          (Node. buf (:dest x)))),
               :char (:char x),
               :end (:end x)}]
        (if (:sibling x)
          (recur (inc i) (conj res el))
          (conj res el)))))
  IEquiv
  (-equiv [this other] (and
                         (= (type this) (type other))
                         (= (.-buf this) (.-buf other))
                         (= (.-pos this) (.-pos other)))))

(defn fsa [buf]
  (Node. buf (aget buf 0)))

(defn follow [node char]
  (first (filter #(= char (:char %)) (children node))))

(defn check [dict word]
  (loop [node dict [ch & chs] word]
    (let [{next-node :node, end :end} (follow node ch)]
      (if (seq chs)
        (when next-node
          (recur next-node chs))
        end))))

(defn str-remove
  "remove char from string"
  [s pos]
  (str (subs s 0 pos) (subs s (inc pos))))

(defn single-step-word [{:keys [node word letters]}]
  (remove nil?
          (apply concat
                 (map-indexed (fn [i ch]
                                (when node
                                  (if (= ch "?")
                                    (for [x (children node)]
                                      (assoc x :word (str word (:char x)) :letters (str-remove letters i)))
                                    (when-let [res (follow node ch)]
                                      [(assoc res :word (str word ch) :letters (str-remove letters i))]))))
                              letters))))

(defn single-step [candidates]
  (if (-> candidates first :letters seq)
    (distinct (mapcat single-step-word candidates))
    (map :word (filter :end candidates))))

(defn anagrams [dict word]
  (sort
   (distinct
    (nth (iterate single-step [{:node dict :letters word}]) (inc (count word))))))
