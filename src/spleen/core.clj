(ns spleen.core
  (:require [spleen.fsa :as fsa]))

(defn lowercase [ch]
  (if ch (Character/toLowerCase ch)))

(def scrabble-board-layout
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

(def tiles
     [[\a 9 1]
      [\ą 1 5]
      [\b 2 3]
      [\c 3 2]
      [\ć 1 6]
      [\d 3 2]
      [\e 7 1]
      [\ę 1 5]
      [\f 1 5]
      [\g 2 3]
      [\h 2 3]
      [\i 8 1]
      [\j 2 3]
      [\k 3 2]
      [\l 3 2]
      [\ł 2 3]
      [\m 3 2]
      [\n 5 1]
      [\ń 1 7]
      [\o 6 1]
      [\ó 1 5]
      [\p 3 2]
      [\r 4 1]
      [\s 4 1]
      [\ś 1 5]
      [\t 3 2]
      [\u 2 3]
      [\w 4 1]
      [\y 4 2]
      [\z 5 1]
      [\ź 1 9]
      [\ż 1 5]
      [\_ 2 0]])

(def nonblank-tiles (butlast (map first tiles)))

(let [scores (apply hash-map (apply concat (map (fn [[a _ b]] [a b]) tiles)))]
  (defn tile-score [tile]
    (or (scores tile) 0)))

(defn letter-bonus [bonus score tile additional]
  (assoc score
    :score (+ (:score score) (* bonus (tile-score tile)))
    :additional (+ (:additional score) additional)))

(def no-bonus (partial letter-bonus 1))

(defn word-bonus [bonus score tile additional]
  (assoc score
    :score (+ (:score score) (tile-score tile))
    :word-bonus (* (:word-bonus score) bonus)
    :additional (+ (:additional score) additional)))

(def scoring-functions
     {\. no-bonus,
      \2 (partial letter-bonus 2),
      \3 (partial letter-bonus 3),
      \D (partial word-bonus 2),
      \T (partial word-bonus 3)})

(def field-score
  (into {}
        (for [[y row] (map-indexed vector scrabble-board-layout)
              [x ch] (map-indexed vector row)]
          [[x y] (scoring-functions ch)])))

(defn tile-bag []
  (apply concat (map (fn [[tile count _]] (take count (repeat tile))) tiles)))

(def board-size (count scrabble-board-layout))

(defn make-board []
  {})

(defn add-position [pos1 pos2]
  (vec (map + pos1 pos2)))

(defn adjacent [pos where]
  (add-position pos ({:up [0 -1], :down [0 1], :left [-1 0], :right [1 0]} where)))

(def opposite {:up :down, :down :up, :left :right, :right :left})

(def perpendicular {:up :left, :left :up, :down :right, :right :down})

(defn neighbours [pos]
  (map (partial adjacent pos) [:up :down :left :right]))

(defn position-legal? [[x y]]
  (and (< -1 x board-size) (< -1 y board-size)))

(defn position-almost-legal? [[x y]]
  (and (<= 0 x board-size) (<= 0 y board-size)))

(defn legal-positions [positions]
  (filter position-legal? positions))

(defn anchor? [board position]
  (if (empty? board)
    (let [middle (quot board-size 2)]
      (= position [middle middle]))
    (and (position-legal? position)
         (not (board position))
         (some #(board %) (neighbours position)))))

(defn anchors [board]
  (if (empty? board)
    (let [middle (quot board-size 2)]
      (list [middle middle]))
    (filter #(and (position-legal? %) (not (board %)))
            (distinct (apply concat (map neighbours (keys board)))))))

(defn skip-free [n board position direction]
  (cond
    (not (position-legal? position)) (adjacent position (opposite direction))
    (board position) (recur n board (adjacent position direction) direction)
    (zero? n) position
    true (recur (dec n) board (adjacent position direction) direction)))

(defn next-anchor
  ([board position direction] (next-anchor 0 board position direction))
  ([n board position direction]
     (cond
       (not (position-legal? position)) nil
       (anchor? board position) n
       true (recur (inc n) board (adjacent position direction) direction))))

(def next-free (partial skip-free 0))

(defn all-positions []
  (for [x (range board-size)
        y (range board-size)]
    [x y]))

(defn place-word [board position dir word]
  (let [[result position]
        (reduce (fn [[board position] letter]
                  [(assoc board position letter)
                   (next-free board (adjacent position dir) dir)])
                [board (next-free board position dir)]
                word)]
    (when-not (position-almost-legal? position)
      (throw (Error. "Incorrectly placed word")))
    result))

(defn scrabble-bonus [word]
  (if (= (count word) 7) 50 0))

(defn score-word
  ([board position dir word]
     (score-word board position dir word true))

  ([board position dir word add-opposite]
     (loop [position position,
            letters word,
            score {:score 0, :word-bonus 1, :additional (scrabble-bonus word)}]
       (let [tile (board position)]
         (cond
           (and (empty? letters) (not tile))
           (+ (* (:score score) (:word-bonus score)) (:additional score))
           tile
           (recur (adjacent position dir) letters (no-bonus score tile 0))
           true
           (let [letter (first letters)
                 letters (rest letters)
                 perp (perpendicular dir)
                 perp-prev (adjacent position (opposite perp))
                 perp-next (adjacent position perp)
                 perp-start (skip-free 1 board position (opposite perp))
                 score+ (field-score position)]
             (recur (adjacent position dir)
                    letters
                    (score+ score letter
                            (if (and add-opposite (or (board perp-prev) (board perp-next)))
                              (score-word board perp-start perp (list letter) false)
                              0)))))))))

(defn repeated-picks [coll]
  (loop [coll coll so-far () result ()]
    (if (empty? coll)
      result
      (let [[ch & chs] coll
            others (sort (concat so-far chs))]
        (recur chs
               (cons ch so-far)
               (if (= ch \_)
                 (concat (for [ch nonblank-tiles] [(Character/toUpperCase ch) others]) result)
                 (cons [ch others] result)))))))

(defn picks [coll]
  (distinct (repeated-picks coll)))

(defn get-edge [dawg current]
  (get dawg (fsa/latin2-byte current)))

(defn follow [dawg current]
  (some-> (get-edge dawg current) fsa/target))

(defn cast-dawg [dawg rack board position dir so-far anchor-passed?]
  (let [current (lowercase (board position))
        next-pos (adjacent position dir)
        next (lowercase (board next-pos))]
    (cond
      (not dawg) ()

      (and current next)
      (cast-dawg (follow dawg current) rack board next-pos dir so-far anchor-passed?)

      (and (empty? rack) current)
      (if (and anchor-passed? (fsa/final? (get-edge dawg current)))
        (list so-far)
        ())

      (empty? rack) ()

      current
      (if (and anchor-passed? (fsa/final? (get-edge dawg current)))
        (cons (reverse so-far) (cast-dawg (follow dawg current) rack board next-pos dir so-far anchor-passed?))
        (cast-dawg (follow dawg current) rack board next-pos dir so-far anchor-passed?))

      next
      (let [anchor-passed? (or anchor-passed? (anchor? board position))]
        (reduce concat (map (fn [[letter rest]]
                              (cast-dawg (follow dawg letter) rest board next-pos dir (cons letter so-far) anchor-passed?))
                            (picks rack))))

      true
      (let [anchor-passed? (or anchor-passed? (anchor? board position))]
        (reduce concat (map (fn [[letter rest]]
                              (let [so-far (cons letter so-far)]
                                (concat
                                 (if (and anchor-passed? (fsa/final? (get-edge dawg letter)))
                                   (list (reverse so-far))
                                   ())
                                 (cast-dawg (follow dawg letter) rest board next-pos dir so-far anchor-passed?))))
                            (picks rack)))))))

(defn cast-point? [board num-letters position direction]
  (let [anchor (next-anchor board position direction)]
    (or (and (board position)
             (not (board (adjacent position (opposite direction))))
             anchor)
        (and anchor
             (not (board position))
             (not (board (adjacent position (opposite direction))))
             (< anchor num-letters)))))

(defn cast-points [board num-letters]
  (for [position (all-positions) direction [:right :down]
        :when (cast-point? board num-letters position direction)]
    [position direction]))

(defn all-possible-moves [board rack dict]
  (reduce concat
          (for [[position direction] (cast-points board (count rack))]
            (map #(list % position direction (score-word board position direction %))
                 (cast-dawg dict rack board position direction () false)))))

(comment
  (all-possible-moves {} "wolanie" (fsa/root fsa)))
