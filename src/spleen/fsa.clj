(ns spleen.fsa
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (morfologik.fsa FSA CFSA2)))

(defprotocol IArc
  (target [this])
  (label [this])
  (final? [this])
  (terminal? [this]))

(deftype Arc [^FSA fsa ^int number]
  clojure.lang.Seqable
  (seq [this]
    (let [next-number (.getNextArc fsa number)]
      (cons this
            (when (pos? next-number)
              (let [arc (Arc. fsa next-number)]
                (lazy-seq (cons arc (next arc))))))))

  clojure.lang.Counted
  (count [this]
    (.getArcCount fsa number))

  clojure.lang.ILookup
  (valAt [this k]
    (when (int? k)
      (let [res (.getArc fsa number k)]
        (when (pos? res)
          (Arc. fsa res)))))

  IArc
  (target [this] (Arc. fsa (.getEndNode fsa number)))
  (label [this] (.getArcLabel fsa number))
  (final? [this] (.isArcFinal fsa number))
  (terminal? [this] (.isArcTerminal fsa number))

  Object
  (toString [this] (string/join " "
                                (remove nil? ["Arc"
                                              number
                                              (label this)
                                              (when (final? this) "final")
                                              (when (terminal? this) "terminal")]))))

(defn root [^FSA fsa]
  (Arc. fsa (.getRootNode fsa)))

(comment

(def fsa (with-open [is (io/input-stream "/Users/nathell/projects/spleen/osps.cfsa")]
           (FSA/read is)))

)
