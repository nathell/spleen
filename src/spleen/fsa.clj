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

(extend-type nil
  IArc
  (target [_] nil)
  (label [_] nil)
  (final? [_] nil)
  (terminal? [_] nil))

(defn root [^FSA fsa]
  (Arc. fsa (.getRootNode fsa)))

(defn ^byte latin2-byte [c]
  (let [code ({\ą -79, \ć -26, \ę -22, \ł -77, \ń -15, \ó -13, \ś -74, \ź -68, \ż -65} c)]
    (byte (or code (int c)))))

(comment

(def fsa (with-open [is (io/input-stream "/Users/nathell/projects/spleen/osps.cfsa")]
           (FSA/read is)))

)
