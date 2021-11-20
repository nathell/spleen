(ns spleen.build
  (:require [clojure.string :as string]))

(defn build [{:keys [args]}]
  (let [args (string/split args #" ")]
    (morfologik.tools.Launcher/main (into-array String args))))
