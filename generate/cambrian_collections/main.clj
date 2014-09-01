(ns cambrian-collections.main
  (:require
    [cambrian-collections
     [java :as j]
     [vector :as v]]))

(defn -main [& args]
  (spit
    (str "collections/clojure/lang/PersistentUnrolledVector.java")
    (j/format-java
      (str
        v/class-prelude
        (v/unrolled-vector 6)))))
