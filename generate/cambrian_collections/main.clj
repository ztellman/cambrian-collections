(ns cambrian-collections.main
  (:require
    [cambrian-collections
     [java :as j]
     [vector :as v]]))

(defn -main [& args]
  (dotimes [i 6]
    (spit
      (str "collections/clojure/lang/PersistentVector" i ".java")
      (j/format-java
        (str
          v/class-prelude
          (v/fixed-arity-vector i))))))
