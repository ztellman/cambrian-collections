(ns cambrian-collections.map-test
  (:require
    [criterium.core :as c]
    [clojure.test :refer :all]
    [collection-check :as check]
    [clojure.test.check.generators :as gen])
  (:import
    [java.util.concurrent
     ConcurrentHashMap]
    [java.util
     HashMap]
    [clojure.lang
     PersistentUnrolledMap]))

(declare unrolled)

(deftest test-map-like
  (check/assert-map-like 1e4 (unrolled) gen/int gen/int))

;;;

(defn unrolled
  ([]
     (PersistentUnrolledMap/create))
  ([a b]
     (PersistentUnrolledMap/create a b))
  ([a b c d]
     (PersistentUnrolledMap/create a b c d))
  ([a b c d e f]
     (PersistentUnrolledMap/create a b c d e f))
  ([a b c d e f g h]
     (PersistentUnrolledMap/create a b c d e f g h))
  ([a b c d e f g h i j]
     (PersistentUnrolledMap/create a b c d e f g h i j))
  ([a b c d e f g h i j k l]
     (PersistentUnrolledMap/create a b c d e f g h i j k l))
  ([a b c d e f g h i j k l & rst]
     (let [r (-> (unrolled)
               transient
               (assoc! a b)
               (assoc! c d)
               (assoc! e f)
               (assoc! g h)
               (assoc! i j)
               (assoc! k l))]
       (loop [r r, s rst]
         (if (empty? s)
           (persistent! r)
           (recur (assoc! r (first s) (second s)) (rest (rest s))))))))

(defmacro do-benchmark [description bench-form-fn]
  (let [bench-form-fn (eval bench-form-fn)]
    `(do
       ~@(map
           (fn [[type x]]
            `(do
               (println "\n  ** "~description ~type "\n")
               ~(bench-form-fn x)))
           (partition 2
             ["key map 1"        '(array-map :a 2)
              "key unrolled 1"   '(unrolled :a 2)
              "sym map 1"        '(array-map 'a 2)
              "sym unrolled 1"   '(unrolled 'a 2)
              "key map 2"        '(array-map :a 2 :b 4)
              "key unrolled 2"   '(unrolled :a 2 :b 4)
              "sym map 2"        '(array-map 'a 2 'b 4)
              "sym unrolled 2"   '(unrolled 'a 2 'b 4)
              "key map 3"        '(array-map :a 2 :b 4 :c 6)
              "key unrolled 3"   '(unrolled :a 2 :b 4 :c 6)
              "sym map 3"        '(array-map 'a 2 'b 4 'c 6)
              "sym unrolled 3"   '(unrolled 'a 2 'b 4 'c 6)
              "key map 5"        '(array-map :a 2 :b 4 :c 6 :d 8 :e 10)
              "key unrolled 5"   '(unrolled :a 2 :b 4 :c 6 :d 8 :e 10)
              "sym map 5"        '(array-map 'a 2 'b 4 'c 6 'd 8 'e 10)
              "sym unrolled 5"   '(unrolled 'a 2 'b 4 'c 6 'd 8 'e 10)
              "key map 7"        '(array-map :a 2 :b 4 :c 6 :d 8 :e 10 :f 12 :g 14)
              "key unrolled 7"   '(unrolled :a 2 :b 4 :c 6 :d 8 :e 10 :f 12 :g 14)
              "sym map 7"        '(array-map 'a 2 'b 4 'c 6 'd 8 'e 10 'f 12 'g 14)
              "sym unrolled 7"   '(unrolled 'a 2 'b 4 'c 6 'd 8 'e 10 'f 12 'g 14)
              ])))))

(deftest ^:benchmark benchmark-construction
  (do-benchmark "create" (fn [x] `(c/quick-bench ~x))))

(deftest ^:benchmark benchmark-reduce
  (do-benchmark "reduce +"
    (fn [x] `(c/quick-bench (reduce-kv (fn [a# b# c#] (+ a# c#)) 0 ~x)))))

(deftest ^:benchmark benchmark-assoc
  (do-benchmark "assoc" (fn [x] `(let [x# ~x] (c/quick-bench (assoc x# ~@(take-last 2 x)))))))

(deftest ^:benchmark benchmark-into
  (do-benchmark "into"
    (fn [x]
      `(let [x# ~x]
         (c/quick-bench
           (persistent! (reduce conj! (transient (empty x#)) x#)))))))

(deftest ^:benchmark benchmark-get-last
  (do-benchmark "get-last"
    (fn [x] `(let [x# ~x] (c/quick-bench (get x# ~(-> x butlast last)))))))

(deftest ^:benchmark benchmark-seq
  (do-benchmark "seq"
    (fn [x]
      `(c/quick-bench (dorun (seq ~x))))))

(deftest ^:benchmark benchmark-hasheq
  (do-benchmark "hasheq" (fn [x] `(c/quick-bench (hash ~x)))))

(deftest ^:benchmark benchmark-hash
  (do-benchmark "hash" (fn [x] `(c/quick-bench (.hashCode ~(with-meta x {:tag "Object"}))))))

(deftest ^:benchmark benchmark-equality
  (do-benchmark "equals"
    (fn [x]
      `(let [a# ~x
             b# ~x]
         (c/quick-bench
           (= a# b#))))))
