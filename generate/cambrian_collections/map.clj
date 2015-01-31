(ns cambrian-collections.map
  (:require
    [cambrian-collections.java :as j]
    [clojure.string :as str]))

(defn reduce-body [reduce-ops]
  (apply str
    (map
      (fn [op]
        (str
          "init = " op ";"
          "if (RT.isReduced(init)) { return ((IDeref)init).deref(); }"))
      reduce-ops)))

(defn overflow-constructor [ks vs]
  (str
    (apply str
      "IPersistentMap map = PersistentHashMap.EMPTY.asTransient()"
      (map
        (fn [k v]
          (str "." (j/invoke 'assoc k v)))
        ks
        vs))
    ".assoc(key,val).persistent();"
    "return (IPersistentMap) ((IObj)map).withMeta(meta);"))

(def class-prelude
  (->>
    ["package clojure.lang"
     "import java.util.Iterator"
     "import java.util.Map"]
    (map #(str % ";\n"))
    (apply str)))

(defn iterator [mname cardinality ret-k-v ks vs]
  (j/method '[public] 'Iterator mname []
    "return new Iterator() {"
    "int i = 0;"
    (j/method '[public] 'boolean 'hasNext []
      (if (zero? cardinality)
        "return false;"
        (str "return i < " cardinality ";")))
    (j/method '[public] 'Object 'next []
      (if (zero? cardinality)
        "throw new IndexOutOfBoundsException();"
        (apply j/switch "i++"
          (concat
            (mapcat
              (fn [idx k v]
                [idx (str "return " (ret-k-v k v) ";")])
              (range)
              ks
              vs)
            ["throw new IndexOutOfBoundsException();"]))))
    (j/method '[public] 'void 'remove []
      "throw new UnsupportedOperationException();")
    "};"))

;;;

(defn persistent-map [cardinality max-cardinality]
  (let [inc-classname (str 'Card (inc cardinality))
        dec-classname (str 'Card (dec cardinality))
        classname (str 'Card cardinality)
        ks (mapv
             #(str "k" %)
             (range cardinality))
        vs (mapv
             #(str "v" %)
             (range cardinality))
        hs (mapv
             #(str "h" %)
             (range cardinality))
        fields (interleave ks vs hs)]
    (j/class
      {:modifiers '[public static]
       :implements '[IObj IEditableCollection IReduce]
       :extends 'APersistentMap}
     classname

      ;; fields
      (->> (concat ks vs)
        (map #(j/field '[final] 'Object %))
        (apply str))

      (->> hs
        (map #(j/field '[final] 'int %))
        (apply str))

      (j/field '[private final] 'IPersistentMap 'meta)

      ;; public CardN(IPersistentMap meta, ...)
      (apply j/method nil nil classname
        (concat '[IPersistentMap meta] (interleave (cycle '[Object Object int]) fields))
        "this.meta = meta;"
        (map
          #(str "this." % " = " % ";")
          (interleave ks vs hs)))

      ;; public CardN(...)
      (apply j/method '[public] nil classname (interleave (cycle '[Object Object int]) fields)
        "this.meta = null;"
        (map
          #(str "this." % " = " % ";")
          (interleave ks vs hs)))

      #_"\n\n// only for use with *print-dup*, assumes correct cardinality\n"
      #_(j/method '[public static] classname 'create '[Map m]
        "Iterator it = m.entrySet().iterator();"
        (apply str
          (map
            (fn [idx k v h]
              (let [m (str 'm idx)]
                (str
                  "Map.Entry " m " = (Map.Entry) it.next();"
                  "Object " k " = " m ".getKey();"
                  "Object " v " = " m ".getValue();"
                  "int " h " = Util.hasheq(" k ");")))
            (range)
            ks
            vs
            hs))
        (j/cond "it.hasNext()"
          "throw new IllegalArgumentException(\"Incorrect cardinality in create method\");")
        (j/return "new " (apply j/invoke classname (interleave ks vs hs))))

      ;; public IPersistentMap meta()
      (j/method '[public] 'IPersistentMap 'meta []
        "return meta;")

      ;; public IObj withMeta(IPersistentMap meta)
      (j/method '[public] 'IObj 'withMeta '[IPersistentMap meta]
        "return new " (apply j/invoke classname 'meta (interleave ks vs hs)) ";")

      ;; int indexOf(int h, Object key)
      (j/method '[private] 'int 'indexOf '[int h, Object key]
        (j/cond "key instanceof Keyword"
          (str
            (when (pos? cardinality)
              (apply j/cond
                (mapcat
                  (fn [idx k]
                   [(str k "== key") (j/return idx)])
                 (range)
                 ks)))
            (j/return -1)))
        "return " (j/invoke 'indexOfObj 'h 'key) ";")

      ;; private int indexOfObj(int h, Object key)
      (j/method '[private] 'int 'indexOfObj '[int h, Object key]
        "Util.EquivPred ep = Util.equivPred(key);"
        (when (pos? cardinality)
          (apply j/cond
            (mapcat
              (fn [idx h k]
                [(str h " == h && "
                   (j/invoke 'ep.equiv 'key k))
                 (j/return idx)])
              (range)
              hs
              ks)))
        (j/return -1))

      ;; public boolean containsKey(Object key)
      (j/method '[public] 'boolean 'containsKey '[Object key]
        (if (zero? cardinality)
          "return false;"
          (j/return "indexOf(Util.hasheq(key), key) >= 0")))

      ;; public IMapEntry entryAt(Object key)
      (j/method '[public] 'IMapEntry 'entryAt '[Object key]
        (if (zero? cardinality)
          "return null;"
          (str
            "int idx = indexOf(Util.hasheq(key), key);"
            (apply j/switch 'idx
              (concat
                (mapcat
                  (fn [idx k v]
                    [idx (j/return (str "new MapEntry(" k "," v ")"))])
                  (range)
                  ks
                  vs)
                [(j/return 'null)])))))

      ;; public Object valAt(Object key)
      (j/method '[public] 'Object 'valAt '[Object key]
        (j/return (j/invoke 'valAt 'key 'null)))

      ;; public Object valAt(Object key, Object notFound)
      (j/method '[public] 'Object 'valAt '[Object key, Object notFound]
        (if (zero? cardinality)
          "return notFound;"
          (str
            "int idx = indexOf(Util.hasheq(key), key);"
            (apply j/switch 'idx
              (concat
                (mapcat
                  (fn [idx v]
                    [idx (j/return v)])
                  (range)
                  vs)
                [(j/return 'notFound)])))))

      ;; public Object asTransient()
      (j/method '[public] 'ITransientMap 'asTransient []
        "return new " (apply j/invoke 'Transient (interleave ks vs hs)) ";")

      ;; public IPersistentMap assoc(Object key, Object val)
      (j/method '[public] 'IPersistentMap 'assoc '[Object key, Object val]
        "int h = Util.hasheq(key);"
        (if (zero? cardinality)
          (j/return "new " (apply j/invoke inc-classname '[meta key val h]))
          (str
            "int idx = indexOf(h, key);"
            (apply j/switch 'idx
              (concat
                (mapcat
                  (fn [idx]
                    [idx
                     (str "return new "
                       (apply j/invoke classname 'meta
                         (interleave
                           ks
                           (assoc vs idx 'val)
                           hs))
                       ";")])
                  (range cardinality))
                [(if (= cardinality max-cardinality)
                   (overflow-constructor ks vs)
                   (str "return new "
                     (apply j/invoke inc-classname 'meta
                       (concat fields '[key val h])) ";"))])))))

      ;; public IPersistentMap assocEx(Object key, Object val)
      (j/method '[public] 'IPersistentMap 'assocEx '[Object key, Object val]
        "int h = Util.hasheq(key);"
        (if (zero? cardinality)
          (j/return "new " (apply j/invoke inc-classname 'meta (concat fields '[key val h])))
          (str
            "int idx = indexOf(h, key);"
            (j/cond "idx >= 0"
              "throw Util.runtimeException(\"Key already present\");")
            (if (= cardinality max-cardinality)
              (overflow-constructor ks vs)
              (str "return new "
                (apply j/invoke inc-classname 'meta (concat fields '[key val h])) ";")))))

      ;; public IPersistentMap without(Object key)
      (j/method '[public] 'IPersistentMap 'without '[Object key]
        (if (zero? cardinality)
          "return this;"
          (str
            "int idx = indexOf(Util.hasheq(key), key);"
            (apply j/switch 'idx
              (concat
                (mapcat
                  (fn [idx]
                    [idx
                     (str "return new "
                       (apply j/invoke dec-classname
                         'meta
                         (interleave
                           (concat (subvec ks 0 idx) (subvec ks (inc idx)))
                           (concat (subvec vs 0 idx) (subvec vs (inc idx)))
                           (concat (subvec hs 0 idx) (subvec hs (inc idx))))) ";")])
                  (range cardinality))
                [(j/return 'this)])))))

      ;; public int hashCode()
      (j/method '[public] 'int 'hashCode []
        (j/cond "_hash == -1"
          (str
            (apply str
              "int h = 0;"
              (map
                (fn [k v]
                  (str "h += " (j/invoke 'Util.hash k) " ^ " (j/invoke 'Util.hash v) ";"))
                ks
                vs))
            "_hash = h;"))
        "return _hash;")

      ;; public int hasheq()
      (j/method '[public] 'int 'hasheq []
        (j/cond "_hasheq == -1"
          (str
            (apply str
              "int h = 0;"
              (map
                (fn [h v]
                  (str "h += "
                    (j/invoke 'Murmur3.mixCollHash
                      (str "31 * (31 + " h ") + " (j/invoke 'Util.hasheq v))
                      2) ";"))
                hs
                vs))
            "_hasheq = " (j/invoke 'Murmur3.mixCollHash 'h cardinality) ";"))
        "return _hasheq;")

      ;; public boolean equiv(Object o)
      (j/method '[public] 'boolean 'equiv '[Object o]
        (j/cond

          "o instanceof IPersistentMap"
          (apply str
            (j/cond "!(o instanceof MapEquivalence)"
              "return false;")
            "IPersistentMap m = (IPersistentMap) o;"
            (j/cond (str "m.count() != " cardinality)
              "return false;")
            (concat
              (map
                (fn [idx k v]
                  (let [ov (str 'o idx)]
                    (str
                      "Object " ov " = " (j/invoke 'm.valAt k 'PersistentUnrolledMap.NOT_FOUND) ";"
                      (j/cond (str ov " == PersistentUnrolledMap.NOT_FOUND || !"
                                (j/invoke 'Util.equiv ov v))
                       "return false;"))))
                (range)
                ks
                vs)
              ["return true;"]))

          "o instanceof Map"
          (apply str
            "Map m = (Map) o;"
            (j/cond (str "m.size() != " cardinality)
              "return false;")
            (concat
              (map
                (fn [idx k v]
                  (let [f (str 'f idx)]
                    (str
                      "boolean " f " = " (j/invoke 'm.containsKey k) ";"
                      (j/cond (str "!" f " || !"
                                (j/invoke 'Util.equiv v
                                  (j/invoke 'm.get k)))
                        "return false;"))))
                (range)
                ks
                vs)
              ["return true;"]))

          "return false;"))

      ;; public boolean equals(Object o)
      (j/method '[public] 'boolean 'equals '[Object o]
        (j/cond

          "o instanceof IPersistentMap"
          (apply str
            "IPersistentMap m = (IPersistentMap) o;"
            (j/cond (str "m.count() != " cardinality)
              "return false;")
            (concat
              (map
                (fn [idx k v]
                  (let [ov (str 'o idx)]
                    (str
                      "Object " ov " = " (j/invoke 'm.valAt k 'PersistentUnrolledMap.NOT_FOUND) ";"
                      (j/cond (str ov " == PersistentUnrolledMap.NOT_FOUND || !"
                                (j/invoke 'Util.equals ov v))
                       "return false;"))))
                (range)
                ks
                vs)
              ["return true;"]))

          "o instanceof Map"
          (apply str
            "Map m = (Map) o;"
            (j/cond (str "m.size() != " cardinality)
              "return false;")
            (concat
              (map
                (fn [idx k v]
                  (let [f (str 'f idx)]
                    (str
                      "boolean " f " = " (j/invoke 'm.containsKey k) ";"
                      (j/cond (str "!" f " || !"
                                (j/invoke 'Util.equals v
                                  (j/invoke 'm.get k)))
                        "return false;"))))
                (range)
                ks
                vs)
              ["return true;"]))

          "return false;"))

      ;; public int count()
      (j/method '[public] 'int 'count []
        (j/return cardinality))

      ;; public IPersistentMap empty()
      (j/method '[public] 'IPersistentMap 'empty []
        "return EMPTY;")

      ;; public Object kvreduce(IFn f, Object init)
      (j/method '[public] 'Object 'kvreduce '[IFn f, Object init]
        (reduce-body
          (map
            (fn [k v]
              (j/invoke 'f.invoke 'init k v))
            ks
            vs))
        "return init;")

      ;; public Object reduce(IFn f)
      (j/method '[public] 'Object 'reduce '[IFn f]
        (cond
          (zero? cardinality)
          "return f.invoke();"

          (= 1 cardinality)
          (str "return new " (j/invoke 'MapEntry (first ks) (first vs)) ";")

          :else
          (str
            "Object init=" (first fields) ";"
            (reduce-body
              (map
                (fn [k v]
                  (j/invoke 'f.invoke 'init (str "new " (j/invoke 'MapEntry k v))))
                (rest ks)
                (rest vs)))
            "return init;")))

      ;; public Object reduce(IFn f, Object init)
      (j/method '[public] 'Object 'reduce '[IFn f, Object init]
        (reduce-body
          (map
            (fn [k v]
              (j/invoke 'f.invoke 'init (str "new " (j/invoke 'MapEntry k v))))
            ks vs))
        "return init;")

      ;; public Iterator iterator()
      (iterator
        'iterator
        cardinality
        (fn [k v]
          (str "new " (j/invoke 'MapEntry k v)))
        ks vs)

      ;; public Iterator keyIterator()
      (iterator
        'keyIterator
        cardinality
        (fn [k v] k)
        ks vs)

      ;; public Iterator valIterator()
      (iterator
        'valIterator
        cardinality
        (fn [k v] v)
        ks vs)

      (j/method '[public] "Object[]" 'toArray []
        "return new Object[] {"
        (str/join ","
          (map
            (fn [k v]
              (str "new MapEntry(" k "," v ")"))
            ks
            vs))
        "};")

      (when (pos? cardinality)
        (j/class
          {:extends 'ASeq
           :implements '[IChunkedSeq Counted]}
          'UnrolledChunkedSeq

          "private final IPersistentMap meta;"
          "private final int offset;"

          (j/method nil nil 'UnrolledChunkedSeq '[IPersistentMap meta, int offset]
            "this.offset = offset;"
            "this.meta = meta;")

          (j/method '[public] 'IChunk 'chunkedFirst []
            "return new " (j/invoke 'ArrayChunk "toArray()" 0) ";")

          (j/method '[public] 'ISeq 'chunkedNext []
            "return null;")

          (j/method '[public] 'ISeq 'chunkedMore []
            "return PersistentList.EMPTY;")

          (j/method '[public] 'UnrolledChunkedSeq 'withMeta '[IPersistentMap meta]
            "return new " (j/invoke 'UnrolledChunkedSeq 'meta 'offset) ";")

          (j/method '[public] 'Object 'first []
            (apply j/switch 'offset
              (mapcat
                (fn [idx k v]
                  [idx (j/return "new " (j/invoke 'MapEntry k v))])
                (range)
                ks
                vs))
            "throw new IndexOutOfBoundsException();")

          (j/method '[public] 'ISeq 'next []
            (j/cond (str "offset < " (dec cardinality))
              (j/return "new " (j/invoke 'UnrolledChunkedSeq 'null "offset+1")))
            (j/return 'null))

          (j/method '[public] 'int 'count []
            "return " cardinality " - offset;")))

      ;; public ISeq seq()
      (j/method '[public] 'ISeq 'seq []
        (if (zero? cardinality)
          "return null;"
          "return new UnrolledChunkedSeq(null, 0);")))))

;;;

(defn transient-map [max-cardinality]
  (let [ks (mapv
             #(str "k" %)
             (range max-cardinality))
        vs (mapv
             #(str "v" %)
             (range max-cardinality))
        hs (mapv
             #(str "h" %)
             (range max-cardinality))]
    (j/class
      {:modifiers '[static]
       :implements '[ITransientMap]
       :extends 'ATransientMap}
      'Transient

      (->> (concat ks vs)
        (map #(j/field '[private] 'Object %))
        (apply str))

      (->> hs
        (map #(j/field '[private] 'int %))
        (apply str))

      "private int count = 0;"

      "private boolean edit = true;"

      (->> (range (inc max-cardinality))
        (map
          (fn [n]
            (apply j/method '[public] nil 'Transient
              (interleave (cycle '[Object Object int])
                (interleave (take n ks) (take n vs) (take n hs)))
              "count = " n ";"
              (map
                #(str "this." % " = " % ";")
                (interleave
                  (take n ks)
                  (take n vs)
                  (take n hs))))))
        (apply str))

      ;; void ensureEditable()
      (j/method '[] 'void 'ensureEditable []
        (j/cond "!edit"
          "throw new IllegalAccessError(\"Transient used after persistent! call\");"))

       ;; int indexOf(int h, Object key)
      (j/method '[private] 'int 'indexOf '[int h, Object key]
        (j/cond "key instanceof Keyword"
          (apply j/switch (str max-cardinality " - count")
            (concat
              (mapcat
                (fn [idx k]
                  [idx
                   (j/cond (str k "== key")
                     (j/return (- max-cardinality idx 1)))])
                (range)
                (reverse ks))
              [(j/return -1)])))
        "return " (j/invoke 'indexOfObj 'h 'key) ";")

      ;; private int indexOfObj(int h, Object key)
      (j/method '[private] 'int 'indexOfObj '[int h, Object key]
        "Util.EquivPred ep = Util.equivPred(key);"
        (apply j/switch (str max-cardinality " - count")
          (mapcat
            (fn [idx h k]
              [idx
               (j/cond (str h " == h && "
                         (j/invoke 'ep.equiv 'key k))
                 (j/return (- max-cardinality idx 1)))])
            (range)
            (reverse hs)
            (reverse ks)))
        (j/return -1))

      (j/method '[] 'ITransientMap 'doAssoc '[Object key Object val]
        "int h = Util.hasheq(key);"
        "int idx = indexOf(h, key);"
        (j/cond "idx == -1"
          (apply j/switch "count++"
            (concat
              (mapcat
                (fn [idx h k v]
                  [idx (str h "= h;" k "= key;" v "= val; return this;")])
                (range)
                hs
                ks
                vs)
              [(str
                 (apply str
                   "return PersistentHashMap.EMPTY.asTransient()"
                   (map
                     (fn [k v]
                       (str "." (j/invoke 'assoc k v)))
                     ks
                     vs))
                 ".assoc(key,val);")]))
          (apply j/switch 'idx
            (concat
              (mapcat
                (fn [idx v]
                  [idx (str v "= val; return this;")])
                (range)
                vs)
              ; this should never happen
              [(j/return 'null)]))))
      (j/method '[] 'Object 'doValAt '[Object key, Object notFound]
        "int h = Util.hasheq(key);"
        "int idx = indexOf(h, key);"
        (apply j/switch 'idx
          (concat
            (mapcat
              (fn [idx v]
                [idx (j/return v)])
              (range)
              vs)
            [(j/return 'notFound)])))

      (j/method '[] 'ITransientMap 'doWithout '[Object key]
        "int h = Util.hasheq(key);"
        "int idx = indexOf(h, key);"
        (j/cond "idx < 0"
          "return this;")
        "count--;"
        (apply j/switch 'idx
          (mapcat
            (fn [idx [k0 k1] [v0 v1] [h0 h1]]
              [idx
               (str
                 k0 "=" k1 ";" v0 "=" v1 ";" h0 "=" h1 ";"
                 (when (< (+ idx 2) max-cardinality)
                   (j/cond (str "count ==" (inc idx))
                     "break;")))])
            (range)
            (partition 2 1 ks)
            (partition 2 1 vs)
            (partition 2 1 hs)))
        (j/return 'this))

      (j/method '[] 'IPersistentMap 'doPersistent '[]
        (apply j/switch 'count
          (mapcat
            (fn [n]
              [n (if (zero? n)
                   (j/return 'EMPTY)
                   (str "return new "
                     (apply j/invoke (str 'Card n)
                       (interleave
                         (take n ks)
                         (take n vs)
                         (take n hs)))
                     ";"))])
            (range (inc max-cardinality))))
        "throw new IllegalStateException();")

      (j/method '[] 'int 'doCount []
        (j/return 'count)))))

;;;

(defn unrolled-map [max-cardinality]
  (let [ks (mapv
             #(str "k" %)
             (range (inc max-cardinality)))
        vs (mapv
             #(str "v" %)
             (range (inc max-cardinality)))]
    (j/class
      {:modifiers '[public]}
      'PersistentUnrolledMap

      "static IPersistentMap EMPTY = new Card0();"

      "static Object NOT_FOUND = new Object();"

      (->> (range (inc max-cardinality))
        (map
          (fn [n]
            (j/method '[public static] 'IPersistentMap 'create
              (interleave
                (repeat 'Object)
                (interleave
                  (take n ks)
                  (take n vs)))
              (if (zero? n)
                "return EMPTY;"
                (str
                  "return new Card" n "("
                  (str/join ","
                    (interleave
                      (take n ks)
                      (take n vs)
                      (map #(j/invoke 'Util.hasheq %) (take n ks))))
                  ");")))))
        (apply str))

      (transient-map max-cardinality)

      (->> (range (inc max-cardinality))
        (map #(persistent-map % max-cardinality))
        (apply str)))))
