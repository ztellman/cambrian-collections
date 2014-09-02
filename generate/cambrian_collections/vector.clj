(ns cambrian-collections.vector
  (:require
    [cambrian-collections.java :as j]))

(defn reduce-body [reduce-ops]
  (apply str
    (map
      (fn [op]
        (str
          "init = " op ";"
          "if (RT.isReduced(init)) { return ((IDeref)init).deref(); }"))
      reduce-ops)))

(def class-prelude
  (->>
    ["package clojure.lang"
     "import java.util.Iterator"]
    (map #(str % ";\n"))
    (apply str)))

;;;

(defn transient-vector [max-cardinality]
  (let [fields (mapv
                 #(str "e" %)
                 (range max-cardinality))]
    (j/class
      {:modifiers '[static]
       :implements '[ITransientVector Counted]
       :extends 'AFn}
      'Transient

      (->> fields
        (map #(j/field nil 'Object %))
        (apply str))

      "private int count;"
      "private transient boolean edit = true;"

      (->> (range (inc max-cardinality))
        (map
          (fn [n]
            (let [fields (take n fields)]
              (apply j/method nil nil 'Transient
                (mapcat #(list 'Object %) fields)
                "this.count = " n ";"
                (map
                  #(str "this." % " = " % ";")
                  fields)))))
        (apply str))

      ;; private void ensureEditable()
      (j/method '[private] 'void 'ensureEditable []
        (j/cond "!edit"
          "throw new IllegalAccessError(\"Transient used after persistent! call\");"))

      ;; public int count()
      (j/method '[public] 'int 'count []
        "ensureEditable();"
        "return count;")

      ;; public ITransientVector conj(Object val)
      (j/method '[public] 'ITransientVector 'conj '[Object val]
        "ensureEditable();"
        (j/cond (str "count == " max-cardinality)

          (str
            "ITransientCollection coll = PersistentVector.EMPTY.asTransient();\n"
            "return (ITransientVector) coll"
            (apply str
              (map
                (fn [f]
                  (str ".conj(" f ")"))
                fields))
            ".conj(val);"))
        (apply j/switch "++count"
          (apply concat
            (map
              (fn [idx field]
                [idx (str field " = val; break;")])
              (drop 1 (range))
              fields)))
        "return this;")

      ;; public Object valAt(Object key, Object notFound)
      (j/method '[public] 'Object 'valAt '[Object key Object notFound]
        (j/cond "Util.isInteger(key)"
          (j/return (j/invoke 'nth "((Number) key).intValue()" 'notFound)))
        (j/return 'notFound))

      ;; public Object valAt(Object key)
      (j/method '[public] 'Object 'valAt '[Object key]
        (j/cond "Util.isInteger(key)"
          (j/return (j/invoke 'nth "((Number) key).intValue()")))
        "throw new IllegalArgumentException(\"Key must be integer\");")

      ;; public invoke(Object key)
      (j/method '[public] 'Object 'invoke '[Object key]
        (j/return (j/invoke 'valAt 'key)))

      ;; public Object nth(int idx)
      (j/method '[public] 'Object 'nth '[int idx]
        (j/cond "idx >= 0 && idx < count"
          (apply j/switch 'idx
            (apply concat
              (map
                (fn [idx field]
                  [idx (j/return field)])
                (range)
                fields))))
        "throw new IndexOutOfBoundsException();")

      ;; public nth(int idx, Object notFound)
      (j/method '[public] 'Object 'nth '[int idx, Object notFound]
        (j/cond "idx >= 0 && idx < count"
          "return nth(idx);")
        "return notFound;")

      ;; public ITransientVector assoc(Object key, Object val)
      (j/method '[public] 'ITransientVector 'assoc '[Object key, Object val]
        (j/cond "Util.isInteger(key)"
          (j/return (j/invoke 'assocN "((Number) key).intValue()" 'val)))
        "throw new IllegalArgumentException(\"Key must be integer\");")

      ;; public ITransientVector assocN(int idx, Object val)
      (j/method '[public] 'ITransientVector 'assocN '[int idx Object val]
        "ensureEditable();"
        (j/cond

          "idx >= 0 && idx < count"
          (str
            (apply j/switch "idx"
              (apply concat
                (map
                  (fn [idx field]
                    [idx (str field " = val; break;")])
                  (range)
                  fields)))
            "return this;")

          "idx == count"
          "return conj(val);")

        "throw new IndexOutOfBoundsException();")

      ;; public ITransientVector pop()
      (j/method '[public] 'ITransientVector 'pop []
        "ensureEditable();"
        (j/cond
          "count == 0"
          "throw new IllegalStateException(\"Can't pop empty vector\");"

          "count--;")
        "return this;")

      (j/method '[public] 'IPersistentVector 'persistent []
        "ensureEditable();"
        "edit = false;"
        (str
          (apply j/switch 'count
            (apply concat
              (map
                (fn [idx]
                  [idx
                   (if (zero? idx)
                     "return EMPTY;"
                     (str "return new Card" idx "("
                       (apply str
                         (interpose "," (take idx fields)))
                       ");\n"))])
                (range (inc max-cardinality))))))
        "throw new IllegalStateException();"))))

;;;

(defn persistent-vector [cardinality max-cardinality]
  (let [inc-classname (str 'Card (inc cardinality))
        dec-classname (str 'Card (dec cardinality))
        classname (str 'Card cardinality)
        fields (mapv
                 #(str "e" %)
                 (range cardinality))]
    (j/class
      {:modifiers '[public static]
       :implements (concat
                     '[IObj IEditableCollection IReduce]
                     (when (= 2 cardinality)
                       '[IMapEntry]))
       :extends 'APersistentVector}
     classname

      ;; fields
      (->> fields
        (map #(j/field '[final] 'Object %))
        (apply str))

      (j/field '[private final] 'IPersistentMap 'meta)

      (when (= 2 cardinality)
        (str

          (j/method '[public] 'Object 'key []
            (j/return (first fields)))

          (j/method '[public] 'Object 'getKey []
            (j/return (first fields)))

          (j/method '[public] 'Object 'val []
            (j/return (second fields)))

          (j/method '[public] 'Object 'getValue []
            (j/return (second fields)))

          (j/method '[public] 'Object 'setValue '[Object v]
            "throw new UnsupportedOperationException();")))

      ;; public CardN(IPersistentMap meta, ...)
      (apply j/method nil nil classname
        (concat '[IPersistentMap meta] (interleave (repeat 'Object) fields))
        "this.meta = meta;"
        (map
          #(str "this." % " = " % ";")
          fields))

      ;; public CardN(...)
      (apply j/method '[public] nil classname (interleave (repeat 'Object) fields)
        "this.meta = null;"
        (map
          #(str "this." % " = " % ";")
          fields))

      "\n\n// only for use with *print-dup*, assumes correct cardinality\n"
      (j/method '[public static] classname 'create '[IPersistentVector v]
        (j/cond (str "v.count() != " cardinality)
          "throw new IllegalArgumentException(\"Incorrect cardinality in create method\");")
        (j/return "new "
          (apply j/invoke classname
            (map
              (fn [n]
                (j/invoke 'v.nth n))
              (range cardinality)))))

      ;; public IPersistentMap meta()
      (j/method '[public] 'IPersistentMap 'meta []
        "return meta;")

      ;; public IObj withMeta(IPersistentMap meta)
      (j/method '[public] 'IObj 'withMeta '[IPersistentMap meta]
        "return new " (apply j/invoke classname 'meta fields) ";")

      ;; public Object nth(int i)
      (j/method '[public] 'Object 'nth '[int i]
        (if (zero? cardinality)
          "throw new IndexOutOfBoundsException();"
          (apply j/switch 'i
            (concat
              (interleave
                (range)
                (map #(str "return " % ";\n") fields))
              ["throw new IndexOutOfBoundsException();\n"]))))

      ;; public Object nth(int i, Object notFound
      (j/method '[public] 'Object 'nth '[int i, Object notFound]
        (if (zero? cardinality)
          "return notFound;"
          (apply j/switch 'i
            (concat
              (interleave
                (range)
                (map #(str "return " % ";\n") fields))
              ["return notFound;\n"]))))

      ;; public int count()
      (j/method '[public] 'int 'count []
        (str "return " cardinality ";"))

      ;; public IPersistentVector empty()
      (j/method '[public] 'IPersistentVector 'empty []
        "return EMPTY;")

      ;; public IPersistentVector assocN(int i, Object val)
      (j/method '[public] 'IPersistentVector 'assocN '[int i, Object val]
        (apply j/switch 'i
          (concat

            (interleave
              (range cardinality)
              (map
                #(str "return new "
                   (apply j/invoke classname 'meta (assoc fields % 'val))
                   ";\n")
                (range cardinality)))

            [cardinality "return cons(val);"]

            ["throw new IndexOutOfBoundsException();\n"])))

      ;; public IPersistentVector cons(Object val)
      (j/method '[public] 'IPersistentVector 'cons '[Object val]
        (if (== max-cardinality cardinality)
          (str
            "IPersistentVector v = (IPersistentVector) asTransient().conj(val).persistent();"
            "return (IPersistentVector) ((IObj) v).withMeta(meta);")
          (str "return new "
            (apply j/invoke inc-classname 'meta (conj fields 'val))
            ";\n")))

      (j/method '[public] 'ITransientCollection 'asTransient []
        (str
          "return new Transient("
          (apply str (interpose "," fields))
          ");"))

      ;; public IPersistentVector pop()
      (j/method '[public] 'IPersistentVector 'pop []
        (if (zero? cardinality)
          "throw new IllegalStateException(\"Can't pop empty vector\");"
          (str "return new "
            (apply j/invoke dec-classname 'meta (pop fields))
            ";\n")))

      ;; public Object kvreduce(IFn f, Object init)
      (j/method '[public] 'Object 'kvreduce '[IFn f, Object init]
        (reduce-body
          (map
            (fn [idx field]
              (j/invoke 'f.invoke 'init idx field))
            (range)
            fields))
        "return init;")

      ;; public Object reduce(IFn f)
      (j/method '[public] 'Object 'reduce '[IFn f]
        (cond
          (zero? cardinality)
          "return f.invoke();"

          (= 1 cardinality)
          (str "return " (first fields) ";")

          :else
          (str
            "Object init=" (first fields) ";"
            (reduce-body
              (map #(j/invoke 'f.invoke 'init %) (rest fields)))
            "return init;")))

      ;; public Object reduce(IFn f, Object init)
      (j/method '[public] 'Object 'reduce '[IFn f, Object init]
        (reduce-body
          (map #(j/invoke 'f.invoke 'init %) fields))
        "return init;")

      ;; public int hashCode()
      (j/method '[public] 'int 'hashCode []
        (str "if (_hash == -1) {"
          "int hash = 1;"
          (apply str
            (map
              (fn [f]
                (str "hash = (31 * hash) + (" f " == null ? 0 : " f ".hashCode());"))
              fields))
          "_hash = hash; }")
        "return _hash;")

      ;; public int hasheq()
      (j/method '[public] 'int 'hasheq []
        (str "if (_hasheq == -1) {"
          "int hash = 1;"
          (apply str
            (map
              (fn [f]
                (str "hash = (31 * hash) + " (j/invoke 'Util.hasheq f) ";"))
              fields))
          "hash = " (j/invoke 'Murmur3.mixCollHash 'hash cardinality) ";"
          "_hasheq = hash; }")
        "return _hasheq;")

      ;; public boolean equals(Object o)
      (j/method '[public] 'boolean 'equals '[Object o]
        (if (zero? cardinality)
          (str "return o == this ? true : super.equals(o);")
          (j/cond
            (str "o instanceof " classname)
            (j/return
              (apply str
                (interpose '&&
                  (map
                    (fn [f]
                      (j/invoke 'Util.equals f (str "((" classname ")o)." f)))
                    fields))))
            "return super.equals(o);")))

      ;; public boolean equiv(Object o)
      (j/method '[public] 'boolean 'equiv '[Object o]
        (if (zero? cardinality)
          (str "return o == this ? true : super.equiv(o);")
          (j/cond
            (str "o instanceof " classname)
            (j/return
              (apply str
                (interpose '&&
                  (map
                    (fn [f]
                      (j/invoke 'Util.equiv f (str "((" classname ")o)." f)))
                    fields))))
            "return super.equiv(o);")))

      ;; public Object[] toArray()
      (j/method '[public] "Object[]" 'toArray []
        "return new Object[] {"
        (apply str (interpose "," fields)) "};")

      ;; public Iterator iterator()
      (j/method '[public] 'Iterator 'iterator []
        "return new Iterator() {"
        "int i = 0;"
        (j/method '[public] 'boolean 'hasNext []
          "return i < " cardinality ";")
        (j/method '[public] 'Object 'next []
          "return nth(i++);")
        (j/method '[public] 'void 'remove []
          "throw new UnsupportedOperationException();")
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
            "return new " (j/invoke 'ArrayChunk "toArray()" 'offset) ";")

          (j/method '[public] 'ISeq 'chunkedNext []
            "return null;")

          (j/method '[public] 'ISeq 'chunkedMore []
            "return PersistentList.EMPTY;")

          (j/method '[public] 'UnrolledChunkedSeq 'withMeta '[IPersistentMap meta]
            "return new " (j/invoke 'UnrolledChunkedSeq 'meta 'offset) ";")

          (j/method '[public] 'Object 'first []
            (j/return (j/invoke 'nth 'offset)))

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

(defn unrolled-vector [max-cardinality]
  (let [fields (mapv
                 #(str "e" %)
                 (range (inc max-cardinality)))]
    (j/class
      {:modifiers '[public]}
      'PersistentUnrolledVector

      "static IPersistentVector EMPTY = new Card0();"

      (->> (range (inc max-cardinality))
        (map
          (fn [n]
            (j/method '[public static] 'IPersistentVector 'create
              (->> fields (take n) (mapcat #(list 'Object %)))
              (if (zero? n)
                "return EMPTY;"
                (str
                  "return new Card" n "("
                  (apply str (interpose "," (take n fields)))
                  ");")))))
        (apply str))

      (->> (range (inc max-cardinality))
        (map #(persistent-vector % max-cardinality))
        (apply str))

      (transient-vector max-cardinality))))
