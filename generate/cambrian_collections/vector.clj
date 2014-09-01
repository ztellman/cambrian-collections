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

(defn transient-vector [max-arity]
  (let [fields (mapv
                 #(str "e" %)
                 (range max-arity))]
    (j/class
      {:modifiers '[static]
       :implements '[ITransientVector Counted]
       :extends 'AFn}
      'Transient

      (->> fields
        (map #(j/field nil 'Object %))
        (apply str))

      "private int count;"
      "private boolean edit = true;"

      (->> (range (inc max-arity))
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
        (j/cond (str "count == " max-arity)

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
                (range (inc max-arity))))))
        "throw new IllegalStateException();"))))

;;;

(defn persistent-vector [arity max-arity]
  (let [inc-classname (str 'Card (inc arity))
        dec-classname (str 'Card (dec arity))
        classname (str 'Card arity)
        fields (mapv
                 #(str "e" %)
                 (range arity))]
    (j/class
      {:modifiers '[static]
       :implements '[IObj IEditableCollection IReduce]
       :extends 'APersistentVector}
     classname

      ;; fields
      (->> fields
        (map #(j/field '[final] 'Object %))
        (apply str))

      (j/field '[private final] 'IPersistentMap 'meta)

      "private int hash = -1;"
      "private int hasheq = -1;"

      ;; public PersistentVectorN(IPersistentMap meta, ...)
      (apply j/method nil nil classname
        (concat '[IPersistentMap meta] (interleave (repeat 'Object) fields))
        "this.meta = meta;"
        (map
          #(str "this." % " = " % ";")
          fields))

      ;; public PersistentVectorN(...)
      (apply j/method '[public] nil classname (interleave (repeat 'Object) fields)
        "this.meta = null;"
        (map
          #(str "this." % " = " % ";")
          fields))

      ;; public IPersistentMap meta()
      (j/method '[public] 'IPersistentMap 'meta []
        "return meta;")

      ;; public IObj withMeta(IPersistentMap meta)
      (j/method '[public] 'IObj 'withMeta '[IPersistentMap meta]
        "return new " (apply j/invoke classname 'meta fields) ";")

      ;; public Object nth(int i)
      (j/method '[public] 'Object 'nth '[int i]
        (if (zero? arity)
          "throw new IndexOutOfBoundsException();"
          (apply j/switch 'i
            (concat
              (interleave
                (range)
                (map #(str "return " % ";\n") fields))
              ["throw new IndexOutOfBoundsException();\n"]))))

      ;; public Object nth(int i, Object notFound
      (j/method '[public] 'Object 'nth '[int i, Object notFound]
        (if (zero? arity)
          "return notFound;"
          (apply j/switch 'i
            (concat
              (interleave
                (range)
                (map #(str "return " % ";\n") fields))
              ["return notFound;\n"]))))

      ;; public int count()
      (j/method '[public] 'int 'count []
        (str "return " arity ";"))

      ;; public IPersistentVector empty()
      (j/method '[public] 'IPersistentVector 'empty []
        "return EMPTY;")

      ;; public IPersistentVector assocN(int i, Object val)
      (j/method '[public] 'IPersistentVector 'assocN '[int i, Object val]
        (apply j/switch 'i
          (concat

            (interleave
              (range arity)
              (map
                #(str "return new "
                   (apply j/invoke classname 'meta (assoc fields % 'val))
                   ";\n")
                (range arity)))

            [arity "return cons(val);"]

            ["throw new IndexOutOfBoundsException();\n"])))

      ;; public IPersistentVector cons(Object val)
      (j/method '[public] 'IPersistentVector 'cons '[Object val]
        (if (== max-arity arity)
          "return (IPersistentVector) asTransient().conj(val).persistent();"
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
        (if (zero? arity)
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
          (zero? arity)
          "return f.invoke();"

          (= 1 arity)
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
        (str "if (this.hash == -1) {"
          "int hash = 1;"
          (apply str
            (map
              (fn [f]
                (str "hash = (31 * hash) + (" f " == null ? 0 : " f ".hashCode());"))
              fields))
          "this.hash = hash; }")
        "return hash;")

      ;; public int hasheq()
      (j/method '[public] 'int 'hasheq []
        (str "if (this.hasheq == -1) {"
          "int hash = 1;"
          (apply str
            (map
              (fn [f]
                (str "hash = (31 * hash) + " (j/invoke 'Util.hasheq f) ";"))
              fields))
          "hash = " (j/invoke 'Murmur3.mixCollHash 'hash arity) ";"
          "this.hasheq = hash; }")
        "return hasheq;")

      ;; public boolean equals(Object o)
      (j/method '[public] 'boolean 'equals '[Object o]
        (if (zero? arity)
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
        (if (zero? arity)
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

      ;; public Iterator iterator()
      (j/method '[public] 'Iterator 'iterator []
        "return new Iterator() {"
        "int i = 0;"
        (j/method '[public] 'boolean 'hasNext []
          "return i < " arity ";")
        (j/method '[public] 'Object 'next []
          "return nth(i++);")
        (j/method '[public] 'void 'remove []
          "throw new UnsupportedOperationException();")
        "};")

      ;; public ISeq seq()
      (j/method '[public] 'ISeq 'seq []
        "return IteratorSeq.create(iterator());"))))

;;;

(defn unrolled-vector [max-arity]
  (let [fields (mapv
                 #(str "e" %)
                 (range (inc max-arity)))]
    (j/class
      {:modifiers '[public]}
      'PersistentUnrolledVector

      "static IPersistentVector EMPTY = new Card0();"

      (->> (range (inc max-arity))
        (map
          (fn [n]
            (j/method '[public static] 'IPersistentVector 'create
              (->> fields (take n) (mapcat #(list 'Object %)))
              (if (zero? n)
                "return PersistentUnrolledVector.EMPTY;"
                (str
                  "return new Card" n "("
                  (apply str (interpose "," (take n fields)))
                  ");")))))
        (apply str))

      (->> (range (inc max-arity))
        (map #(persistent-vector % max-arity))
        (apply str))

      (transient-vector max-arity))))
