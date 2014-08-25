(ns cambrian-collections.core
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

(defn hash-body [fields]
  (apply str
    (map
      (fn [f]
        (str "hash = (31 * hash) + (" f " == null ? 0 : " f ".hashCode());"))
      fields)))

(def max-vector-arity 6)

(defn fixed-arity-vector [arity]
  (let [inc-classname (str 'PersistentVector (inc arity))
        dec-classname (str 'PersistentVector (dec arity))
        classname (str 'PersistentVector arity)
        fields (mapv
                 #(str "e" %)
                 (range arity))]
    (j/class
      {:implements '[IObj IEditableCollection IReduce]
       :extends 'APersistentVector}
     classname

      ;; fields
      (->> fields
        (map #(j/field '[final] 'Object %))
        (apply str))

      (j/field '[private final] 'IPersistentMap 'meta)

      "private int hash = -1;"
      "private int hasheq = -1;"

      (when (zero? arity)
        (str "public final static " classname " EMPTY = new " classname "();"))

      (apply j/method nil nil classname
        (concat '[Object meta] (interleave (repeat 'Object) fields))
        "this.meta = meta;"
        (map
          #(str "this." % " = " % ";")
          fields))

      (apply j/method nil nil classname (interleave (repeat 'Object) fields)
        "this.meta = nil;"
        (map
          #(str "this." % " = " % ";")
          fields))

      (j/method '[public] 'IPersistentMap 'meta []
        "return meta;")

      ;; public Object nth(int i)
      (j/method '[public] 'Object 'nth '[int i]
        (if (zero? arity)
          "throw new IndexOfOutBoundsException();"
          (apply j/switch 'i
            (concat
              (interleave
                (range)
                (map #(str "return " % ";\n") fields))
              ["throw new IndexOfOutBoundsException();\n"]))))

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
        (if (== max-vector-arity arity)
          (str "return PersistentVector.EMPTY.asTransient()"
            (apply str
              (map
                (fn [f]
                  (str ".conj(" f ")"))
                (conj fields 'val)))
            ".persistent();")
          (str "return new "
            (apply j/invoke inc-classname 'meta (conj fields 'val))
            ";\n")))

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
          "hash = " (j/invoke 'Mumur3.mixCollHash 'hash arity) ";"
          "this.hasheq = hash; }")
        "return hasheq;")

      ;; public boolean equals(Object o)
      (j/method '[public] 'boolean 'equals '[Object o]
        (if (zero? arity)
          (str "return o == this ? true : super.equals(o);")
          (str "if (o instanceof " classname ") { "
            "return "
            (apply str
              (interpose '&&
                (map
                  (fn [f]
                    (j/invoke 'Util.equals f (str "((" classname ")o)." f)))
                  fields)))
            "; } else { return super.equals(o); }")))

      ;; public boolean equiv(Object o)
      (j/method '[public] 'boolean 'equiv '[Object o]
        (if (zero? arity)
          (str "return o == this ? true : super.equiv(o);")
          (str "if (o instanceof " classname ") { "
            "return "
            (if (zero? arity)
              "true"
              (apply str
                (interpose '&&
                  (map
                    (fn [f]
                      (j/invoke 'Util.equiv f (str "((" classname ")o)." f)))
                    fields))))
            "; } else { return super.equiv(o); }")))

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

      (j/method '[public] 'ISeq 'seq []
        "return IteratorSeq.create(iterator());"))))
