(ns cambrian-collections.java
  (:refer-clojure
    :exclude [class import cond])
  (:import
    [org.eclipse.jdt.core.formatter
     CodeFormatter]
    [org.eclipse.jdt.internal.formatter
     DefaultCodeFormatter]
    [org.eclipse.jface.text
     Document]))

(def code "public class geo{public static void main(String[]     args){System.out.println(\"geo\");}}")

(defn format-java [s]
  (let [f  (DefaultCodeFormatter.)
        te (.format f CodeFormatter/K_UNKNOWN s 0 (count s) 0 nil)
        d  (Document. s)]
    (.apply te d)
    (.get d)))

(defn class [{:keys [modifiers implements extends]} name & body]
  (str
    (->> modifiers (interpose " ") (apply str))
    " class " name
    (when extends
      (str " extends " extends))
    (when implements
      (str " implements " (->> implements (interpose ", ") (apply str))))
    "{ "
    (apply str body)
    " }"))

(defn method [modifiers return-type name args & body]
  (str
    (->> modifiers (interpose " ") (apply str))
    " "
    return-type
    " "
    name
    "("
    (->> args
      (partition 2)
      (map (fn [[a b]] (str a " " b)))
      (interpose ", ")
      (apply str))
    ") {"
    (apply str body)
    "}"))

(defn field [modifiers type name]
  (str (->> modifiers (interpose " ") (apply str)) " " type " " name ";"))

(defn package [name]
  (str "package " name ";\n"))

(defn import [& classes]
  (->> classes
    (map #(str "import " % ";\n"))
    (apply str)))

(defn switch [value & cases]
  (str "switch (" value ") {"
    (->> cases
      (partition-all 2)
      (map (fn [[a b]]
             (if (nil? b)
               (str "default: " a)
               (str "case " a ": " b))))
      (apply str))
    " }"))

(defn invoke [method & args]
  (str method "(" (apply str (interpose "," args)) ")"))

(defn cond [& clauses]
  (loop [clauses (partition-all 2 clauses), s ""]
    (if (empty? clauses)
      s
      (let [[pred statement] (first clauses)]
        (recur
          (rest clauses)
          (str s
            (when-not (empty? s) " else ")
            (if statement
              (str "if (" pred ") { " statement " } ")
              (str "{ " pred " }"))))))))

(defn return [expr]
  (apply str "return " expr ";"))
