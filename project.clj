(defproject cambrian-collections "0.1.0-SNAPSHOT"
  :description "a veritable explosion of data structures"
  :license {:name "MIT License"}
  :source-paths ["generate"]
  :java-source-paths ["collections"]
  :dependencies
  [[collection-check "0.1.5-SNAPSHOT"]
   [criterium "0.4.3"]
   [org.clojure/clojure "1.6.0"]
   [org.eclipse.tycho/org.eclipse.jdt.core "3.10.0.v20140604-1726"]
   [org.eclipse.core/runtime "3.9.100-v20131218-1515"
    :exclusions [org.eclipse.equinox/app]]
   [org.jibx.config.3rdparty.org.eclipse/org.eclipse.text "3.5.100.v20110505-0800"]
   [org.jibx.config.3rdparty.org.eclipse/org.eclipse.core.resources "3.7.100.v20110510-0712"]
   [org.jibx.config.3rdparty.org.eclipse/org.eclipse.equinox.common "3.6.0.v20110523"]]
  :jvm-opts ["-server" "-Xmx2g"]
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark}
  :main cambrian-collections.main
  )
