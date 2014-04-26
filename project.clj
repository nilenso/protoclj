(defproject protoclj "0.1.0-SNAPSHOT"
  :description "A protobuffer compiler for clojure wrappers"
  :url "https://github.com/nilenso/protoclj"
  :license {:name "The MIT License"}
  :dependencies [[org.clojure/clojure "1.6.0"]
		 [com.google.protobuf/protobuf-java "2.5.0"]]
  :java-source-paths ["test/fixtures"]
  :test-paths ["test/fixtures", "test"])
