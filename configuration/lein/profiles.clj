{
 :user
 {
  :dependencies
  [[org.clojure/tools.namespace "1.0.0"]

   [spyscope "0.1.6"]
   [io.aviso/pretty "0.1.37"
    :exclusions [org.clojure/clojure]]]

  ;; magic
  :plugins
  [[cider/cider-nrepl "0.24.0"]
   [io.aviso/pretty "0.1.37"]]
  
  :middleware     [io.aviso.lein-pretty/inject]
  :injections     [(require 'spyscope.core)]}}
