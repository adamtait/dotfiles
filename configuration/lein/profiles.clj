{:user {
        :plugins [[lein-cprint "1.3.1"]
                  [lein-deps-tree "0.1.2"]
                  [lein-jdk-tools "0.1.1"]
                  [lein-marginalia "0.9.1"]
                  [lein-test-bang-bang "0.2.0"]
                  [org.clojars.strongh/lein-init-script "1.4.0"]
                  [lein-localrepo "0.5.4"]]
        :dependencies [[spyscope "0.1.6"]]
        :injections [(require 'spyscope.core)]}
 :repl {:plugins [[cider/cider-nrepl "0.21.1"]]}}
