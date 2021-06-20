(defsystem "colour-colonies"
  :version "0.1.0"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("arrow-macros"
               "anaphora"
               "herodotus"
               "game-runner"
               "runtime"
               "alexandria"
               "trivia"
               "trivia.ppcre")
  :components ((:module "src"
                :components
                ((:file "colour-colonies"))))
  :description ""
  :in-order-to ((test-op (test-op "colour-colonies/tests"))))

(defsystem "colour-colonies/tests"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("colour-colonies"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "colour-colonies"))))
  :description "Test system for colour-colonies"
  :perform (test-op (op c) (symbol-call :rove :run c)))
