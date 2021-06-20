(ql:quickload :cl-project)

(cl-project:make-project #p"colour-colonies"
                         :author "Henry and Ed"
                         :license "MIT"
                         :depends-on '(:arrow-macros :anaphora))
