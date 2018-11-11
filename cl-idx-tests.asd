(defsystem "cl-idx-tests"
  :description "cl-idx unit tests"
  :author "Guillaume MICHEL"
  :mailto "contact@orilla.fr"
  :license "MIT license (see COPYING)"
  :depends-on ("cl-idx"
               "fiveam")
  :perform (test-op (o s) (uiop:symbol-call :cl-idx-tests :run-tests))
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))
