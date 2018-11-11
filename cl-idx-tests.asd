(asdf:defsystem #:cl-idx-tests
  :description "cl-idx unit tests"
  :author "Guillaume MICHEL"
  :mailto "guillaume.michel@orilla.fr"
  :license "MIT license (see COPYING)"
  :depends-on (#:cl-idx
               #:fiveam)
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))
