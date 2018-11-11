(defsystem "cl-idx"
  :description "IDX file Format in Common Lisp (as defined in http://yann.lecun.com/exdb/mnist)"
  :version "0.0.1"
  :author "Guillaume MICHEL"
  :mailto "contact@orilla.fr"
  :homepage "http://orilla.fr"
  :license "MIT license (see COPYING)"
  :depends-on ("alexandria"
               "nibbles")
  :in-order-to ((test-op (test-op "cl-idx-tests")))
  :components ((:static-file "COPYING")
               (:static-file "README.md")
               (:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "idx-format")))))
