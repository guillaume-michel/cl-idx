(asdf:defsystem #:cl-idx
  :description "IDX file Format in Common Lisp (as defined in http://yann.lecun.com/exdb/mnist)"
  :version "0.0.1"
  :author "Guillaume MICHEL"
  :mailto "guillaume.michel@orilla.fr"
  :homepage "http://orilla.fr"
  :license "MIT license (see COPYING)"
  :depends-on (#:alexandria #:nibbles)
  :components ((:static-file "COPYING")
               (:static-file "README.md")
               (:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "idx-format"))))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-idx-tests"))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:cl-idx))))
  (asdf:oos 'asdf:load-op '#:cl-idx-tests)
  (funcall (intern (symbol-name '#:run-tests) (find-package '#:cl-idx-tests))))
