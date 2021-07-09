(defsystem "cl-trello-clone-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Rajasegar Chandran"
  :license ""
  :depends-on ("cl-trello-clone"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-trello-clone"))))
  :description "Test system for cl-trello-clone"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
