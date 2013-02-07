(asdf:defsystem #:message-oo
  :description "Message-passing Smalltalk-style object system"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.9.1"
  :license "BSD"
  :components
  ((:file "package")
   (:file "src" :depends-on ("package"))))