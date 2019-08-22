(asdf:defsystem :paip
  :author "Tushar Tyagi"
  :version 0.0.1
  :licence "GNU GPLv3"

  :description "A stab at PAIP"

  :depends-on (:iterate)
  
  :serial t

  :components ((:module "src" :serial t
                        :components ((:file "package")
                                     (:file "chapter1")
                                     (:file "chapter2")
                                     (:file "chapter3")
                                     (:file "chapter4")
                                     (:file "chapter6")
                                     (:file "chapter7")
                                     (:file "chapter8")
                                     (:file "chapter9")
                                     (:file "chapter11")))))
