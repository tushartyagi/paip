(asdf:defsystem :paip
  :author "Tushar Tyagi"
  :version 0.0.1
  :licence "GNU GPLv3"

  :description "A stab at PAIP"

  :depends-on (:iterate)
  
  :serial t

  :components ((:module "src" :serial t
                        :components ((:file "package")
                                     (:file "chapter1")))))
