(asdf:defsystem :lc3-macro-assembler
  :depends-on (:alexandria)
  :serial t
  :components ((:file "package")
               (:file "instructions")
               (:file "control-flow")))
