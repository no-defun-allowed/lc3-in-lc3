(asdf:defsystem :lc3-macro-assembler
  :depends-on (:alexandria :nibbles)
  :serial t
  :components ((:file "package")
               (:file "instructions")
               (:file "control-flow")))
