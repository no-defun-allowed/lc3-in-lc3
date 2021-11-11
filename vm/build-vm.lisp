(in-package :lc3)

(defun build-vm ()
  (dot "orig x3000")
  (jsr 'setup)
  (jsr 'interpret)
  (load "bits")
  (load "microcode")
  (load "instructions")
  (load "interpreter")
  (load "virtual-memory")
  (dot "end"))
