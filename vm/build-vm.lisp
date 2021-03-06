(in-package :lc3)

(defun build-vm (pathname target)
  (let ((*default-pathname-defaults*
          (asdf:system-source-directory :lc3-in-lc3)))
    (with-open-file (*standard-output* target :direction :output)
      (dot "orig x3000")
      (jsr 'setup)
      (jsr 'interpret)
      (load "bits")
      (load "microcode")
      (load "machine")
      (load "interpreter")
      (load "traps")
      (load "virtual-memory")
      (load "instructions")
      (load "image")
      (read-image pathname)
      (dot "end"))))
