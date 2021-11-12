(in-package :lc3)

(comment "Machine state" :big t)

(label registers)
(words 10)

(macrolet ((frob ()
             `(progn
                ,@(loop for variable in (sort (copy-list *variables-used*) #'string>)
                        collect `(label ,variable)
                        collect `(literal 0)))))
  (frob))

