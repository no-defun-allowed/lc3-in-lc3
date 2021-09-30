(in-package :lc3)

(defmacro with-gentemps ((&rest names) &body body)
  `(let ,(loop for name in names
               collect `(,name (gentemp ,(symbol-name name))))
     ,@body))

(defmacro polarity-case (&body cases)
  (let ((label-names (loop for nil in cases collect (gentemp "CASE")))
        (end-name (gentemp "END")))
    `(progn
       ,@(loop for (conditions . nil) in cases
               for label in label-names
               collect `(br ,conditions ',label))
       (br :always ',end-name)
       ,@(loop for (nil . body) in cases
               for label in label-names
               collect `(label ,label)
               collect `(progn ,@body)
               collect `(br :always ',end-name))
       (label ,end-name))))

(defmacro while (condition &body body)
  (with-gentemps (start)
    `(progn
       (label ,start)
       ,@body
       (br ,condition ',start))))

(defmacro procedure ((name &rest arguments) (&rest locals) &body body)
  (with-gentemps (link)
    `(progn
       (label ,name)
       (let ,(loop for argument in arguments
                   for register in *registers*
                   collect (list argument register))
         (st r7 ',link)
         (macrolet ((return ()
                      (list 'progn
                            (list 'ld 'r7 '',link)
                            (list 'ret))))
           ,@body))
       ,@(loop for (local value) in (cons (list link 0) locals)
               collect `(label ,local)
               collect `(literal ,(if (null value)
                                      0
                                      value))))))
