(in-package :lc3)

(defun format-assembler-value (value)
  (etypecase value
    (symbol
     (substitute #\_ #\- (string-downcase value)))
    (integer
     (format nil "#~d" value))))

(defmacro define-instruction (name arguments)
  `(defun ,name ,arguments
     (format t "~&~a~{ ~a~^,~}"
             ',name
             (list ,@(loop for argument in arguments
                           collect `(format-assembler-value ,argument))))))
(defmacro define-instructions (&body specs)
  `(progn
     ,@(loop for spec in specs
             collect `(define-instruction ,@spec))))

(define-instructions
  (add (destination register register-or-immediate))
  (and (destination register register-or-immediate))
  (jmp (register))
  (jsr (label))
  (jsrr (register))
  (ld  (destination label))
  (ldi (destination label))
  (ldr (destination register offset))
  (lea (destination label))
  (not (destination register))
  (ret ())
  (st  (source label))
  (sti (source label))
  (str (source register offset))
  (trap (index)))

(defun %br (conditions label)
  (let ((condition
          (if (eql conditions :always)
              ""
              (coerce
               (loop for condition in (alexandria:ensure-list conditions)
                     collect (ecase condition
                               ((:positive :p) #\p)
                               ((:zero :z) #\z)
                               ((:negative :n) #\n)))
               'string))))
    (format t "~&BR~a ~a"
            condition (format-assembler-value label))))

(defmacro br (conditions label)
  `(%br ',conditions ,label))

(macrolet ((d ()
             (loop for n below 8
                   for name = (alexandria:format-symbol t "R~d" n)
                   collect `(defconstant ,name ',name) into constants
                   collect name into names
                   finally (cl:return `(progn ,@constants (defvar *registers* ',names))))))
  (d))

(defmacro label (name)
  `(format t "~&~a " (format-assembler-value ',name)))
(defun literal (value)
  (format t "~&.fill ~a" (format-assembler-value value)))
(defmacro words (n)
  `(format t "~&.blkw ~d" ,n))

(defun mov (destination register-or-immediate)
  (if (integerp register-or-immediate)
      (progn
        (and destination destination 0)
        (add destination destination register-or-immediate))
      (add destination register-or-immediate 0)))

(defun double (register)
  (add register register register))
(defun increment (register)
  (add register register 1))
(defun decrement (register)
  (add register register -1))
