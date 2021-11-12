(in-package :lc3)

;;; Note that rN refers to a register of the host VM, and register-N
;;; refers something to do with a register of the guest VM.

(defvar *program-counter-offset* 8)
(defvar *flag-register-offset* 9)

(defvar *instruction-parts* (make-hash-table))
(defvar *variables-used* '())

(defmacro define-instruction-part ((name link? &rest variables) &body body)
  (let ((label-name (alexandria:format-symbol t "~a-PART" name)))
    `(progn
       (progn
         (label ,label-name)
         ,(if link?
              `(st r7 'microcode-link)
              `(progn))
         ,@body
         ,(if link?
              `(ld r7 'microcode-link)
              `(progn))
         (ret))
       (setf (gethash ',name *instruction-parts*)
             ',label-name
             *variables-used*
             (union *variables-used* ',variables)))))

(defvar *handlers* (make-array 16))

(defmacro define-instruction-handler (name instruction-number &rest parts)
  (declare (ignore name))
  (let ((name (alexandria:format-symbol t "INSTRUCTION-~d" instruction-number)))
    `(progn
       (label ,name)
       ,@(loop for part in parts
               for label = (gethash part *instruction-parts*)
               collect `(literal ',label))
       (literal 0)
       (setf (aref *handlers* ,instruction-number) ',name))))

(defun ldb-instruction (position size)
  (mov r0 position)
  (mov r1 size)
  (ld r2 'instruction)
  (jsr 'ldb))
(defun signed-ldb-instruction (position size)
  (ldb-instruction position size)
  (mov r1 r0)
  (mov r0 size)
  (jsr 'sign-extend))

(defun load-register (target constant-number)
  (lea target 'registers)
  (ldr target target constant-number))
(defun store-register (scratch value constant-number)
  (lea scratch 'registers)
  (str value scratch constant-number))
(defun load-register* (target register)
  (assert (cl:not (eql target register)))
  (lea target 'registers)
  (add target target register)
  (ldr target target 0))
(defun store-register* (scratch value register)
  (assert (cl:and (cl:not (eql scratch register))
                  (cl:not (eql value register))))
  (lea scratch 'registers)
  (add scratch scratch register)
  (str value scratch 0))

(comment "Microcode" :big t)

(label microcode-link)
(literal 0)

(define-instruction-part (:r0 t instruction register-0)
  (ldb-instruction 9 3)
  (st r0 'register-0))

(define-instruction-part (:r1 t instruction register-1)
  (ldb-instruction 6 3)
  (st r0 'register-1))

(define-instruction-part (:r2 t instruction immediate-p immediate register-2)
  (ldb-instruction 5 1)
  (st r0 'immediate-p)
  (br :zero 'no-immediate-r2)
  (signed-ldb-instruction 0 5)
  (st r0 'immediate)
  (br :always 'done-with-r2)
  (label no-immediate-r2)
  (ldb-instruction 0 3)
  (st r0 'register-2)
  (label done-with-r2))

(define-instruction-part (:effective-address t register-1 address)
  ;; load offset from instruction
  (signed-ldb-instruction 0 6)
  ;; load base from register
  (ld r2 'register-1)
  (load-register* r1 r2)
  ;; add
  (add r0 r0 r1)
  (st r0 'address))

(define-instruction-part (:pc-relative t address)
  ;; load offset from instruction
  (signed-ldb-instruction 0 9)
  ;; load PC
  (load-register r1 *program-counter-offset*)
  ;; add
  (add r0 r0 r1)
  (st r0 'address))

(define-instruction-part (:branch t address)
  ;; load mask from instruction
  (ldb-instruction 9 3)
  ;; load flags register
  (load-register r1 *flag-register-offset*)
  ;; test mask
  (and r0 r0 r1)
  (br :zero 'no-branch)
  (ld r2 'address)
  (store-register r3 r2 *program-counter-offset*)
  (label no-branch))

(macrolet ((frob (asm part)
             `(define-instruction-part (,part nil immediate-p immediate
                                              register-0 register-1 register-2)
                (ld r0 'immediate-p)
                (polarity-case
                  (:positive
                   (ld r2 'immediate))
                  ;; load "immediate" from R2
                  ((:negative :zero)
                   (ld r3 'register-2)
                   (load-register* r2 r3)))
                (ld r3 'register-1)
                (load-register* r1 r3)
                (,asm r0 r1 r2)
                (ld r3 'register-0)
                (store-register* r1 r0 r3))))
  (frob add :add)
  (frob and :and))

(define-instruction-part (:not nil register-0 register-1)
  (ld r1 'register-1)
  (load-register* r0 r1)
  (not r0 r0)
  (ld r1 'register-0)
  (store-register* r2 r0 r1))

(define-instruction-part (:jump nil register-1)
  (ld r1 'register-1)
  (load-register* r0 r1)
  (store-register r1 r0 *program-counter-offset*))

(define-instruction-part (:jsr t instruction register-1)
  ;; Set link register
  (load-register r0 *program-counter-offset*)
  (store-register r2 r0 7)
  (ld r0 'instruction)
  (mov r1 11)
  (jsr 'bit-test)
  (br :zero 'do-jsrr)
  ;; JSR
  (signed-ldb-instruction 0 11)
  (load-register r1 *program-counter-offset*)
  (add r0 r0 r1)
  (store-register r1 r0 *program-counter-offset*)
  (br :always 'finish-jsr)
  ;; JSRR
  (label do-jsrr)
  (ld r1 'register-1)
  (load-register* r0 r1)
  (store-register r2 r0 *program-counter-offset*)
  (label finish-jsr))

(define-instruction-part (:load t address)
  (ld r0 'address)
  (jsr 'read-word)
  (st r0 'address))

(define-instruction-part (:register-address nil register-0 address)
  (ld r1 'address)
  (ld r0 'register-0)
  (store-register* r2 r1 r0))

(define-instruction-part (:store t register-0 address)
  (ld r0 'register-0)
  (load-register* r1 r0)
  (ld r0 'address)
  (jsr 'write-word))

(define-instruction-part (:update-flags nil register-0)
  (ld r0 'register-0)
  (load-register* r2 r0)
  (and r1 r1 0)
  (add r2 r2 0)                         ; prime flags
  (polarity-case
    (:positive
     (add r1 r1 #b001))
    (:zero
     (add r1 r1 #b010))
    (:negative
     (add r1 r1 #b100)))
  (store-register r2 r1 *flag-register-offset*))

(define-instruction-part (:trap t instruction)
  (ld r0 'instruction)
  (and r0 r0 7)
  (lea r1 'trap-vector)
  (add r1 r1 r0)
  (ldr r1 r1 0)
  (jsrr r1))

(define-instruction-part (:crash nil)
  (trap #x25))
