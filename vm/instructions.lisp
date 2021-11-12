(in-package :lc3)

(comment "Instructions" :big t)

(define-instruction-handler br    0 :pc-relative :branch)
(define-instruction-handler add   1 :r0 :r1 :r2 :add :update-flags)
(define-instruction-handler ld    2 :r0 :pc-relative :load :register-address :update-flags)
(define-instruction-handler st    3 :r0 :pc-relative :store)
(define-instruction-handler jsr   4 :r0 :r1 :jsr)
(define-instruction-handler and   5 :r0 :r1 :r2 :and :update-flags)
(define-instruction-handler ldr   6 :r0 :r1 :effective-address :load :register-address :update-flags)
(define-instruction-handler str   7 :r0 :r1 :effective-address :store)
(define-instruction-handler rti   8 :crash)
(define-instruction-handler not   9 :r0 :r1 :not :update-flags)
(define-instruction-handler ldi  10 :r0 :pc-relative :load :load :register-address :update-flags)
(define-instruction-handler sti  11 :r0 :pc-relative :load :store)
(define-instruction-handler jmp  12 :pc-relative :jump)
(define-instruction-handler res  13 :crash)
(define-instruction-handler lea  14 :r0 :pc-relative :register-address :update-flags)
(define-instruction-handler trap 15 :trap)

(label handler-table)
(loop for label across *handlers*
      do (literal label))
