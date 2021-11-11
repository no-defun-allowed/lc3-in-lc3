(in-package :lc3)

(defvar *program-counter-offset* 8)
(defvar *flag-register-offset* 9)

(comment "Instructions" :big t)

(define-instruction-handler br   0 :pc-relative :branch)
(define-instruction-handler add  1 :r0 :r1 :r2 :add)
(define-instruction-handler ld   2 :r0 :pc-relative :load :register<-address)
(define-instruction-handler st   3 :r0 :pc-relative :store)
(define-instruction-handler jsr  4)      ; TODO: implement jsr
(define-instruction-handler and  5 :r0 :r1 :r2 :and)
(define-instruction-handler ldr  6 :r0 :r1 :effective-address :load :register<-address)
(define-instruction-handler str  7 :r0 :r1 :effective-address :store)
(define-instruction-handler rti  8)      ; uninteresting
(define-instruction-handler not  9 :r0 :r1 :not)
(define-instruction-handler ldi  10 :r0 :pc-relative :load :load :register<-address)
(define-instruction-handler sti  11 :r0 :pc-relative :load :store)
(define-instruction-handler jmp  12 :pc-relative :jump)
(define-instruction-handler res  13)     ; unused
(define-instruction-handler lea  14 :r0 :pc-relative :register<-address)
(define-instruction-handler trap 15)    ; TODO: implement trap

(label registers)
(words 10)
