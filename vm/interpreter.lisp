(in-package :lc3)

(comment "Interpreter loop" :big t)

(procedure (setup)
    ((r 'registers)
     (pc 'initial-program-counter))
  ;; Set the zero flag at startup. I don't think the initial state of
  ;; the machine is specified anywhere?
  (mov r0 2)
  (store-register r2 r0 *flag-register-offset*)
  (ld r0 'pc)
  (store-register r2 r0 *program-counter-offset*))

(procedure (interpret)
    ((instruction 0)
     (h 'handler-table)) 
  (label again)
  (load-register r0 *program-counter-offset*)
  (jsr 'read-word)
  (st r0 'instruction)
  ;; Get instruction number
  (mov r2 r0)
  (mov r0 12)
  (mov r1 4)
  (jsr 'ldb)
  ;; Call handler
  (ld r1 'h)
  (add r0 r0 r1)
  (jsrr r0)
  (jmp 'again))
