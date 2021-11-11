(in-package :lc3)

(comment "Interpreter loop" :big t)

(procedure (setup)
    ((r 'registers)
     (pc 'initial-program-counter))
  ;; Set the zero flag at startup. I don't think the initial state of
  ;; the machine is specified anywhere?
  (mov r0 2)
  (store-register r2 r0 *flag-register-offset*)
  (ldi r0 'pc)
  (store-register r2 r0 *program-counter-offset*)
  (return))

(procedure (interpret)
    ((h 'handler-table))
  (label again)
  (ld r3 'r)
  (add r3 r3 *program-counter-offset*)
  ;; Load and update PC
  (ldr r0 r3 0)
  (add r1 r0 2)
  (str r1 r3 0)
  ;; Fetch instruction
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
  (ldr r0 r0 0)
  (jsrr r0)
  (br :always 'again))
