(in-package :lc3)

(comment "Interpreter loop" :big t)

(procedure (setup)
    ((r 'registers)
     (pc 'initial-program-counter))
  ;; Write out herald
  (lea r0 'herald)
  (trap #x22)
  (mov r0 (char-code #\Newline))
  (trap #x21)
  ;; Set the zero flag at startup. I don't think the initial state of
  ;; the machine is specified anywhere?
  (mov r0 2)
  (store-register r2 r0 *flag-register-offset*)
  (ldi r0 'pc)
  (store-register r2 r0 *program-counter-offset*)
  (return))

(procedure (interpret)
    ((h 'handler-table)
     (next-part 0))
  (label again)
  (ld r3 'r)
  (add r3 r3 *program-counter-offset*)
  ;; Load and update PC
  (ldr r0 r3 0)
  (add r1 r0 1)
  (str r1 r3 0)
  ;; Fetch instruction
  (jsr 'read-word)
  (st r0 'instruction)
  ;; Get instruction number
  (mov r2 r0)
  (mov r0 12)
  (mov r1 4)
  (jsr 'ldb)
  ;; Run stream for handler
  (ld r1 'h)
  (add r0 r0 r1)
  (ldr r0 r0 0)
  (st r0 'next-part)
  (label continue-reading-parts)
  (ldi r0 'next-part)
  ;; Streams finish with a zero
  (br :zero 'again)
  (jsrr r0)
  (ld r0 'next-part)
  (increment r0)
  (st r0 'next-part)
  (br :always 'continue-reading-parts))

(label herald)
(dot "stringz \"lc3-lc3 by Hayley Patton\"")
