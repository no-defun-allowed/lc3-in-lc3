(in-package :lc3)

(comment "Traps" :big t)

(label trap-vector)
(literal 'getc-trap)
(literal 'out-trap)
(literal 'puts-trap)
(literal 'in-trap)
(literal 'putsp-trap)
(literal 'crash-part)
(literal 'crash-part)
(literal 'crash-part)

(procedure (getc-trap)
    ()
  (trap #x20)
  (store-register r2 r0 0)
  (return))

(procedure (out-trap)
    ()
  (load-register r0 0)
  (trap #x21)
  (return))

(procedure (puts-trap)
    ((next-address 0))
  ;; We have to hand code this ourselves, as a string may not be
  ;; contiguous in physical memory.
  (load-register r0 0)
  (st r0 'next-address)
  (label continue-puts)
  (jsr 'read-word)
  (mov r0 r0)
  (polarity-case
    (:zero (return)))
  (trap #x21)
  (ld r0 'next-address)
  (increment r0)
  (st r0 'next-address)
  (br :always 'continue-puts))

(procedure (in-trap)
    ()
  (trap #x23)
  (store-register r2 r0 0)
  (return))

(label bogus-byte-string)
;; Use this as a temporary byte string so that we don't have to
;; decode.
(words 2) 

(procedure (putsp-trap)
    ()
  ;; Who the hell decided this was a good idea?
  (load-register r1 0)
  (st r0 'next-address)
  (label continue-putsp)
  (jsr 'read-word)
  (polarity-case
    (:zero (return)))
  (lea r1 'bogus-byte-string)
  (str r0 r1 0)
  (mov r0 r1)
  (trap #x24)
  (ld r0 'next-address)
  (increment r0)
  (st r0 'next-address)
  (br :always 'continue-putsp))
