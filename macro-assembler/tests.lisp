(in-package :lc3)

;; test ldb
(mov r0 9)
(mov r1 3)
(ld r2 'instruction)
(jsr 'ldb)
(st r0 'instruction)
;; test sign-extend
(mov r0 3)
(mov r1 4)
(jsr 'sign-extend)
(st r0 'negative-four)
(trap #x25)

(label instruction)
(literal #b0001101010000011)
(label negative-four)
(literal 0)

(procedure (ldb position size integer)
    ()
  (let ((scratch r3)
        (destination-mask r4)
        (source-mask r5)
        (result r6))
    ;; Load initial masks.
    (lea source-mask 'shift-table)
    (add source-mask source-mask position)
    (ldr source-mask source-mask 0)
    (mov destination-mask 1)
    (mov result 0)
    ;; Prime the status register.
    (mov size size)
    (while :positive
      ;; If bit is set, set the bit on the result.
      (and scratch integer source-mask)
      (polarity-case
        ((:negative :positive)
         (add result result destination-mask)))
      ;; Keep going until size = 0
      (double source-mask)
      (double destination-mask)
      (decrement size))
    (mov r0 result)
    (return)))

(procedure (sign-extend bits integer)
    ()
  (let ((full r3)
        (half r4))
    ;; Load the FULL word, i.e. 2^bits
    (lea half 'shift-table)
    (add half half bits)
    (ldr full half 0)
    ;; Load the HALF word, i.e. 2^(bits - 1)
    (decrement half)
    (ldr half half 0)
    ;; If the HALF bit is set, then wrap around.
    (and half half integer)
    (polarity-case
      ((:negative :positive)
       ;; Compute INTEGER - FULL to get the wrapped around integer.
       ;; -A = not(A) + 1
       (not full full)
       (increment full)
       (add r0 integer full))
      ((:zero)
       (mov r0 integer)))
    (return)))
  
(label shift-table)
(loop for n below 16
      do (literal (ash 1 n)))
